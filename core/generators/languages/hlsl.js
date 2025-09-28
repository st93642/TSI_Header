/**
 * Generates HLSL code base
 */
function generateHLSLCodeBase() {
    return `
// HLSL (High Level Shading Language) - DirectX Shader Code
// Microsoft's shading language for GPU programming in DirectX

// Include necessary headers for DirectX
#include "Common.hlsl"

// Constant buffers for shader parameters
cbuffer PerFrameConstants : register(b0)
{
    float4x4 g_ViewMatrix;
    float4x4 g_ProjectionMatrix;
    float4x4 g_ViewProjectionMatrix;
    float4x4 g_InverseViewMatrix;
    float4x4 g_InverseProjectionMatrix;
    float4 g_CameraPosition;
    float4 g_CameraDirection;
    float g_Time;
    float g_DeltaTime;
    float2 g_Resolution;
    float2 g_MousePosition;
};

cbuffer PerObjectConstants : register(b1)
{
    float4x4 g_WorldMatrix;
    float4x4 g_WorldViewMatrix;
    float4x4 g_WorldViewProjectionMatrix;
    float4x4 g_InverseTransposeWorldMatrix;
    float4 g_MaterialColor;
    float g_Metallic;
    float g_Roughness;
    float g_EmissiveIntensity;
};

// Texture and sampler declarations
Texture2D g_DiffuseTexture : register(t0);
Texture2D g_NormalTexture : register(t1);
Texture2D g_SpecularTexture : register(t2);
Texture2D g_EmissiveTexture : register(t3);
TextureCube g_EnvironmentTexture : register(t4);

SamplerState g_LinearSampler : register(s0);
SamplerState g_PointSampler : register(s1);
SamplerComparisonState g_ShadowSampler : register(s2);

// Vertex shader input structure
struct VSInput
{
    float3 Position : POSITION;
    float3 Normal : NORMAL;
    float3 Tangent : TANGENT;
    float2 TexCoord : TEXCOORD0;
    float4 Color : COLOR;
};

// Vertex shader output structure
struct VSOutput
{
    float4 Position : SV_Position;
    float3 WorldPosition : POSITION;
    float3 Normal : NORMAL;
    float3 Tangent : TANGENT;
    float2 TexCoord : TEXCOORD0;
    float4 Color : COLOR;
    float4 ShadowCoord : TEXCOORD1;
};

// Pixel shader output structure
struct PSOutput
{
    float4 Color : SV_Target0;
    float4 Normal : SV_Target1;
    float4 Specular : SV_Target2;
};

// Utility functions
float3 FresnelSchlick(float cosTheta, float3 F0)
{
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}

float DistributionGGX(float3 N, float3 H, float roughness)
{
    float a = roughness * roughness;
    float a2 = a * a;
    float NdotH = max(dot(N, H), 0.0);
    float NdotH2 = NdotH * NdotH;

    float num = a2;
    float denom = (NdotH2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;

    return num / denom;
}

float GeometrySchlickGGX(float NdotV, float roughness)
{
    float r = (roughness + 1.0);
    float k = (r * r) / 8.0;

    float num = NdotV;
    float denom = NdotV * (1.0 - k) + k;

    return num / denom;
}

float GeometrySmith(float3 N, float3 V, float3 L, float roughness)
{
    float NdotV = max(dot(N, V), 0.0);
    float NdotL = max(dot(N, L), 0.0);
    float ggx2 = GeometrySchlickGGX(NdotV, roughness);
    float ggx1 = GeometrySchlickGGX(NdotL, roughness);

    return ggx1 * ggx2;
}

// Physically Based Rendering (PBR) lighting function
float3 PBRLighting(float3 N, float3 V, float3 L, float3 radiance, float3 albedo,
                   float metallic, float roughness, float3 F0)
{
    float3 H = normalize(V + L);

    // Cook-Torrance BRDF
    float NDF = DistributionGGX(N, H, roughness);
    float G = GeometrySmith(N, V, L, roughness);
    float3 F = FresnelSchlick(max(dot(H, V), 0.0), F0);

    float3 kS = F;
    float3 kD = float3(1.0, 1.0, 1.0) - kS;
    kD *= 1.0 - metallic;

    float3 numerator = NDF * G * F;
    float denominator = 4.0 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0) + 0.0001;
    float3 specular = numerator / denominator;

    // Add to outgoing radiance
    float NdotL = max(dot(N, L), 0.0);
    return (kD * albedo / PI + specular) * radiance * NdotL;
}

// Vertex Shader - Basic transformation
VSOutput VSMain(VSInput input)
{
    VSOutput output;

    // Transform position to world space
    float4 worldPosition = mul(float4(input.Position, 1.0), g_WorldMatrix);
    output.WorldPosition = worldPosition.xyz;

    // Transform position to clip space
    output.Position = mul(worldPosition, g_ViewProjectionMatrix);

    // Transform normal to world space
    output.Normal = mul(input.Normal, (float3x3)g_InverseTransposeWorldMatrix);

    // Transform tangent to world space
    output.Tangent = mul(input.Tangent, (float3x3)g_WorldMatrix);

    // Pass through texture coordinates and color
    output.TexCoord = input.TexCoord;
    output.Color = input.Color;

    // Calculate shadow coordinates
    output.ShadowCoord = mul(worldPosition, g_LightViewProjectionMatrix);

    return output;
}

// Pixel Shader - Physically Based Rendering
PSOutput PSMain(VSOutput input)
{
    PSOutput output;

    // Sample textures
    float4 diffuseColor = g_DiffuseTexture.Sample(g_LinearSampler, input.TexCoord);
    float3 normalMap = g_NormalTexture.Sample(g_LinearSampler, input.TexCoord).xyz * 2.0 - 1.0;
    float4 specularColor = g_SpecularTexture.Sample(g_LinearSampler, input.TexCoord);
    float3 emissiveColor = g_EmissiveTexture.Sample(g_LinearSampler, input.TexCoord).xyz;

    // Apply material color
    diffuseColor *= g_MaterialColor;

    // Normal mapping
    float3 N = normalize(input.Normal);
    float3 T = normalize(input.Tangent);
    float3 B = cross(N, T);
    float3x3 TBN = float3x3(T, B, N);
    N = normalize(mul(normalMap, TBN));

    // View direction
    float3 V = normalize(g_CameraPosition.xyz - input.WorldPosition);

    // Lighting calculation
    float3 Lo = float3(0.0, 0.0, 0.0);

    // Directional light
    {
        float3 L = normalize(-g_DirectionalLightDirection.xyz);
        float3 radiance = g_DirectionalLightColor.xyz * g_DirectionalLightIntensity;

        float3 F0 = lerp(float3(0.04, 0.04, 0.04), diffuseColor.xyz, g_Metallic);
        Lo += PBRLighting(N, V, L, radiance, diffuseColor.xyz, g_Metallic, g_Roughness, F0);
    }

    // Point lights
    for (int i = 0; i < MAX_POINT_LIGHTS; ++i)
    {
        float3 lightPos = g_PointLights[i].Position.xyz;
        float3 L = normalize(lightPos - input.WorldPosition);
        float distance = length(lightPos - input.WorldPosition);
        float attenuation = 1.0 / (distance * distance);
        float3 radiance = g_PointLights[i].Color.xyz * g_PointLights[i].Intensity * attenuation;

        float3 F0 = lerp(float3(0.04, 0.04, 0.04), diffuseColor.xyz, g_Metallic);
        Lo += PBRLighting(N, V, L, radiance, diffuseColor.xyz, g_Metallic, g_Roughness, F0);
    }

    // Ambient lighting
    float3 ambient = float3(0.03, 0.03, 0.03) * diffuseColor.xyz;

    // Environment reflection
    float3 R = reflect(-V, N);
    float3 envColor = g_EnvironmentTexture.Sample(g_LinearSampler, R).xyz;
    float3 F = FresnelSchlick(max(dot(N, V), 0.0), float3(0.04, 0.04, 0.04));
    float3 kS = F;
    float3 kD = 1.0 - kS;
    kD *= 1.0 - g_Metallic;
    float3 specular = envColor * F;

    // Final color
    float3 color = ambient + Lo + specular + emissiveColor * g_EmissiveIntensity;

    // Tone mapping (ACES approximation)
    color = color / (color + float3(1.0, 1.0, 1.0));
    color = pow(color, float3(1.0 / 2.2, 1.0 / 2.2, 1.0 / 2.2)); // Gamma correction

    output.Color = float4(color, diffuseColor.a);
    output.Normal = float4(N * 0.5 + 0.5, 1.0); // Store normal in [0,1] range
    output.Specular = specularColor;

    return output;
}

// Compute Shader - Particle simulation
RWStructuredBuffer<Particle> g_Particles : register(u0);

[numthreads(256, 1, 1)]
void CSMain(uint3 dispatchThreadID : SV_DispatchThreadID)
{
    uint particleIndex = dispatchThreadID.x;

    if (particleIndex >= g_NumParticles)
        return;

    // Update particle physics
    Particle particle = g_Particles[particleIndex];

    // Apply forces (gravity, etc.)
    particle.Velocity += g_Gravity * g_DeltaTime;

    // Update position
    particle.Position += particle.Velocity * g_DeltaTime;

    // Boundary conditions
    if (particle.Position.y < 0.0)
    {
        particle.Position.y = 0.0;
        particle.Velocity.y *= -0.8; // Bounce with damping
    }

    // Update lifetime
    particle.LifeTime -= g_DeltaTime;
    if (particle.LifeTime <= 0.0)
    {
        // Respawn particle
        particle.Position = g_EmitterPosition;
        particle.Velocity = g_EmitterVelocity + float3(
            (g_RandomSeed.x - 0.5) * g_Spread,
            (g_RandomSeed.y - 0.5) * g_Spread,
            (g_RandomSeed.z - 0.5) * g_Spread
        );
        particle.LifeTime = g_ParticleLifeTime;
    }

    g_Particles[particleIndex] = particle;
}

// Geometry Shader - Billboard generation
[maxvertexcount(4)]
void GSMain(point VSOutput input[1], inout TriangleStream<VSOutput> outputStream)
{
    VSOutput output;

    // Calculate billboard vertices
    float3 right = float3(g_ViewMatrix._11, g_ViewMatrix._21, g_ViewMatrix._31);
    float3 up = float3(g_ViewMatrix._12, g_ViewMatrix._22, g_ViewMatrix._32);

    float halfSize = g_ParticleSize * 0.5;

    // Bottom-left
    output.Position = mul(float4(input[0].WorldPosition - right * halfSize - up * halfSize, 1.0), g_ViewProjectionMatrix);
    output.TexCoord = float2(0.0, 1.0);
    output.Color = input[0].Color;
    outputStream.Append(output);

    // Bottom-right
    output.Position = mul(float4(input[0].WorldPosition + right * halfSize - up * halfSize, 1.0), g_ViewProjectionMatrix);
    output.TexCoord = float2(1.0, 1.0);
    output.Color = input[0].Color;
    outputStream.Append(output);

    // Top-left
    output.Position = mul(float4(input[0].WorldPosition - right * halfSize + up * halfSize, 1.0), g_ViewProjectionMatrix);
    output.TexCoord = float2(0.0, 0.0);
    output.Color = input[0].Color;
    outputStream.Append(output);

    // Top-right
    output.Position = mul(float4(input[0].WorldPosition + right * halfSize + up * halfSize, 1.0), g_ViewProjectionMatrix);
    output.TexCoord = float2(1.0, 0.0);
    output.Color = input[0].Color;
    outputStream.Append(output);

    outputStream.RestartStrip();
}

// Tessellation Control Shader (Hull Shader)
struct HSInput
{
    float3 Position : POSITION;
    float3 Normal : NORMAL;
    float2 TexCoord : TEXCOORD0;
};

struct HSPatchOutput
{
    float EdgeTessFactor[3] : SV_TessFactor;
    float InsideTessFactor : SV_InsideTessFactor;
};

struct HSOutput
{
    float3 Position : POSITION;
    float3 Normal : NORMAL;
    float2 TexCoord : TEXCOORD0;
};

HSPatchOutput HSMainPatch(InputPatch<HSInput, 3> patch, uint patchID : SV_PrimitiveID)
{
    HSPatchOutput output;

    // Calculate tessellation factors based on distance and screen space size
    float3 center = (patch[0].Position + patch[1].Position + patch[2].Position) / 3.0;
    float distance = length(center - g_CameraPosition.xyz);

    float tessFactor = saturate(1.0 / (distance * 0.01));

    output.EdgeTessFactor[0] = tessFactor;
    output.EdgeTessFactor[1] = tessFactor;
    output.EdgeTessFactor[2] = tessFactor;
    output.InsideTessFactor = tessFactor;

    return output;
}

[domain("tri")]
[partitioning("fractional_odd")]
[outputtopology("triangle_cw")]
[outputcontrolpoints(3)]
[patchconstantfunc("HSMainPatch")]
HSOutput HSMain(InputPatch<HSInput, 3> patch, uint controlPointID : SV_OutputControlPointID)
{
    HSOutput output;

    output.Position = patch[controlPointID].Position;
    output.Normal = patch[controlPointID].Normal;
    output.TexCoord = patch[controlPointID].TexCoord;

    return output;
}

// Tessellation Evaluation Shader (Domain Shader)
struct DSOutput
{
    float4 Position : SV_Position;
    float3 Normal : NORMAL;
    float2 TexCoord : TEXCOORD0;
};

[domain("tri")]
DSOutput DSMain(HSPatchOutput patch, float3 barycentricCoords : SV_DomainLocation,
                const OutputPatch<HSOutput, 3> patch)
{
    DSOutput output;

    // Interpolate position
    float3 position = barycentricCoords.x * patch[0].Position +
                     barycentricCoords.y * patch[1].Position +
                     barycentricCoords.z * patch[2].Position;

    // Apply displacement mapping
    float displacement = g_DisplacementTexture.SampleLevel(g_LinearSampler, output.TexCoord, 0).r;
    position += output.Normal * displacement * g_DisplacementScale;

    output.Position = mul(float4(position, 1.0), g_WorldViewProjectionMatrix);

    // Interpolate normal and texture coordinates
    output.Normal = barycentricCoords.x * patch[0].Normal +
                   barycentricCoords.y * patch[1].Normal +
                   barycentricCoords.z * patch[2].Normal;

    output.TexCoord = barycentricCoords.x * patch[0].TexCoord +
                     barycentricCoords.y * patch[1].TexCoord +
                     barycentricCoords.z * patch[2].TexCoord;

    return output;
}

// Ray Tracing shaders (DirectX Raytracing)
struct RayPayload
{
    float3 Color;
    uint RecursionDepth;
};

struct ShadowRayPayload
{
    bool IsHit;
};

[shader("raygeneration")]
void RayGenMain()
{
    uint2 launchIndex = DispatchRaysIndex().xy;
    uint2 launchDimensions = DispatchRaysDimensions().xy;

    float2 pixelCenter = float2(launchIndex.xy) + float2(0.5, 0.5);
    float2 inUV = pixelCenter / float2(launchDimensions);

    // Generate ray
    RayDesc ray;
    ray.Origin = g_CameraPosition.xyz;
    ray.Direction = normalize(g_RayDirection.xyz);
    ray.TMin = 0.0;
    ray.TMax = 1000.0;

    RayPayload payload;
    payload.RecursionDepth = 0;

    TraceRay(g_SceneTLAS, RAY_FLAG_CULL_BACK_FACING_TRIANGLES, 0xFF, 0, 0, 0, ray, payload);

    g_OutputTexture[launchIndex] = float4(payload.Color, 1.0);
}

[shader("closesthit")]
void ClosestHitMain(inout RayPayload payload, in BuiltInTriangleIntersectionAttributes attr)
{
    // Get hit information
    float3 hitPosition = WorldRayOrigin() + WorldRayDirection() * RayTCurrent();

    // Sample material
    float3 albedo = g_DiffuseTexture.SampleLevel(g_LinearSampler, attr.barycentrics, 0).xyz;

    // Simple diffuse shading
    float3 lightDir = normalize(g_DirectionalLightDirection.xyz);
    float NdotL = max(dot(WorldRayDirection(), lightDir), 0.0);

    payload.Color = albedo * NdotL * g_DirectionalLightColor.xyz;
}

[shader("miss")]
void MissMain(inout RayPayload payload)
{
    payload.Color = g_SkyColor.xyz;
}

[shader("anyhit")]
void AnyHitMain(inout RayPayload payload, in BuiltInTriangleIntersectionAttributes attr)
{
    // Alpha testing
    float alpha = g_DiffuseTexture.SampleLevel(g_LinearSampler, attr.barycentrics, 0).a;
    if (alpha < 0.5)
    {
        IgnoreHit();
    }
}

/*
Compilation commands:
fxc /T vs_5_0 /E VSMain /Fo vertex_shader.cso vertex_shader.hlsl
fxc /T ps_5_0 /E PSMain /Fo pixel_shader.cso pixel_shader.hlsl
fxc /T cs_5_0 /E CSMain /Fo compute_shader.cso compute_shader.hlsl
fxc /T gs_5_0 /E GSMain /Fo geometry_shader.cso geometry_shader.hlsl
fxc /T hs_5_0 /E HSMain /Fo hull_shader.cso hull_shader.hlsl
fxc /T ds_5_0 /E DSMain /Fo domain_shader.cso domain_shader.hlsl

For DirectX 12:
dxc -T vs_6_0 -E VSMain -Fo vertex_shader.cso vertex_shader.hlsl
dxc -T ps_6_0 -E PSMain -Fo pixel_shader.cso pixel_shader.hlsl
dxc -T cs_6_0 -E CSMain -Fo compute_shader.cso compute_shader.hlsl
dxc -T gs_6_0 -E GSMain -Fo geometry_shader.cso geometry_shader.hlsl
dxc -T hs_6_0 -E HSMain -Fo hull_shader.cso hull_shader.hlsl
dxc -T ds_6_0 -E DSMain -Fo domain_shader.cso domain_shader.hlsl

For ray tracing (DirectX Raytracing):
dxc -T lib_6_3 -Fo raytracing.cso raytracing.hlsl
*/
`;
}

/**
 * Export the HLSL code generation function
 */
module.exports = {
    generateHLSLCodeBase
};