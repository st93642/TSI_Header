/**
 * PowerShell Code Base Generator
 * Generates boilerplate code for PowerShell scripts
 */

function generatePowerShellCodeBase() {
    return `\n# Basic PowerShell script\n\n# Main function - entry point of the program\nfunction Main {\n    Write-Output "Hello, World!"\n    Write-Output "This is a basic PowerShell script."\n}\n\n# Execute main function\nMain\n\n# Alternative direct approach\n# Write-Output "Hello, World!"\n# Write-Output "This is a basic PowerShell script."\n\n# Example function with parameters\n# function Greet {\n#     param(\n#         [string]$Name = "World"\n#     )\n#     return "Hello, $Name!"\n# }\n\n# Example usage\n# Write-Output (Greet -Name "TSI Student")\n\n# Example with arrays and hashtables\n# $languages = @("PowerShell", "C#", "Python")\n# $config = @{\n#     Version = "1.0"\n#     Debug = $true\n#     Author = "TSI Student"\n# }\n\n# foreach ($lang in $languages) {\n#     Write-Output "Language: $lang"\n# }\n`;
}

module.exports = {
    generatePowerShellCodeBase
};