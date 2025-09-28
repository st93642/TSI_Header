/**
 * TypeScript Language Code Base Generator
 * Generates TypeScript code base/boilerplate code
 */

/**
 * Generates TypeScript code base
 * @returns {string} TypeScript code base template
 */
function generateTypeScriptCodeBase() {
    return `\n/**\n * Main function - entry point of the program\n */\nfunction main(): void {\n    console.log('Hello, World!');\n    console.log('This is a basic TypeScript script.');\n}\n\n// Execute main function\nmain();\n\n// Export for use as module\nexport { main };\n`;
}

module.exports = {
    generateTypeScriptCodeBase
};