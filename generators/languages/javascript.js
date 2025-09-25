/**
 * JavaScript Language Code Base Generator
 * Generates JavaScript code base/boilerplate code
 */

/**
 * Generates JavaScript code base
 * @returns {string} JavaScript code base template
 */
function generateJavaScriptCodeBase() {
    return `\n/**\n * Main function - entry point of the program\n */\nfunction main() {\n    console.log('Hello, World!');\n    console.log('This is a basic JavaScript script.');\n}\n\n// Execute main function\nmain();\n\n// Export for use as module\nmodule.exports = { main };\n`;
}

module.exports = {
    generateJavaScriptCodeBase
};