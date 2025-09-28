/**
 * D Language Code Base Generator
 * Generates D code base/boilerplate code
 */

/**
 * Generates D code base
 * @returns {string} D code base template
 */
function generateDCodeBase() {
    return `\nimport std.stdio;\n\nvoid main() {\n    writeln("Hello, World! This is a basic D program.");\n}\n`;
}

module.exports = {
    generateDCodeBase
};