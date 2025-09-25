/**
 * C Language Code Base Generator
 * Generates C code base/boilerplate code
 */

/**
 * Generates C code base
 * @returns {string} C code base template
 */
function generateCCodeBase() {
    return `\n#include <stdio.h>\n\nint main(int argc, char *argv[]) {\n    printf("Hello, World!\\n");\n    return 0;\n}\n`;
}

module.exports = {
    generateCCodeBase
};