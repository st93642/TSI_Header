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

/**
 * Generates C header code base
 * @param {string} fileName - The header file name for include guard
 * @returns {string} C header code base template
 */
function generateCHeaderCodeBase(fileName) {
    // Extract base name from full path for include guard (remove path and .h extension)
    const baseFileName = fileName.split('/').pop();
    const baseName = baseFileName.replace(/\.h$/, '').toUpperCase().replace(/[^A-Z0-9_]/g, '_');
    const includeGuard = `${baseName}_H`;

    const headerContent = `\n#ifndef ${includeGuard}\n#define ${includeGuard}\n\n` +
        `// Include standard libraries\n#include <stdio.h>\n#include <stdlib.h>\n#include <stdbool.h>\n\n` +
        `// Type definitions\n// typedef struct {\n//     int id;\n//     char name[50];\n// } ExampleStruct;\n\n` +
        `// Function declarations\n// void example_function(int param);\n// int calculate_sum(int a, int b);\n\n` +
        `// Macro definitions\n// #define MAX_SIZE 100\n// #define DEBUG 1\n\n#endif // ${includeGuard}\n`;
    return headerContent;
}

module.exports = {
    generateCCodeBase,
    generateCHeaderCodeBase
};