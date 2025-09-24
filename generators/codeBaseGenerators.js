/**
 * Code Base Generators Module
 * Contains language-specific code base generation logic
 */

/**
 * Generates code base/boilerplate code for the specified language
 * @param {string} languageId - The language identifier
 * @param {string} fileName - The file name (for extension detection in plaintext)
 * @returns {object} Result object with success and content properties
 */
function generateCodeBase(languageId, fileName) {
    let content;
    switch (languageId) {
        case 'c':
            content = generateCCodeBase();
            break;
        case 'csharp':
            content = generateCSharpCodeBase();
            break;
        case 'java':
            content = generateJavaCodeBase();
            break;
        case 'python':
            content = generatePythonCodeBase();
            break;
        case 'javascript':
            content = generateJavaScriptCodeBase();
            break;
        case 'kotlin':
            content = generateKotlinCodeBase();
            break;
        case 'php':
            content = generatePhpCodeBase();
            break;
        case 'plaintext':
            // Handle plaintext files (may be Kotlin .kt files)
            const fileExtension = fileName.split('.').pop().toLowerCase();
            if (fileExtension === 'kt') {
                content = generateKotlinCodeBase();
            } else {
                // Default to C for other plaintext files
                content = generateCCodeBase();
            }
            break;
        default:
            // Default to C for unsupported languages
            content = generateCCodeBase();
            break;
    }
    return { success: true, content: content };
}

/**
 * Generates C code base
 */
function generateCCodeBase() {
    return `\n#include <stdio.h>\n\nint main(int argc, char *argv[]) {\n    printf("Hello, World!\\n");\n    return 0;\n}\n`;
}

/**
 * Generates C# code base
 */
function generateCSharpCodeBase() {
    return `\nusing System;\n\nclass Program\n{\n    static void Main(string[] args)\n    {\n        Console.WriteLine("Hello, World!");\n        Console.WriteLine("This is a basic C# program.");\n    }\n}\n`;
}

/**
 * Generates Java code base
 */
function generateJavaCodeBase() {
    return `\npublic class Main {\n    public static void main(String[] args) {\n        System.out.println("Hello, World!");\n        System.out.println("This is a basic Java program.");\n    }\n}\n`;
}

/**
 * Generates Python code base
 */
function generatePythonCodeBase() {
    return `\ndef main():\n    """Main function - entry point of the program."""\n    print("Hello, World!")\n    print("This is a basic Python script.")\n\n\nif __name__ == "__main__":\n    main()\n`;
}

/**
 * Generates JavaScript code base
 */
function generateJavaScriptCodeBase() {
    return `\n/**\n * Main function - entry point of the program\n */\nfunction main() {\n    console.log('Hello, World!');\n    console.log('This is a basic JavaScript script.');\n}\n\n// Execute main function\nmain();\n\n// Export for use as module\nmodule.exports = { main };\n`;
}

/**
 * Generates Kotlin code base
 */
function generateKotlinCodeBase() {
    return `\n/**\n * Main function - entry point of the program\n */\nfun main(args: Array<String>) {\n    println("Hello, World!")\n    println("This is a basic Kotlin program.")\n}\n`;
}

/**
 * Generates PHP code base
 */
function generatePhpCodeBase() {
    return `\n<?php\n\n/**\n * Main function - entry point of the program\n */\nfunction main()\n{\n    echo "Hello, World!" . PHP_EOL;\n    echo "This is a basic PHP script." . PHP_EOL;\n}\n\n// Execute main function\nmain();\n`;
}

module.exports = {
    generateCodeBase
};