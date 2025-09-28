/**
 * Python Language Code Base Generator
 * Generates Python code base/boilerplate code
 */

/**
 * Generates Python code base
 * @returns {string} Python code base template
 */
function generatePythonCodeBase() {
    return `\ndef main():\n    """Main function - entry point of the program."""\n    print("Hello, World!")\n    print("This is a basic Python script.")\n\n\nif __name__ == "__main__":\n    main()\n`;
}

module.exports = {
    generatePythonCodeBase
};