/**
 * Java Language Code Base Generator
 * Generates Java code base/boilerplate code
 */

/**
 * Generates Java code base
 * @returns {string} Java code base template
 */
function generateJavaCodeBase() {
    return `\npublic class Main {\n    public static void main(String[] args) {\n        System.out.println("Hello, World!");\n        System.out.println("This is a basic Java program.");\n    }\n}\n`;
}

module.exports = {
    generateJavaCodeBase
};