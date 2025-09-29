/**
 * C++ Language Code Base Generator
 * Generates C++ code base/boilerplate code
 */

/**
 * Generates C++ code base
 * @returns {string} C++ code base template
 */
function generateCppCodeBase() {
    return `
#include <iostream>
#include <string>
#include <memory>
#include "BaseClass.hpp"

/**
 * Main function - Entry point of the C++ program
 *
 * This function demonstrates:
 * - Basic C++ output
 * - Object creation and usage
 * - Memory management with smart pointers
 * - BaseClass functionality
 */
int main(int argc, char* argv[]) {
    std::cout << "=== TSI C++ Project Demo ===" << std::endl;
    std::cout << "Hello, World!" << std::endl;
    std::cout << "This is a professional C++ program." << std::endl;

    // Demonstrate BaseClass usage
    std::cout << "\\n--- BaseClass Demonstration ---" << std::endl;

    // Create BaseClass objects
    BaseClass obj1;
    BaseClass obj2("TestObject");
    BaseClass obj3("CustomObject", 100);

    // Display object information
    std::cout << "Object 1: " << obj1 << std::endl;
    std::cout << "Object 2: " << obj2 << std::endl;
    std::cout << "Object 3: " << obj3 << std::endl;

    // Demonstrate object methods
    std::cout << "\\n--- Object Methods ---" << std::endl;
    obj1.display();

    std::cout << "\\nIs obj1 valid? " << (obj1.isValid() ? "Yes" : "No") << std::endl;
    std::cout << "Next available ID: " << BaseClass::getNextId() << std::endl;

    // Smart pointer demonstration
    std::cout << "\\n--- Smart Pointer Usage ---" << std::endl;
    auto smartObj = std::make_unique<BaseClass>("SmartPointerObject");
    std::cout << "Smart pointer object: " << *smartObj << std::endl;

    std::cout << "\\n=== Program completed successfully! ===" << std::endl;
    return 0;
}
`;
}

/**
 * Generates C++ header code base
 * @param {string} fileName - The header file name for include guard
 * @returns {string} C++ header code base template
 */
function generateCppHeaderCodeBase(fileName) {
    // Extract base name from full path for include guard (remove path and .hpp/.h extension)
    const baseFileName = fileName.split('/').pop();
    const baseName = baseFileName.replace(/\.(hpp|h)$/i, '').toUpperCase().replace(/[^A-Z0-9_]/g, '_');
    const includeGuard = `${baseName}_HPP`;

    const headerContent = `\n#ifndef ${includeGuard}\n#define ${includeGuard}\n\n` +
        `// Include standard libraries\n#include <iostream>\n#include <string>\n#include <vector>\n#include <memory>\n\n` +
        `// Namespace declaration\n// namespace TSI {\n//     // Your code here\n// }\n\n` +
        `// Class declarations\n// class ExampleClass {\n// public:\n//     ExampleClass();\n//     ~ExampleClass();\n//     \n//     void exampleMethod();\n//     int getValue() const;\n//     \n// private:\n//     int value_;\n//     std::string name_;\n// };\n\n` +
        `// Function declarations\n// void exampleFunction(int param);\n// int calculateSum(int a, int b);\n// std::string processData(const std::vector<int>& data);\n\n` +
        `// Template declarations\n// template<typename T>\n// T findMax(const std::vector<T>& values);\n\n` +
        `// Constant definitions\n// const int MAX_SIZE = 100;\n// constexpr double PI = 3.14159;\n\n#endif // ${includeGuard}\n`;
    return headerContent;
}

module.exports = {
    generateCppCodeBase,
    generateCppHeaderCodeBase
};