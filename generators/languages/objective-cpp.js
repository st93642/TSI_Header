/**
 * Objective-C++ Code Base Generator
 * Generates boilerplate code for Objective-C++ projects
 */

function generateObjectiveCppCodeBase() {
    return `\n#import <Foundation/Foundation.h>\n#include <iostream>\n#include <string>\n\n/**\n * Main function - entry point of the program\n */\nint main(int argc, const char * argv[]) {\n    @autoreleasepool {\n        // Objective-C style\n        NSLog(@"Hello, World!");\n        NSLog(@"This is a basic Objective-C++ program.");\n        \n        // C++ style\n        std::cout << "C++ integration works!" << std::endl;\n        \n        // Mix of both\n        std::string cppString = "TSI Student";\n        NSString *objcString = [NSString stringWithUTF8String:cppString.c_str()];\n        NSLog(@"Mixed languages: %@", objcString);\n        \n        // C++ STL containers\n        std::vector<std::string> courses = {"Programming", "Mathematics", "Physics"};\n        for (const auto& course : courses) {\n            std::cout << "Course: " << course << std::endl;\n        }\n    }\n    return 0;\n}\n`;
}

module.exports = {
    generateObjectiveCppCodeBase
};