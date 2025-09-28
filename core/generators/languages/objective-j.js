/**
 * Objective-J Code Base Generator
 * Generates boilerplate Objective-J code
 */

function generateObjectiveJCodeBase() {
    return `\n// Basic Objective-J program\n// Note: Objective-J is used with Cappuccino framework\n\n@import <Foundation/Foundation.j>\n@import <AppKit/AppKit.j>\n\n@implementation HelloWorld : CPObject\n{\n    CPString message;\n}\n\n// Main function - entry point\n+ (void)main\n{\n    console.log("Hello, World!");\n    console.log("This is a basic Objective-J program.");\n    \n    var app = [[HelloWorld alloc] init];\n    [app greet:@"TSI Student"];\n}\n\n// Initialize\n- (id)init\n{\n    self = [super init];\n    if (self)\n    {\n        message = @"Hello from TSI!";\n    }\n    return self;\n}\n\n// Example method with parameter\n- (void)greet:(CPString)name\n{\n    console.log("Hello, " + name + "!");\n}\n\n// Getter for message\n- (CPString)message\n{\n    return message;\n}\n\n@end\n\n// Execute main\n[HelloWorld main];\n`;
}

module.exports = {
    generateObjectiveJCodeBase
};