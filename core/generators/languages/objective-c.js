/**
 * Objective-C Code Base Generator
 * Generates boilerplate code for Objective-C projects
 */

function generateObjectiveCCodeBase() {
    return `\n#import <Foundation/Foundation.h>\n\n/**\n * Main function - entry point of the program\n */\nint main(int argc, const char * argv[]) {\n    @autoreleasepool {\n        NSLog(@"Hello, World!");\n        NSLog(@"This is a basic Objective-C program.");\n        \n        // Example with NSString\n        NSString *message = @"Welcome to TSI!";\n        NSLog(@"%@", message);\n        \n        // Example with NSArray\n        NSArray *languages = @[@"C", @"Objective-C", @"Swift"];\n        for (NSString *lang in languages) {\n            NSLog(@"Language: %@", lang);\n        }\n    }\n    return 0;\n}\n\n// Example class (uncomment to use)\n/*\n@interface TSIStudent : NSObject\n@property (nonatomic, strong) NSString *name;\n@property (nonatomic, strong) NSString *email;\n- (void)introduce;\n@end\n\n@implementation TSIStudent\n- (void)introduce {\n    NSLog(@"Hello, I'm %@ from TSI!", self.name);\n}\n@end\n*/\n`;
}

module.exports = {
    generateObjectiveCCodeBase
};