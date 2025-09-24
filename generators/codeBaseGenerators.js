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
        case 'ruby':
            content = generateRubyCodeBase();
            break;
        case 'rust':
            content = generateRustCodeBase();
            break;
        case 'go':
            content = generateGoCodeBase();
            break;
        case 'php':
            content = generatePhpCodeBase();
            break;
        case 'swift':
            content = generateSwiftCodeBase();
            break;
        case 'dart':
            content = generateDartCodeBase();
            break;
        case 'scala':
            content = generateScalaCodeBase();
            break;
        case 'sql':
            content = generateSqlCodeBase();
            break;
        case 'html':
            content = generateHtmlCodeBase();
            break;
        case 'delphi':
            content = generateDelphiCodeBase();
            break;
        case 'pascal':
        case 'objectpascal':
            content = generateObjectPascalCodeBase();
            break;
        case 'basic':
            content = generateBasicCodeBase();
            break;
        case 'fortran':
            content = generateFortranCodeBase();
            break;
        case 'r':
            content = generateRCodeBase();
            break;
        case 'matlab':
            content = generateMatlabCodeBase();
            break;
        case 'assembly':
            content = generateAssemblyCodeBase();
            break;
        case 'cobol':
            content = generateCobolCodeBase();
            break;
        case 'prolog':
            content = generatePrologCodeBase();
            break;
        case 'makefile':
            content = generateMakefileCodeBase();
            break;
        case 'javascript':
            content = generateJavaScriptCodeBase();
            break;
        case 'kotlin':
            content = generateKotlinCodeBase();
            break;
        case 'typescript':
            content = generateTypeScriptCodeBase();
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
 * Generates Ruby code base
 */
function generateRubyCodeBase() {
    return `\n# Main function - entry point of the program\ndef main\n  puts "Hello, World!"\n  puts "This is a basic Ruby script."\nend\n\n# Execute main function\nmain\n`;
}

/**
 * Generates Rust code base
 */
function generateRustCodeBase() {
    return `\nfn main() {\n    println!("Hello, World!");\n    println!("This is a basic Rust program.");\n}\n`;
}

/**
 * Generates Go code base
 */
function generateGoCodeBase() {
    return `\npackage main\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Hello, World!")\n    fmt.Println("This is a basic Go program.")\n}\n`;
}

/**
 * Generates SQL code base
 */
function generateSqlCodeBase() {
    return `\n-- Basic SQL script template\n\n-- Create a sample table\nCREATE TABLE users (\n    id INTEGER PRIMARY KEY AUTO_INCREMENT,\n    name VARCHAR(100) NOT NULL,\n    email VARCHAR(255) UNIQUE,\n    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP\n);\n\n-- Insert sample data\nINSERT INTO users (name, email) VALUES\n    ('John Doe', 'john@example.com'),\n    ('Jane Smith', 'jane@example.com');\n\n-- Select data\nSELECT * FROM users;\n\n-- This is a basic SQL script template.\n`;
}

/**
 * Generates HTML code base
 */
function generateHtmlCodeBase() {
    return `\n<!DOCTYPE html>\n<html lang="en">\n<head>\n    <meta charset="UTF-8">\n    <meta name="viewport" content="width=device-width, initial-scale=1.0">\n    <title>TSI Header - Basic HTML Template</title>\n    <style>\n        body {\n            font-family: Arial, sans-serif;\n            margin: 40px;\n            line-height: 1.6;\n        }\n        .container {\n            max-width: 800px;\n            margin: 0 auto;\n        }\n    </style>\n</head>\n<body>\n    <div class="container">\n        <h1>Hello, World!</h1>\n        <p>This is a basic HTML template.</p>\n        <p>Welcome to the Transport and Telecommunication Institute!</p>\n    </div>\n</body>\n</html>\n`;
}

/**
 * Generates Delphi code base
 */
function generateDelphiCodeBase() {
    return `\nprogram HelloWorld;\n\n{\$APPTYPE CONSOLE}\n\nuses\n  System.SysUtils;\n\nbegin\n  Writeln('Hello, World!');\n  Writeln('This is a basic Delphi program.');\n  Readln;\nend.\n`;
}

/**
 * Generates Object Pascal code base
 */
function generateObjectPascalCodeBase() {
    return `\nprogram HelloWorld;\n\n{\$MODE OBJFPC}\n{\$APPTYPE CONSOLE}\n\nuses\n  SysUtils;\n\nbegin\n  Writeln('Hello, World!');\n  Writeln('This is a basic Object Pascal program.');\n  Readln;\nend.\n`;
}

/**
 * Generates BASIC code base
 */
function generateBasicCodeBase() {
    return `\n10 PRINT "Hello, World!"\n20 PRINT "This is a basic BASIC program."\n30 END\n`;
}

/**
 * Generates Fortran code base
 */
function generateFortranCodeBase() {
    return `\nPROGRAM HelloWorld\n    IMPLICIT NONE\n    \n    PRINT *, 'Hello, World!'\n    PRINT *, 'This is a basic Fortran program.'\n    \nEND PROGRAM HelloWorld\n`;
}

/**
 * Generates R code base
 */
function generateRCodeBase() {
    return `\n# Main script - basic R program\n\n# Print messages\ncat("Hello, World!\\n")\ncat("This is a basic R script.\\n")\n\n# This is a basic R script template.\n`;
}

/**
 * Generates MATLAB code base
 */
function generateMatlabCodeBase() {
    return `\n% Main script - basic MATLAB program\n\n% Display messages\ndisp('Hello, World!');\ndisp('This is a basic MATLAB script.');\n\n% This is a basic MATLAB script template.\n`;
}

/**
 * Generates Assembly code base
 */
function generateAssemblyCodeBase() {
    return `\n; Basic x86 Assembly program (NASM syntax for Linux)\n; Assemble with: nasm -f elf64 hello.asm\n; Link with: ld hello.o -o hello\n\nsection .data\n    hello db 'Hello, World!', 0xA\n    hello_len equ $ - hello\n    message db 'This is a basic Assembly program.', 0xA\n    message_len equ $ - message\n\nsection .text\n    global _start\n\n_start:\n    ; Write "Hello, World!" to stdout\n    mov rax, 1          ; syscall: write\n    mov rdi, 1          ; file descriptor: stdout\n    mov rsi, hello      ; message address\n    mov rdx, hello_len  ; message length\n    syscall\n\n    ; Write message to stdout\n    mov rax, 1          ; syscall: write\n    mov rdi, 1          ; file descriptor: stdout\n    mov rsi, message    ; message address\n    mov rdx, message_len ; message length\n    syscall\n\n    ; Exit program\n    mov rax, 60         ; syscall: exit\n    xor rdi, rdi        ; exit code: 0\n    syscall\n`;
}

/**
 * Generates COBOL code base
 */
function generateCobolCodeBase() {
    return `\nIDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO-WORLD.\n\nDATA DIVISION.\nWORKING-STORAGE SECTION.\n\nPROCEDURE DIVISION.\nMAIN-PROCEDURE.\n    DISPLAY "Hello, World!".\n    DISPLAY "This is a basic COBOL program.".\n    STOP RUN.\n`;
}

/**
 * Generates Prolog code base
 */
function generatePrologCodeBase() {
    return `\n% Basic Prolog program\n\n% Main predicate\nhello_world :-\n    write('Hello, World!'), nl,\n    write('This is a basic Prolog program.'), nl.\n\n% Query to run the program\n% ?- hello_world.\n`;
}

/**
 * Generates Makefile code base
 */
function generateMakefileCodeBase() {
    return `\n# Basic Makefile template\n\n# Compiler and flags\nCC = gcc\nCFLAGS = -Wall -Wextra -std=c99\n\n# Target executable\nTARGET = program\n\n# Source files\nSRCS = main.c\n\n# Object files\nOBJS = \$(SRCS:.c=.o)\n\n# Default target\nall: \$(TARGET)\n\n# Link object files to create executable\n\$(TARGET): \$(OBJS)\n\t\$(CC) \$(CFLAGS) -o \$(TARGET) \$(OBJS)\n\n# Compile source files to object files\n%.o: %.c\n\t\$(CC) \$(CFLAGS) -c $< -o $@\n\n# Clean build artifacts\nclean:\n\trm -f \$(OBJS) \$(TARGET)\n\n# Run the program\nrun: \$(TARGET)\n\t./\$(TARGET)\n\n# Phony targets\n.PHONY: all clean run\n`;
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

/**
 * Generates Swift code base
 */
function generateSwiftCodeBase() {
    return `\n// Basic Swift program\n\nprint("Hello, World!")\nprint("This is a basic Swift program.")\n\n// You can also use a main function:\n// func main() {\n//     print("Hello, World!")\n//     print("This is a basic Swift program.")\n// }\n// main()\n`;
}

/**
 * Generates Dart code base
 */
function generateDartCodeBase() {
    return `\n// Basic Dart program\n\nvoid main() {\n  print('Hello, World!');\n  print('This is a basic Dart program.');\n}\n\n// Alternative functional style:\n// void main() => print('Hello, World!');\n`;
}

/**
 * Generates Scala code base
 */
function generateScalaCodeBase() {
    return `\n// Basic Scala program\n\nobject Main extends App {\n  println("Hello, World!")\n  println("This is a basic Scala program.")\n}\n\n// Alternative with main method:\n// object Main {\n//   def main(args: Array[String]): Unit = {\n//     println("Hello, World!")\n//     println("This is a basic Scala program.")\n//   }\n// }\n`;
}

/**
 * Generates TypeScript code base
 */
function generateTypeScriptCodeBase() {
    return `\n/**\n * Main function - entry point of the program\n */\nfunction main(): void {\n    console.log('Hello, World!');\n    console.log('This is a basic TypeScript script.');\n}\n\n// Execute main function\nmain();\n\n// Export for use as module\nexport { main };\n`;
}

module.exports = {
    generateCodeBase
};