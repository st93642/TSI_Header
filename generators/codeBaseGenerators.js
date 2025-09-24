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
        case 'cpp':
        case 'c++':
            content = generateCppCodeBase();
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
        case 'ada':
            content = generateAdaCodeBase();
            break;
        case 'apl':
            content = generateAplCodeBase();
            break;
        case 'asm':
        case 'assembly':
            content = generateAssemblyCodeBase();
            break;
        case 'awk':
            content = generateAwkCodeBase();
            break;
        case 'bat':
        case 'batch':
            content = generateBatchCodeBase();
            break;
        case 'cfml':
        case 'coldfusion':
            content = generateColdFusionCodeBase();
            break;
        case 'clojure':
            content = generateClojureCodeBase();
            break;
        case 'coffeescript':
            content = generateCoffeeScriptCodeBase();
            break;
        case 'css':
            content = generateCssCodeBase();
            break;
        case 'dockerfile':
            content = generateDockerfileCodeBase();
            break;
        case 'elixir':
            content = generateElixirCodeBase();
            break;
        case 'erlang':
            content = generateErlangCodeBase();
            break;
        case 'factor':
            content = generateFactorCodeBase();
            break;
        case 'forth':
            content = generateForthCodeBase();
            break;
        case 'fsharp':
            content = generateFSharpCodeBase();
            break;
        case 'groovy':
            content = generateGroovyCodeBase();
            break;
        case 'haskell':
            content = generateHaskellCodeBase();
            break;
        case 'idl':
            content = generateIdlCodeBase();
            break;
        case 'ini':
            content = generateIniCodeBase();
            break;
        case 'jade':
            content = generateJadeCodeBase();
            break;
        case 'javascriptreact':
            content = generateJavaScriptReactCodeBase();
            break;
        case 'julia':
            content = generateJuliaCodeBase();
            break;
        case 'latex':
            content = generateLatexCodeBase();
            break;
        case 'less':
            content = generateLessCodeBase();
            break;
        case 'lisp':
            content = generateLispCodeBase();
            break;
        case 'lua':
            content = generateLuaCodeBase();
            break;
        case 'maple':
            content = generateMapleCodeBase();
            break;
        case 'mathematica':
            content = generateMathematicaCodeBase();
            break;
        case 'mercury':
            content = generateMercuryCodeBase();
            break;
        case 'objective-c':
            content = generateObjectiveCCodeBase();
            break;
        case 'objective-cpp':
            content = generateObjectiveCppCodeBase();
            break;
        case 'ocaml':
            content = generateOcamlCodeBase();
            break;
        case 'octave':
            content = generateOctaveCodeBase();
            break;
        case 'perl':
        case 'perl6':
        case 'raku':
            content = generatePerlCodeBase();
            break;
        case 'postscript':
            content = generatePostScriptCodeBase();
            break;
        case 'powershell':
            content = generatePowerShellCodeBase();
            break;
        case 'scheme':
            content = generateSchemeCodeBase();
            break;
        case 'scss':
            content = generateScssCodeBase();
            break;
        case 'shellscript':
            content = generateShellScriptCodeBase();
            break;
        case 'smalltalk':
            content = generateSmalltalkCodeBase();
            break;
        case 'solidity':
            content = generateSolidityCodeBase();
            break;
        case 'tcl':
            content = generateTclCodeBase();
            break;
        case 'typescriptreact':
            content = generateTypeScriptReactCodeBase();
            break;
        case 'vb':
            content = generateVbCodeBase();
            break;
        case 'vhdl':
            content = generateVhdlCodeBase();
            break;
        case 'vue':
            content = generateVueCodeBase();
            break;
        case 'xml':
            content = generateXmlCodeBase();
            break;
        case 'yaml':
        case 'yml':
            content = generateYamlCodeBase();
            break;
        case 'kotlin':
            content = generateKotlinCodeBase();
            break;
        case 'typescript':
            content = generateTypeScriptCodeBase();
            break;
        case 'plaintext':
            // Handle plaintext files (may be Kotlin .kt files or Scala .scala files)
            const fileExtension = fileName.split('.').pop().toLowerCase();
            if (fileExtension === 'kt') {
                content = generateKotlinCodeBase();
            } else if (fileExtension === 'scala') {
                content = generateScalaCodeBase();
            } else {
                // Default to C for other plaintext files
                content = generateCCodeBase();
            }
            break;
        case 'xsl':
            content = generateXslCodeBase();
            break;
        case 'verse':
            content = generateVerseCodeBase();
            break;
        case 'vimscript':
            content = generateVimscriptCodeBase();
            break;
        case 'sed':
            content = generateSedCodeBase();
            break;
        case 'sas':
            content = generateSasCodeBase();
            break;
        case 'objective-j':
            content = generateObjectiveJCodeBase();
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
 * Generates C++ code base
 */
function generateCppCodeBase() {
    return `\n#include <iostream>\n#include <string>\n\nint main(int argc, char* argv[]) {\n    std::cout << "Hello, World!" << std::endl;\n    std::cout << "This is a basic C++ program." << std::endl;\n    return 0;\n}\n`;
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

/**
 * Generates Ada code base
 */
function generateAdaCodeBase() {
    return `\nwith Ada.Text_IO;\n\nprocedure Hello_World is\nbegin\n   Ada.Text_IO.Put_Line("Hello, World!");\n   Ada.Text_IO.Put_Line("This is a basic Ada program.");\nend Hello_World;\n`;
}

/**
 * Generates APL code base
 */
function generateAplCodeBase() {
    return `\n⍝ Basic APL program\n\n'Hello, World!'\n'This is a basic APL program.'\n\n⍝ APL functions are defined with ∇\n∇ result ← Hello\n  result ← 'Hello, World!'\n∇\n\nHello\n`;
}

/**
 * Generates AWK code base
 */
function generateAwkCodeBase() {
    return `\n# Basic AWK script\n\nBEGIN {\n    print "Hello, World!"\n    print "This is a basic AWK script."\n}\n\n# Process each line of input\n{\n    # Add processing logic here\n    # print $0  # Print entire line\n}\n\nEND {\n    print "AWK script completed."\n}\n`;
}

/**
 * Generates Batch code base
 */
function generateBatchCodeBase() {
    return `\n@echo off\nREM Basic batch script\n\necho Hello, World!\necho This is a basic batch script.\n\nREM Pause to see output\npause\n`;
}

/**
 * Generates ColdFusion code base
 */
function generateColdFusionCodeBase() {
    return `\n<!--- Basic ColdFusion script --->\n<cfscript>\n    // Main function - entry point\n    function main() {\n        writeOutput("Hello, World!<br>");\n        writeOutput("This is a basic ColdFusion script.<br>");\n    }\n    \n    // Execute main function\n    main();\n</cfscript>\n\n<!--- Alternative CFML tag syntax --->\n<!---\n<cfoutput>\n    Hello, World!<br>\n    This is a basic ColdFusion script.<br>\n</cfoutput>\n--->\n`;
}

/**
 * Generates Clojure code base
 */
function generateClojureCodeBase() {
    return `\n;; Basic Clojure program\n\n(defn main []\n  (println "Hello, World!")\n  (println "This is a basic Clojure program."))\n\n;; Execute main function\n(main)\n\n;; Alternative functional style\n;; (println "Hello, World!")\n;; (println "This is a basic Clojure program.")\n`;
}

/**
 * Generates CoffeeScript code base
 */
function generateCoffeeScriptCodeBase() {
    return `\n# Basic CoffeeScript program\n\n# Main function\nmain = ->\n  console.log "Hello, World!"\n  console.log "This is a basic CoffeeScript script."\n\n# Execute main function\nmain()\n\n# Alternative syntax\n# console.log "Hello, World!"\n# console.log "This is a basic CoffeeScript script."\n`;
}

/**
 * Generates CSS code base
 */
function generateCssCodeBase() {
    return `\n/* Basic CSS stylesheet template */\n\n/* Reset and base styles */\n* {\n    margin: 0;\n    padding: 0;\n    box-sizing: border-box;\n}\n\nbody {\n    font-family: Arial, sans-serif;\n    line-height: 1.6;\n    color: #333;\n    background-color: #f4f4f4;\n}\n\n/* Container */\n.container {\n    max-width: 1200px;\n    margin: 0 auto;\n    padding: 20px;\n}\n\n/* Header */\nheader {\n    background-color: #2c3e50;\n    color: white;\n    padding: 1rem 0;\n    text-align: center;\n}\n\nh1 {\n    font-size: 2rem;\n    margin-bottom: 0.5rem;\n}\n\n/* Main content */\nmain {\n    background-color: white;\n    padding: 2rem;\n    margin: 2rem 0;\n    border-radius: 5px;\n    box-shadow: 0 2px 5px rgba(0,0,0,0.1);\n}\n\n/* Utility classes */\n.text-center {\n    text-align: center;\n}\n\n.mb-1 {\n    margin-bottom: 1rem;\n}\n`;
}

/**
 * Generates Dockerfile code base
 */
function generateDockerfileCodeBase() {
    return `\n# Basic Dockerfile template\n# Use official Node.js runtime as base image\nFROM node:16-alpine\n\n# Set the working directory in container\nWORKDIR /app\n\n# Copy package.json and package-lock.json\nCOPY package*.json ./\n\n# Install dependencies\nRUN npm install\n\n# Copy application code\nCOPY . .\n\n# Expose port\nEXPOSE 3000\n\n# Define environment variable\nENV NODE_ENV=production\n\n# Add metadata\nLABEL maintainer="TSI Student <student@tsi.lv>"\nLABEL description="Basic Docker container template"\n\n# Create non-root user for security\nRUN addgroup -g 1001 -S nodejs\nRUN adduser -S nextjs -u 1001\nUSER nextjs\n\n# Command to run the application\nCMD ["npm", "start"]\n`;
}

/**
 * Generates Elixir code base
 */
function generateElixirCodeBase() {
    return `\n# Basic Elixir program\n\ndefmodule HelloWorld do\n  @moduledoc """\n  Basic Elixir module template\n  """\n\n  @doc """\n  Main function - entry point of the program\n  """\n  def main do\n    IO.puts("Hello, World!")\n    IO.puts("This is a basic Elixir program.")\n  end\n\n  @doc """\n  Example function with pattern matching\n  """\n  def greet(name) when is_binary(name) do\n    "Hello, #{name}!"\n  end\n\n  def greet(_), do: "Hello, Anonymous!"\nend\n\n# Execute main function\nHelloWorld.main()\n\n# Example usage\n# IO.puts(HelloWorld.greet("TSI Student"))\n`;
}

/**
 * Generates Erlang code base
 */
function generateErlangCodeBase() {
    return `\n%% Basic Erlang program\n-module(hello_world).\n-export([main/0, start/0]).\n\n%% Main function - entry point of the program\nmain() ->\n    io:format("Hello, World!~n"),\n    io:format("This is a basic Erlang program.~n").\n\n%% Start function for application\nstart() ->\n    main().\n\n%% To run this program:\n%% 1. Compile: erlc hello_world.erl\n%% 2. Run: erl -noshell -s hello_world start -s init stop\n`;
}

/**
 * Generates Factor code base
 */
function generateFactorCodeBase() {
    return `\n! Basic Factor program\n\nUSING: io ;\nIN: hello-world\n\n: main ( -- )\n    "Hello, World!" print\n    "This is a basic Factor program." print ;\n\n! Execute main function\nmain\n\n! Alternative approach\n! "Hello, World!" print\n! "This is a basic Factor program." print\n`;
}

/**
 * Generates Forth code base
 */
function generateForthCodeBase() {
    return `\n( Basic Forth program )\n\n: HELLO ( -- )\n    ." Hello, World!" CR\n    ." This is a basic Forth program." CR ;\n\n( Execute the word )\nHELLO\n\n( Alternative approach )\n( ." Hello, World!" CR )\n( ." This is a basic Forth program." CR )\n`;
}

/**
 * Generates F# code base
 */
function generateFSharpCodeBase() {
    return `\n(* Basic F# program *)\n\n// Main function - entry point of the program\n[<EntryPoint>]\nlet main argv =\n    printfn "Hello, World!"\n    printfn "This is a basic F# program."\n    0 // Return exit code\n\n// Alternative functional style\n// let sayHello () =\n//     "Hello, World!"\n//     |> printfn "%s"\n//     "This is a basic F# program."\n//     |> printfn "%s"\n// \n// sayHello ()\n`;
}

/**
 * Generates Groovy code base
 */
function generateGroovyCodeBase() {
    return `\n// Basic Groovy program\n\n// Main method (optional in Groovy)\ndef main(args) {\n    println "Hello, World!"\n    println "This is a basic Groovy script."\n}\n\n// Groovy can also run without main method\n// println "Hello, World!"\n// println "This is a basic Groovy script."\n\n// Call main method\nmain(args)\n\n// Example of Groovy features\ndef greet(name = "World") {\n    "Hello, ${name}!"\n}\n\n// println greet("TSI Student")\n`;
}

/**
 * Generates Haskell code base
 */
function generateHaskellCodeBase() {
    return `\n-- Basic Haskell program\n\n-- Main function - entry point of the program\nmain :: IO ()\nmain = do\n    putStrLn "Hello, World!"\n    putStrLn "This is a basic Haskell program."\n\n-- Example function with type signature\ngreet :: String -> String\ngreet name = "Hello, " ++ name ++ "!"\n\n-- Example usage (uncomment to use)\n-- main = putStrLn (greet "TSI Student")\n\n-- Alternative point-free style\n-- main = mapM_ putStrLn ["Hello, World!", "This is a basic Haskell program."]\n`;
}

/**
 * Generates IDL code base
 */
function generateIdlCodeBase() {
    return `\n;; Basic IDL program\n\n;; Main procedure\nPRO hello_world\n    PRINT, 'Hello, World!'\n    PRINT, 'This is a basic IDL program.'\nEND\n\n;; Execute main procedure\nhello_world\n\n;; Alternative direct approach\n;; PRINT, 'Hello, World!'\n;; PRINT, 'This is a basic IDL program.'\n\n;; Example with variables\n;; message1 = 'Hello, World!'\n;; message2 = 'This is a basic IDL program.'\n;; PRINT, message1\n;; PRINT, message2\n`;
}

/**
 * Generates INI code base
 */
function generateIniCodeBase() {
    return `\n; Basic INI configuration file template\n\n[Application]\nName=TSI Header Application\nVersion=1.0.0\nDescription=Basic application configuration\n\n[Database]\nHost=localhost\nPort=5432\nName=tsi_database\nUser=tsi_user\n; Password=your_password_here\n\n[Settings]\nDebug=true\nLogLevel=info\nMaxConnections=100\nTimeout=30\n\n[Paths]\nDataDir=/var/data\nLogDir=/var/log\nTempDir=/tmp\n\n; Comments can use semicolons or hash symbols\n# This is also a valid comment\n\n[Features]\nFeature1=enabled\nFeature2=disabled\nExperimentalFeatures=false\n`;
}

/**
 * Generates Jade code base
 */
function generateJadeCodeBase() {
    return `\n//- Basic Jade template\ndoctype html\nhtml(lang="en")\n  head\n    meta(charset="UTF-8")\n    meta(name="viewport", content="width=device-width, initial-scale=1.0")\n    title TSI Header - Basic Jade Template\n    style.\n      body {\n        font-family: Arial, sans-serif;\n        margin: 40px;\n        line-height: 1.6;\n      }\n      .container {\n        max-width: 800px;\n        margin: 0 auto;\n      }\n  body\n    .container\n      header\n        h1 Hello, World!\n        p This is a basic Jade template.\n      main\n        section\n          h2 Welcome to TSI\n          p Transport and Telecommunication Institute\n          ul\n            li Modern education\n            li Innovative technologies\n            li Professional development\n      footer\n        p © 2025 TSI - All rights reserved\n`;
}

/**
 * Generates JavaScript React code base
 */
function generateJavaScriptReactCodeBase() {
    return `\nimport React from 'react';\nimport ReactDOM from 'react-dom/client';\n\n/**\n * Main React component\n */\nfunction App() {\n  return (\n    <div className="app">\n      <header className="app-header">\n        <h1>Hello, World!</h1>\n        <p>This is a basic React application.</p>\n      </header>\n      <main>\n        <section>\n          <h2>Welcome to TSI</h2>\n          <p>Transport and Telecommunication Institute</p>\n          <p>Building the future of technology education.</p>\n        </section>\n      </main>\n    </div>\n  );\n}\n\n/**\n * Render the application\n */\nconst root = ReactDOM.createRoot(document.getElementById('root'));\nroot.render(<App />);\n\nexport default App;\n`;
}

/**
 * Generates Julia code base
 */
function generateJuliaCodeBase() {
    return `\n# Basic Julia program\n\n\"\"\"\nMain function - entry point of the program\n\"\"\"\nfunction main()\n    println("Hello, World!")\n    println("This is a basic Julia program.")\nend\n\n# Execute main function\nmain()\n\n# Alternative direct approach\n# println("Hello, World!")\n# println("This is a basic Julia program.")\n\n# Example function with type annotations\nfunction greet(name::String)::String\n    return "Hello, $name!"\nend\n\n# Example usage\n# println(greet("TSI Student"))\n\n# Julia supports multiple dispatch\nfunction greet(name::String, enthusiasm::Int)\n    exclamation = "!" ^ enthusiasm\n    return "Hello, $name$exclamation"\nend\n\n# println(greet("TSI Student", 3))\n`;
}

/**
 * Generates LaTeX code base
 */
function generateLatexCodeBase() {
    return `\n\\documentclass[12pt,a4paper]{article}\n\n% Basic packages\n\\usepackage[utf8]{inputenc}\n\\usepackage[T1]{fontenc}\n\\usepackage{amsmath,amsfonts,amssymb}\n\\usepackage{graphicx}\n\\usepackage[margin=2.5cm]{geometry}\n\\usepackage{hyperref}\n\n% Document information\n\\title{TSI Header - Basic LaTeX Template}\n\\author{TSI Student}\n\\date{\\today}\n\n\\begin{document}\n\n\\maketitle\n\n\\begin{abstract}\nThis is a basic LaTeX document template for Transport and Telecommunication Institute students. It includes common packages and a standard structure for academic documents.\n\\end{abstract}\n\n\\section{Introduction}\nHello, World! This is a basic LaTeX document.\n\n\\section{Main Content}\nThis template provides a foundation for creating professional academic documents.\n\n\\subsection{Features}\n\\begin{itemize}\n    \\item Professional document formatting\n    \\item Mathematical notation support\n    \\item Graphics and table support\n    \\item Bibliography support (add \\texttt{biblatex} if needed)\n\\end{itemize}\n\n\\section{Mathematics}\nHere's an example equation:\n\\begin{equation}\n    E = mc^2\n\\end{equation}\n\n\\section{Conclusion}\nThis concludes the basic LaTeX template.\n\n\\end{document}\n`;
}

/**
 * Generates Less code base
 */
function generateLessCodeBase() {
    return `\n// Basic Less stylesheet template\n\n// Variables\n@primary-color: #2c3e50;\n@secondary-color: #3498db;\n@font-size-base: 16px;\n@line-height-base: 1.6;\n@border-radius: 5px;\n@container-width: 1200px;\n\n// Mixins\n.border-radius(@radius: @border-radius) {\n  border-radius: @radius;\n  -webkit-border-radius: @radius;\n  -moz-border-radius: @radius;\n}\n\n.box-shadow(@shadow: 0 2px 5px rgba(0,0,0,0.1)) {\n  box-shadow: @shadow;\n  -webkit-box-shadow: @shadow;\n  -moz-box-shadow: @shadow;\n}\n\n.transition(@property: all, @duration: 0.3s, @easing: ease) {\n  transition: @property @duration @easing;\n  -webkit-transition: @property @duration @easing;\n  -moz-transition: @property @duration @easing;\n}\n\n// Base styles\n* {\n  margin: 0;\n  padding: 0;\n  box-sizing: border-box;\n}\n\nbody {\n  font-family: Arial, sans-serif;\n  font-size: @font-size-base;\n  line-height: @line-height-base;\n  color: #333;\n  background-color: #f4f4f4;\n}\n\n// Container\n.container {\n  max-width: @container-width;\n  margin: 0 auto;\n  padding: 20px;\n}\n\n// Header\nheader {\n  background-color: @primary-color;\n  color: white;\n  padding: 1rem 0;\n  text-align: center;\n  .border-radius(0);\n  \n  h1 {\n    font-size: 2rem;\n    margin-bottom: 0.5rem;\n  }\n}\n\n// Main content\nmain {\n  background-color: white;\n  padding: 2rem;\n  margin: 2rem 0;\n  .border-radius();\n  .box-shadow();\n  .transition();\n  \n  &:hover {\n    .box-shadow(0 4px 10px rgba(0,0,0,0.15));\n  }\n}\n\n// Buttons\n.btn {\n  display: inline-block;\n  padding: 0.5rem 1rem;\n  background-color: @secondary-color;\n  color: white;\n  text-decoration: none;\n  .border-radius();\n  .transition(background-color);\n  \n  &:hover {\n    background-color: darken(@secondary-color, 10%);\n  }\n}\n`;
}

/**
 * Generates Lisp code base
 */
function generateLispCodeBase() {
    return `\n;; Basic Lisp program\n\n;; Main function - entry point of the program\n(defun main ()\n  (format t "Hello, World!~%")\n  (format t "This is a basic Lisp program.~%"))\n\n;; Execute main function\n(main)\n\n;; Alternative direct approach\n;; (format t "Hello, World!~%")\n;; (format t "This is a basic Lisp program.~%")\n\n;; Example function definition\n(defun greet (name)\n  (format t "Hello, ~a!~%" name))\n\n;; Example usage\n;; (greet "TSI Student")\n\n;; Example of list processing\n(defun process-list (lst)\n  (if (null lst)\n      nil\n      (cons (* 2 (car lst))\n            (process-list (cdr lst)))))\n\n;; Example usage\n;; (process-list '(1 2 3 4 5))\n`;
}

/**
 * Generates Lua code base
 */
function generateLuaCodeBase() {
    return `\n-- Basic Lua program\n\n-- Main function - entry point of the program\nfunction main()\n    print("Hello, World!")\n    print("This is a basic Lua script.")\nend\n\n-- Execute main function\nmain()\n\n-- Alternative direct approach\n-- print("Hello, World!")\n-- print("This is a basic Lua script.")\n\n-- Example function with parameters\nfunction greet(name)\n    return "Hello, " .. (name or "World") .. "!"\nend\n\n-- Example usage\n-- print(greet("TSI Student"))\n\n-- Example table (Lua's main data structure)\nlocal config = {\n    version = "1.0",\n    debug = true,\n    features = {"logging", "caching"}\n}\n\n-- print("Version: " .. config.version)\n`;
}

/**
 * Generates Maple code base
 */
function generateMapleCodeBase() {
    return `\n# Basic Maple program\n\n# Main procedure\nhello_world := proc()\n    printf("Hello, World!\\n");\n    printf("This is a basic Maple program.\\n");\nend proc:\n\n# Execute main procedure\nhello_world();\n\n# Alternative direct approach\n# printf("Hello, World!\\n");\n# printf("This is a basic Maple program.\\n");\n\n# Example mathematical computation\n# f := x -> x^2 + 2*x + 1;\n# result := f(5);\n# printf("f(5) = %d\\n", result);\n\n# Example symbolic math\n# expr := x^3 - 6*x^2 + 11*x - 6;\n# factored := factor(expr);\n# printf("Factored form: %a\\n", factored);\n`;
}

/**
 * Generates Mathematica code base
 */
function generateMathematicaCodeBase() {
    return `\n(* Basic Mathematica program *)\n\n(* Main function - entry point of the program *)\nhelloWorld[] := (\n  Print["Hello, World!"];\n  Print["This is a basic Mathematica program."]\n)\n\n(* Execute main function *)\nhelloWorld[]\n\n(* Alternative direct approach *)\n(* Print["Hello, World!"] *)\n(* Print["This is a basic Mathematica program."] *)\n\n(* Example mathematical computations *)\n(* f[x_] := x^2 + 2*x + 1 *)\n(* result = f[5] *)\n(* Print["f(5) = ", result] *)\n\n(* Example symbolic math *)\n(* expr = x^3 - 6*x^2 + 11*x - 6; *)\n(* factored = Factor[expr]; *)\n(* Print["Factored form: ", factored] *)\n\n(* Example plotting (uncomment to use) *)\n(* Plot[Sin[x], {x, 0, 2*Pi}] *)\n`;
}

/**
 * Generates Mercury code base
 */
function generateMercuryCodeBase() {
    return `\n%% Basic Mercury program\n\n:- module hello_world.\n:- interface.\n\n:- import_module io.\n\n:- pred main(io::di, io::uo) is det.\n\n:- implementation.\n\nmain(!IO) :-\n    io.write_string("Hello, World!\\n", !IO),\n    io.write_string("This is a basic Mercury program.\\n", !IO).\n\n%% To compile and run:\n%% mmc --make hello_world\n%% ./hello_world\n\n%% Example predicate\n%% :- pred greet(string::in, string::out) is det.\n%% greet(Name, Greeting) :-\n%%     string.append_list(["Hello, ", Name, "!"], Greeting).\n`;
}

/**
 * Generates Objective-C code base
 */
function generateObjectiveCCodeBase() {
    return `\n#import <Foundation/Foundation.h>\n\n/**\n * Main function - entry point of the program\n */\nint main(int argc, const char * argv[]) {\n    @autoreleasepool {\n        NSLog(@"Hello, World!");\n        NSLog(@"This is a basic Objective-C program.");\n        \n        // Example with NSString\n        NSString *message = @"Welcome to TSI!";\n        NSLog(@"%@", message);\n        \n        // Example with NSArray\n        NSArray *languages = @[@"C", @"Objective-C", @"Swift"];\n        for (NSString *lang in languages) {\n            NSLog(@"Language: %@", lang);\n        }\n    }\n    return 0;\n}\n\n// Example class (uncomment to use)\n/*\n@interface TSIStudent : NSObject\n@property (nonatomic, strong) NSString *name;\n@property (nonatomic, strong) NSString *email;\n- (void)introduce;\n@end\n\n@implementation TSIStudent\n- (void)introduce {\n    NSLog(@"Hello, I'm %@ from TSI!", self.name);\n}\n@end\n*/\n`;
}

/**
 * Generates Objective-C++ code base
 */
function generateObjectiveCppCodeBase() {
    return `\n#import <Foundation/Foundation.h>\n#include <iostream>\n#include <string>\n\n/**\n * Main function - entry point of the program\n */\nint main(int argc, const char * argv[]) {\n    @autoreleasepool {\n        // Objective-C style\n        NSLog(@"Hello, World!");\n        NSLog(@"This is a basic Objective-C++ program.");\n        \n        // C++ style\n        std::cout << "C++ integration works!" << std::endl;\n        \n        // Mix of both\n        std::string cppString = "TSI Student";\n        NSString *objcString = [NSString stringWithUTF8String:cppString.c_str()];\n        NSLog(@"Mixed languages: %@", objcString);\n        \n        // C++ STL containers\n        std::vector<std::string> courses = {"Programming", "Mathematics", "Physics"};\n        for (const auto& course : courses) {\n            std::cout << "Course: " << course << std::endl;\n        }\n    }\n    return 0;\n}\n`;
}

/**
 * Generates OCaml code base
 */
function generateOcamlCodeBase() {
    return `\n(* Basic OCaml program *)\n\n(* Main function - entry point of the program *)\nlet main () =\n  print_endline "Hello, World!";\n  print_endline "This is a basic OCaml program."\n\n(* Execute main function *)\nlet () = main ()\n\n(* Alternative direct approach *)\n(* let () = *)\n(*   print_endline "Hello, World!"; *)\n(*   print_endline "This is a basic OCaml program." *)\n\n(* Example function with pattern matching *)\n(* let rec factorial n = *)\n(*   match n with *)\n(*   | 0 | 1 -> 1 *)\n(*   | n -> n * factorial (n - 1) *)\n\n(* Example usage *)\n(* let result = factorial 5 in *)\n(* Printf.printf "5! = %d\\n" result *)\n\n(* Example with lists *)\n(* let numbers = [1; 2; 3; 4; 5] in *)\n(* let doubled = List.map (fun x -> x * 2) numbers in *)\n(* List.iter (Printf.printf "%d ") doubled *)\n`;
}

/**
 * Generates Octave code base
 */
function generateOctaveCodeBase() {
    return `\n% Basic Octave program\n\n% Main function - entry point of the program\nfunction main()\n    disp('Hello, World!');\n    disp('This is a basic Octave script.');\nend\n\n% Execute main function\nmain();\n\n% Alternative direct approach\n% disp('Hello, World!');\n% disp('This is a basic Octave script.');\n\n% Example mathematical operations\n% x = [1, 2, 3, 4, 5];\n% y = x .^ 2;\n% disp('Squares:');\n% disp(y);\n\n% Example plotting (uncomment to use)\n% x = linspace(0, 2*pi, 100);\n% y = sin(x);\n% plot(x, y);\n% title('Sine Wave');\n% xlabel('x');\n% ylabel('sin(x)');\n`;
}

/**
 * Generates Perl code base
 */
function generatePerlCodeBase() {
    return `\n#!/usr/bin/env perl\n\n# Basic Perl program\n\nuse strict;\nuse warnings;\nuse v5.10;\n\n# Main subroutine - entry point of the program\nsub main {\n    say "Hello, World!";\n    say "This is a basic Perl script.";\n}\n\n# Execute main subroutine\nmain();\n\n# Alternative direct approach\n# say "Hello, World!";\n# say "This is a basic Perl script.";\n\n# Example subroutine with parameters\n# sub greet {\n#     my ($name) = @_;\n#     return "Hello, " . ($name // "World") . "!";\n# }\n\n# Example usage\n# say greet("TSI Student");\n\n# Example with arrays and hashes\n# my @languages = ("Perl", "Python", "Ruby");\n# my %config = (\n#     version => "1.0",\n#     debug   => 1,\n#     author  => "TSI Student"\n# );\n\n# foreach my $lang (@languages) {\n#     say "Language: $lang";\n# }\n`;
}

/**
 * Generates PostScript code base
 */
function generatePostScriptCodeBase() {
    return `\n%% Basic PostScript program\n\n%% Main procedure - entry point of the program\n/main {\n    (Hello, World!) show\n    newline\n    (This is a basic PostScript program.) show\n    newline\n} def\n\n%% Execute main procedure\nmain\n\n%% Alternative direct approach\n%% (Hello, World!) show\n%% newline\n%% (This is a basic PostScript program.) show\n%% newline\n\n%% Example procedures\n%% /greet {\n%%     /name exch def\n%%     (Hello, ) show\n%%     name show\n%%     (!) show\n%%     newline\n%% } def\n\n%% Example usage\n%% (TSI Student) greet\n\n%% Example drawing (uncomment to use)\n%% newpath\n%% 100 100 50 0 360 arc\n%% stroke\n%% showpage\n`;
}

/**
 * Generates PowerShell code base
 */
function generatePowerShellCodeBase() {
    return `\n# Basic PowerShell script\n\n# Main function - entry point of the program\nfunction Main {\n    Write-Output "Hello, World!"\n    Write-Output "This is a basic PowerShell script."\n}\n\n# Execute main function\nMain\n\n# Alternative direct approach\n# Write-Output "Hello, World!"\n# Write-Output "This is a basic PowerShell script."\n\n# Example function with parameters\n# function Greet {\n#     param(\n#         [string]$Name = "World"\n#     )\n#     return "Hello, $Name!"\n# }\n\n# Example usage\n# Write-Output (Greet -Name "TSI Student")\n\n# Example with arrays and hashtables\n# $languages = @("PowerShell", "C#", "Python")\n# $config = @{\n#     Version = "1.0"\n#     Debug = $true\n#     Author = "TSI Student"\n# }\n\n# foreach ($lang in $languages) {\n#     Write-Output "Language: $lang"\n# }\n`;
}

/**
 * Generates Scheme code base
 */
function generateSchemeCodeBase() {
    return `\n;; Basic Scheme program\n\n;; Main function - entry point of the program\n(define (main)\n  (display "Hello, World!")\n  (newline)\n  (display "This is a basic Scheme program.")\n  (newline))\n\n;; Execute main function\n(main)\n\n;; Alternative direct approach\n;; (display "Hello, World!")\n;; (newline)\n;; (display "This is a basic Scheme program.")\n;; (newline)\n\n;; Example function definition\n;; (define (greet name)\n;;   (string-append "Hello, " name "!"))\n\n;; Example usage\n;; (display (greet "TSI Student"))\n;; (newline)\n\n;; Example with lists\n;; (define numbers '(1 2 3 4 5))\n;; (define doubled (map (lambda (x) (* x 2)) numbers))\n;; (display "Doubled: ")\n;; (display doubled)\n;; (newline)\n`;
}

/**
 * Generates SCSS code base
 */
function generateScssCodeBase() {
    return `\n// Basic SCSS stylesheet template\n\n// Variables\n$primary-color: #2c3e50;\n$secondary-color: #3498db;\n$font-size-base: 16px;\n$line-height-base: 1.6;\n$border-radius: 5px;\n$container-width: 1200px;\n\n// Mixins\n@mixin border-radius($radius: $border-radius) {\n  border-radius: $radius;\n  -webkit-border-radius: $radius;\n  -moz-border-radius: $radius;\n}\n\n@mixin box-shadow($shadow: 0 2px 5px rgba(0,0,0,0.1)) {\n  box-shadow: $shadow;\n  -webkit-box-shadow: $shadow;\n  -moz-box-shadow: $shadow;\n}\n\n@mixin transition($property: all, $duration: 0.3s, $easing: ease) {\n  transition: $property $duration $easing;\n  -webkit-transition: $property $duration $easing;\n  -moz-transition: $property $duration $easing;\n}\n\n// Functions\n@function calculate-rem($px) {\n  @return #{$px / 16px}rem;\n}\n\n// Base styles\n* {\n  margin: 0;\n  padding: 0;\n  box-sizing: border-box;\n}\n\nbody {\n  font-family: Arial, sans-serif;\n  font-size: $font-size-base;\n  line-height: $line-height-base;\n  color: #333;\n  background-color: #f4f4f4;\n}\n\n// Container\n.container {\n  max-width: $container-width;\n  margin: 0 auto;\n  padding: 20px;\n}\n\n// Header\nheader {\n  background-color: $primary-color;\n  color: white;\n  padding: 1rem 0;\n  text-align: center;\n  @include border-radius(0);\n  \n  h1 {\n    font-size: calculate-rem(32px);\n    margin-bottom: 0.5rem;\n  }\n}\n\n// Main content\nmain {\n  background-color: white;\n  padding: 2rem;\n  margin: 2rem 0;\n  @include border-radius();\n  @include box-shadow();\n  @include transition();\n  \n  &:hover {\n    @include box-shadow(0 4px 10px rgba(0,0,0,0.15));\n  }\n}\n\n// Buttons\n.btn {\n  display: inline-block;\n  padding: 0.5rem 1rem;\n  background-color: $secondary-color;\n  color: white;\n  text-decoration: none;\n  @include border-radius();\n  @include transition(background-color);\n  \n  &:hover {\n    background-color: darken($secondary-color, 10%);\n  }\n  \n  &--large {\n    padding: 1rem 2rem;\n    font-size: calculate-rem(18px);\n  }\n}\n`;
}

/**
 * Generates Shell script code base
 */
function generateShellScriptCodeBase() {
    return `\n#!/bin/bash\n\n# Basic shell script\n\n# Main function - entry point of the program\nmain() {\n    echo "Hello, World!"\n    echo "This is a basic shell script."\n}\n\n# Execute main function\nmain "$@"\n\n# Alternative direct approach\n# echo "Hello, World!"\n# echo "This is a basic shell script."\n\n# Example function with parameters\n# greet() {\n#     local name="\${1:-World}"\n#     echo "Hello, \$name!"\n# }\n\n# Example usage\n# greet "TSI Student"\n\n# Example with arrays and variables\n# languages=("Bash" "Python" "Ruby")\n# config_file="/etc/myapp.conf"\n# debug_mode=true\n\n# for lang in "\${languages[@]}"; do\n#     echo "Language: \$lang"\n# done\n\n# Example error handling\n# set -euo pipefail  # Exit on error, undefined vars, pipe failures\n# trap 'echo "Error on line \$LINENO"' ERR\n`;
}

/**
 * Generates Smalltalk code base
 */
function generateSmalltalkCodeBase() {
    return `\n"Basic Smalltalk program"\n\n"Main class definition"\nObject subclass: #HelloWorld\n    instanceVariableNames: ''\n    classVariableNames: ''\n    poolDictionaries: ''\n    category: 'TSI-Examples'.\n\n"Main method"\nHelloWorld class >> main\n    "Main method - entry point of the program"\n    Transcript show: 'Hello, World!'.\n    Transcript cr.\n    Transcript show: 'This is a basic Smalltalk program.'.\n    Transcript cr.\n\n"Execute main method"\nHelloWorld main.\n\n"Alternative direct approach"\n"Transcript show: 'Hello, World!'; cr."\n"Transcript show: 'This is a basic Smalltalk program.'; cr."\n\n"Example instance method"\n"HelloWorld >> greet: aName\n    ^'Hello, ', aName, '!'"\n\n"Example usage"\n"| hello |\nhello := HelloWorld new.\nTranscript show: (hello greet: 'TSI Student'); cr."\n`;
}

/**
 * Generates Solidity code base
 */
function generateSolidityCodeBase() {
    return `\n// SPDX-License-Identifier: MIT\npragma solidity ^0.8.0;\n\n/**\n * @title HelloWorld\n * @dev Basic Solidity contract\n */\ncontract HelloWorld {\n    // State variables\n    string public message;\n    address public owner;\n    uint256 public createdAt;\n    \n    // Events\n    event MessageChanged(string newMessage, address changedBy);\n    \n    // Constructor\n    constructor() {\n        message = "Hello, World!";\n        owner = msg.sender;\n        createdAt = block.timestamp;\n    }\n    \n    // Main function - view the message\n    function getMessage() public view returns (string memory) {\n        return message;\n    }\n    \n    // Function to change the message (only owner)\n    function setMessage(string memory _newMessage) public {\n        require(msg.sender == owner, "Only owner can change message");\n        message = _newMessage;\n        emit MessageChanged(_newMessage, msg.sender);\n    }\n    \n    // Function to get contract info\n    function getInfo() public view returns (string memory, address, uint256) {\n        return (message, owner, createdAt);\n    }\n    \n    // Fallback function\n    receive() external payable {\n        // Handle plain Ether transfers\n    }\n}\n`;
}

/**
 * Generates TCL code base
 */
function generateTclCodeBase() {
    return `\n# Basic TCL script\n\n# Main procedure - entry point of the program\nproc main {} {\n    puts "Hello, World!"\n    puts "This is a basic TCL script."\n}\n\n# Execute main procedure\nmain\n\n# Alternative direct approach\n# puts "Hello, World!"\n# puts "This is a basic TCL script."\n\n# Example procedure with parameters\n# proc greet {name} {\n#     if {$name eq ""} {\n#         set name "World"\n#     }\n#     return "Hello, $name!"\n# }\n\n# Example usage\n# puts [greet "TSI Student"]\n\n# Example with arrays and variables\n# set languages [list "TCL" "Python" "Ruby"]\n# array set config {\n#     version "1.0"\n#     debug 1\n#     author "TSI Student"\n# }\n\n# foreach lang $languages {\n#     puts "Language: $lang"\n# }\n\n# puts "Version: $config(version)"\n`;
}

/**
 * Generates TypeScript React code base
 */
function generateTypeScriptReactCodeBase() {
    return `\nimport React from 'react';\nimport ReactDOM from 'react-dom/client';\n\n// Interface definitions\ninterface AppProps {}\n\ninterface AppState {\n  message: string;\n  count: number;\n}\n\n/**\n * Main React component\n */\nfunction App(): JSX.Element {\n  const [state, setState] = React.useState<AppState>({\n    message: 'Hello, World!',\n    count: 0\n  });\n\n  const handleIncrement = (): void => {\n    setState(prev => ({ ...prev, count: prev.count + 1 }));\n  };\n\n  const handleMessageChange = (newMessage: string): void => {\n    setState(prev => ({ ...prev, message: newMessage }));\n  };\n\n  return (\n    <div className="app">\n      <header className="app-header">\n        <h1>{state.message}</h1>\n        <p>This is a basic TypeScript React application.</p>\n        <p>Count: {state.count}</p>\n      </header>\n      <main>\n        <section>\n          <h2>Welcome to TSI</h2>\n          <p>Transport and Telecommunication Institute</p>\n          <button onClick={handleIncrement}>\n            Increment Counter\n          </button>\n          <button onClick={() => handleMessageChange('Hello, TSI Student!')}>\n            Change Message\n          </button>\n        </section>\n      </main>\n    </div>\n  );\n}\n\n/**\n * Render the application\n */\nconst root = ReactDOM.createRoot(\n  document.getElementById('root') as HTMLElement\n);\nroot.render(<App />);\n\nexport default App;\n`;
}

/**
 * Generates VB code base
 */
function generateVbCodeBase() {
    return `\n' Basic VB.NET program\n\nImports System\n\nModule HelloWorld\n    ' Main subroutine - entry point of the program\n    Sub Main()\n        Console.WriteLine("Hello, World!")\n        Console.WriteLine("This is a basic VB.NET program.")\n        \n        ' Wait for user input\n        Console.ReadLine()\n    End Sub\n    \n    ' Example function\n    ' Function Greet(name As String) As String\n    '     If String.IsNullOrEmpty(name) Then\n    '         name = "World"\n    '     End If\n    '     Return "Hello, " & name & "!"\n    ' End Function\n    \n    ' Example usage\n    ' Console.WriteLine(Greet("TSI Student"))\nEnd Module\n\n' Alternative class-based approach\n' Public Class HelloWorldClass\n'     Public Shared Sub Main(args() As String)\n'         Console.WriteLine("Hello, World!")\n'         Console.WriteLine("This is a basic VB.NET program.")\n'     End Sub\n' End Class\n`;
}

/**
 * Generates VHDL code base
 */
function generateVhdlCodeBase() {
    return `\n-- Basic VHDL program\n\nlibrary IEEE;\nuse IEEE.STD_LOGIC_1164.ALL;\nuse IEEE.NUMERIC_STD.ALL;\n\n-- Entity declaration\nentity HelloWorld is\n    Port (\n        clk : in STD_LOGIC;\n        reset : in STD_LOGIC;\n        output : out STD_LOGIC_VECTOR(7 downto 0)\n    );\nend HelloWorld;\n\n-- Architecture implementation\narchitecture Behavioral of HelloWorld is\n    signal counter : unsigned(7 downto 0) := (others => '0');\nbegin\n    -- Main process\n    process(clk, reset)\n    begin\n        if reset = '1' then\n            counter <= (others => '0');\n        elsif rising_edge(clk) then\n            counter <= counter + 1;\n        end if;\n    end process;\n    \n    -- Output assignment\n    output <= std_logic_vector(counter);\n    \n    -- Alternative combinational logic\n    -- output <= \"10101010\" when reset = '1' else\n    --           std_logic_vector(counter);\n    \nend Behavioral;\n\n-- Testbench (uncomment to use)\n-- library IEEE;\n-- use IEEE.STD_LOGIC_1164.ALL;\n-- \n-- entity HelloWorld_TB is\n-- end HelloWorld_TB;\n-- \n-- architecture Testbench of HelloWorld_TB is\n--     component HelloWorld\n--         Port (\n--             clk : in STD_LOGIC;\n--             reset : in STD_LOGIC;\n--             output : out STD_LOGIC_VECTOR(7 downto 0)\n--         );\n--     end component;\n--     \n--     signal clk : STD_LOGIC := '0';\n--     signal reset : STD_LOGIC := '0';\n--     signal output : STD_LOGIC_VECTOR(7 downto 0);\n--     \n-- begin\n--     uut: HelloWorld Port Map (\n--         clk => clk,\n--         reset => reset,\n--         output => output\n--     );\n--     \n--     clk_process: process\n--     begin\n--         clk <= '0';\n--         wait for 10 ns;\n--         clk <= '1';\n--         wait for 10 ns;\n--     end process;\n--     \n-- end Testbench;\n`;
}

/**
 * Generates Vue code base
 */
function generateVueCodeBase() {
    return `\n<template>\n  <div id="app" class="app">\n    <header class="app-header">\n      <h1>{{ message }}</h1>\n      <p>This is a basic Vue.js application.</p>\n      <p>Count: {{ count }}</p>\n    </header>\n    \n    <main>\n      <section>\n        <h2>Welcome to TSI</h2>\n        <p>Transport and Telecommunication Institute</p>\n        \n        <div class="buttons">\n          <button @click="increment" class="btn">\n            Increment Counter\n          </button>\n          <button @click="changeMessage" class="btn">\n            Change Message\n          </button>\n          <button @click="reset" class="btn btn--secondary">\n            Reset\n          </button>\n        </div>\n        \n        <div v-if="showWelcome" class="welcome">\n          <p>Welcome, {{ studentName }}!</p>\n        </div>\n      </section>\n    </main>\n  </div>\n</template>\n\n<script>\nexport default {\n  name: 'App',\n  data() {\n    return {\n      message: 'Hello, World!',\n      count: 0,\n      studentName: 'TSI Student',\n      showWelcome: false\n    }\n  },\n  methods: {\n    increment() {\n      this.count++;\n    },\n    changeMessage() {\n      this.message = 'Hello, TSI Student!';\n      this.showWelcome = true;\n    },\n    reset() {\n      this.message = 'Hello, World!';\n      this.count = 0;\n      this.showWelcome = false;\n    }\n  },\n  mounted() {\n    console.log('Vue app mounted successfully!');\n  }\n}\n</script>\n\n<style scoped>\n.app {\n  font-family: Arial, sans-serif;\n  margin: 40px;\n  line-height: 1.6;\n}\n\n.app-header {\n  text-align: center;\n  margin-bottom: 2rem;\n}\n\n.buttons {\n  display: flex;\n  gap: 1rem;\n  justify-content: center;\n  margin: 2rem 0;\n}\n\n.btn {\n  padding: 0.5rem 1rem;\n  background-color: #3498db;\n  color: white;\n  border: none;\n  border-radius: 5px;\n  cursor: pointer;\n  transition: background-color 0.3s ease;\n}\n\n.btn:hover {\n  background-color: #2980b9;\n}\n\n.btn--secondary {\n  background-color: #95a5a6;\n}\n\n.btn--secondary:hover {\n  background-color: #7f8c8d;\n}\n\n.welcome {\n  text-align: center;\n  margin-top: 2rem;\n  padding: 1rem;\n  background-color: #e8f5e8;\n  border-radius: 5px;\n}\n</style>\n`;
}

/**
 * Generates XML code base
 */
function generateXmlCodeBase() {
    return `\n<?xml version="1.0" encoding="UTF-8"?>\n<!-- Basic XML document template -->\n\n<tsi:document xmlns:tsi="https://tsi.lv/schema" \n              xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n              xsi:schemaLocation="https://tsi.lv/schema tsi-schema.xsd">\n  \n  <!-- Document metadata -->\n  <tsi:metadata>\n    <tsi:title>TSI Header - Basic XML Template</tsi:title>\n    <tsi:author>TSI Student</tsi:author>\n    <tsi:created>2025-09-24</tsi:created>\n    <tsi:version>1.0</tsi:version>\n  </tsi:metadata>\n  \n  <!-- Main content -->\n  <tsi:content>\n    <tsi:greeting>Hello, World!</tsi:greeting>\n    <tsi:description>This is a basic XML document.</tsi:description>\n    \n    <!-- Institution information -->\n    <tsi:institution>\n      <tsi:name>Transport and Telecommunication Institute</tsi:name>\n      <tsi:location>Riga, Latvia</tsi:location>\n      <tsi:website>https://tsi.lv</tsi:website>\n      \n      <tsi:programs>\n        <tsi:program id="cs" level="bachelor">\n          <tsi:name>Computer Science</tsi:name>\n          <tsi:duration>4 years</tsi:duration>\n        </tsi:program>\n        <tsi:program id="ee" level="bachelor">\n          <tsi:name>Electrical Engineering</tsi:name>\n          <tsi:duration>4 years</tsi:duration>\n        </tsi:program>\n      </tsi:programs>\n    </tsi:institution>\n    \n    <!-- Example data structure -->\n    <tsi:courses>\n      <tsi:course code="CS101">\n        <tsi:title>Programming Fundamentals</tsi:title>\n        <tsi:credits>6</tsi:credits>\n        <tsi:semester>1</tsi:semester>\n      </tsi:course>\n      <tsi:course code="MATH201">\n        <tsi:title>Advanced Mathematics</tsi:title>\n        <tsi:credits>4</tsi:credits>\n        <tsi:semester>2</tsi:semester>\n      </tsi:course>\n    </tsi:courses>\n  </tsi:content>\n  \n</tsi:document>\n`;
}

/**
 * Generates YAML code base
 */
function generateYamlCodeBase() {
    return `\n# Basic YAML configuration file\n\n# Document metadata\nmetadata:\n  title: "TSI Header - Basic YAML Template"\n  author: "TSI Student"\n  created: "2025-09-24"\n  version: "1.0"\n  description: "This is a basic YAML configuration."\n\n# Application configuration\napp:\n  name: "TSI Application"\n  version: "1.0.0"\n  debug: true\n  environment: "development"\n  \n  # Server configuration\n  server:\n    host: "localhost"\n    port: 3000\n    ssl: false\n    timeout: 30\n    \n  # Database configuration\n  database:\n    type: "postgresql"\n    host: "localhost"\n    port: 5432\n    name: "tsi_database"\n    user: "tsi_user"\n    # password: "secure_password"  # Should be in environment variables\n    ssl: true\n    pool_size: 10\n\n# Institution information\ninstitution:\n  name: "Transport and Telecommunication Institute"\n  location: "Riga, Latvia"\n  website: "https://tsi.lv"\n  \n  programs:\n    - id: "cs"\n      name: "Computer Science"\n      level: "bachelor"\n      duration: "4 years"\n      \n    - id: "ee"\n      name: "Electrical Engineering"\n      level: "bachelor"\n      duration: "4 years"\n\n# Course configuration\ncourses:\n  - code: "CS101"\n    title: "Programming Fundamentals"\n    credits: 6\n    semester: 1\n    prerequisites: []\n    \n  - code: "MATH201"\n    title: "Advanced Mathematics"\n    credits: 4\n    semester: 2\n    prerequisites: ["MATH101"]\n\n# Feature flags\nfeatures:\n  enable_logging: true\n  enable_caching: true\n  enable_analytics: false\n  experimental_features: false\n\n# Logging configuration\nlogging:\n  level: "info"\n  format: "json"\n  output:\n    - type: "console"\n    - type: "file"\n      path: "/var/log/app.log"\n      rotation: "daily"\n      max_size: "100MB"\n      max_files: 7\n`;
}

/**
 * Generates XSL code base
 */
function generateXslCodeBase() {
    return `\n<?xml version="1.0" encoding="UTF-8"?>\n<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">\n\n<!-- Basic XSL transformation template -->\n<xsl:output method="html" encoding="UTF-8" indent="yes" />\n\n<!-- Main template match -->\n<xsl:template match="/">\n    <html>\n        <head>\n            <title>TSI Header - Basic XSL Template</title>\n        </head>\n        <body>\n            <h1>Hello, World!</h1>\n            <p>This is a basic XSL transformation.</p>\n            \n            <!-- Process document content -->\n            <xsl:apply-templates select="//content" />\n        </body>\n    </html>\n</xsl:template>\n\n<!-- Template for content elements -->\n<xsl:template match="content">\n    <div class="content">\n        <h2><xsl:value-of select="@title" /></h2>\n        <p><xsl:value-of select="." /></p>\n    </div>\n</xsl:template>\n\n<!-- Example: Transform list items -->\n<xsl:template match="items">\n    <ul>\n        <xsl:for-each select="item">\n            <li><xsl:value-of select="." /></li>\n        </xsl:for-each>\n    </ul>\n</xsl:template>\n\n</xsl:stylesheet>\n`;
}

/**
 * Generates Verse code base
 */
function generateVerseCodeBase() {
    return `\n// Basic Verse code\n// Note: Verse is Epic Games' programming language for Fortnite/UEFN\n\nusing { /Fortnite.com/Devices }\nusing { /Verse.org/Simulation }\nusing { /UnrealEngine.com/Temporary/Diagnostics }\n\n# Main device class\nhello_world_device := class(creative_device):\n    \n    # Main function - entry point\n    OnBegin<override>()<suspends>: void =\n        Print("Hello, World!")\n        Print("This is a basic Verse program.")\n        \n    # Example function with parameters\n    Greet(Name: string): string =\n        "Hello, {Name}!"\n    \n    # Example async function\n    WaitAndGreet<public>(Name: string, Seconds: float)<suspends>: void =\n        Sleep(Seconds)\n        Print(Greet(Name))\n`;
}

/**
 * Generates Vimscript code base
 */
function generateVimscriptCodeBase() {
    return `\n\" Basic Vimscript code\n\" Note: This is a Vim script for editor automation\n\n\" Main function - entry point\nfunction! HelloWorld()\n    echo \"Hello, World!\"\n    echo \"This is a basic Vimscript program.\"\nendfunction\n\n\" Call main function\ncall HelloWorld()\n\n\" Example function with parameters\nfunction! Greet(name)\n    if a:name ==# ''\n        let name = 'World'\n    else\n        let name = a:name\n    endif\n    return 'Hello, ' . name . '!'\nendfunction\n\n\" Example usage\n\" echo Greet('TSI Student')\n\n\" Example with variables and lists\n\" let languages = ['Vim', 'Python', 'Ruby']\n\" let config = {'version': '1.0', 'debug': 1}\n\n\" for lang in languages\n\"     echo 'Language: ' . lang\n\" endfor\n\n\" echo 'Version: ' . config.version\n`;
}

/**
 * Generates SED code base
 */
function generateSedCodeBase() {
    return `\n#!/usr/bin/sed -f\n# Basic SED script\n# Note: SED is a stream editor for filtering and transforming text\n\n# Print a greeting message (insert at beginning)\n1i\\\nHello, World!\\\nThis is a basic SED script.\n\n# Example transformations (commented out)\n# s/old/new/g        # Replace 'old' with 'new' globally\n# /pattern/d         # Delete lines containing 'pattern'\n# p                  # Print current line\n# q                  # Quit after first line\n\n# Example: Convert text to uppercase\n# y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/\n\n# Example: Add line numbers\n# =\n# N\n# s/\\n/: /\n`;
}

/**
 * Generates SAS code base
 */
function generateSasCodeBase() {
    return `\n/* Basic SAS program */\n/* Note: SAS is used for statistical analysis */\n\n/* Main data step */\ndata hello_world;\n    message = "Hello, World!";\n    description = "This is a basic SAS program.";\n    put message;\n    put description;\nrun;\n\n/* Example proc step */\nproc print data=hello_world;\nrun;\n\n/* Example with variables */\n/* data tsi_students; */\n/*     input name $ program $ year; */\n/*     datalines; */\n/* John CS 2024 */\n/* Jane EE 2023 */\n/* Bob IT 2025 */\n/* ; */\n/* run; */\n\n/* proc means data=tsi_students; */\n/*     var year; */\n/* run; */\n\n/* proc freq data=tsi_students; */\n/*     tables program; */\n/* run; */\n`;
}

/**
 * Generates Objective-J code base
 */
function generateObjectiveJCodeBase() {
    return `\n// Basic Objective-J program\n// Note: Objective-J is used with Cappuccino framework\n\n@import <Foundation/Foundation.j>\n@import <AppKit/AppKit.j>\n\n@implementation HelloWorld : CPObject\n{\n    CPString message;\n}\n\n// Main function - entry point\n+ (void)main\n{\n    console.log("Hello, World!");\n    console.log("This is a basic Objective-J program.");\n    \n    var app = [[HelloWorld alloc] init];\n    [app greet:@"TSI Student"];\n}\n\n// Initialize\n- (id)init\n{\n    self = [super init];\n    if (self)\n    {\n        message = @"Hello from TSI!";\n    }\n    return self;\n}\n\n// Example method with parameter\n- (void)greet:(CPString)name\n{\n    console.log("Hello, " + name + "!");\n}\n\n// Getter for message\n- (CPString)message\n{\n    return message;\n}\n\n@end\n\n// Execute main\n[HelloWorld main];\n`;
}

module.exports = {
    generateCodeBase
};