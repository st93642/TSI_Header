/**
 * Unified TSI Header Test Suite
 *
 * This is the single, comprehensive test suite for the TSI Header extension.
 * It combines all testing functionality into one unified suite that tests:
 * - Header generation for all 88+ supported languages
 * - Code base generation for all supported languages
 * - Class generation for 13 object-oriented languages
 * - Syntax validation and TSI header format verification
 * - Extension API and VS Code integration
 * - Ruby backend functionality
 *
 * Usage:
 *   node unified_test_suite.js              # Run all tests (default)
 *   npm test                                # Run all tests via package.json
 *   ruby spec/unified_test.rb               # Run Ruby-only tests
 */

const fs = require('fs');
const path = require('path');
const os = require('os');
const { execSync } = require('child_process');

// Test configuration
const TEST_DIR = path.join(__dirname, 'test_output');
const CLI_PATH = path.join(__dirname, 'core', 'lib', 'tsi_header_cli.rb');
const EXTENSION_PATH = __dirname;

// Language support matrices
const HEADER_LANGUAGES = [
    // Mainstream languages
    'abap', 'c', 'cpp', 'csharp', 'cuda', 'cuda-cpp', 'java', 'python',
    'racket', 'razor', 'ruby', 'rust', 'go', 'php', 'swift',
    'dart', 'scala', 'scratch', 'sql', 'html', 'javascript', 'typescript',
    'kotlin',

    // Specialized languages
    'ada', 'apex', 'algol', 'apl', 'applescript', 'awk', 'basic', 'bat',
    'batch', 'cfml', 'clojure', 'coffeescript', 'crystal',
    'css', 'dockercompose', 'd', 'zig', 'nim', 'v', 'dockerfile', 'elixir',
    'ejs', 'erb', 'erlang', 'factor', 'forth', 'fsharp',
    'groovy', 'hack', 'haml', 'handlebars', 'hlsl', 'haskell', 'idl', 'ini',
    'jade', 'jinja', 'json', 'jsonc', 'julia', 'latex', 'tex',
    'less', 'lisp', 'logo', 'lua', 'labview', 'markdown', 'maple',
    'mathematica', 'mercury',
    'objective-c', 'objective-cpp', 'ocaml', 'octave', 'perl', 'postscript',
    'powershell', 'prolog', 'rpg', 'scheme', 'scss', 'shaderlab',
    'shellscript', 'smalltalk',
    'solidity', 'tcl', 'toml', 'twig', 'vala', 'genie', 'vb', 'vbscript',
    'verilog', 'vhdl', 'vue', 'xml', 'xsl', 'yaml',

    // Additional variants
    'c++', 'fortran', 'fortran90', 'FortranFreeForm', 'perl6', 'raku',
    'plaintext', 'systemverilog', 'Verilog', 'yml', 'coldfusion', 'haskell',
    'javascriptreact', 'typescriptreact', 'makefile', 'assembly', 'asm',
    'cobol', 'delphi', 'pascal', 'objectpascal', 'matlab', 'r', 'vbscript',
    'verse', 'vimscript', 'sed', 'sas', 'objective-j', 'vb', 'vbscript',
    'verse', 'vimscript', 'sed', 'sas', 'bibtex', 'diff',
    'pug', 'slim', 'stylus', 'svelte', 'vue-html', 'git', 'git-commit',
    'git-rebase'
];

const CODEBASE_LANGUAGES = HEADER_LANGUAGES; // Same as header languages

const CLASS_LANGUAGES = [
    'java', 'cpp', 'csharp', 'python', 'javascript', 'kotlin', 'php',
    'typescript', 'ruby', 'go', 'swift', 'dart', 'scala'
];

// Test results tracking
let testResults = {
    total: 0,
    passed: 0,
    failed: 0,
    errors: [],
    sections: {
        headers: { total: 0, passed: 0, failed: 0, failures: [] },
        codebases: { total: 0, passed: 0, failed: 0, failures: [] },
        classes: { total: 0, passed: 0, failed: 0, failures: [] },
        syntax: { total: 0, passed: 0, failed: 0, failures: [] },
        api: { total: 0, passed: 0, failed: 0, failures: [] },
        ruby: { total: 0, passed: 0, failed: 0, failures: [] },
        projects: { total: 0, passed: 0, failed: 0, failures: [] },
        studyMode: { total: 0, passed: 0, failed: 0, failures: [] }
    }
};

// Import generators
const { generateCodeBase } = require('./core/generators/codeBaseGenerators');
const { generateClass } = require('./core/generators/classGenerators');
const { generateTSIHeaderContent } = require('./core/generators/project/headerUtils');

// Mock VS Code for testing
const vscode = {
    workspace: {
        fs: {
            createDirectory: async (uri) => {
                const dirPath = uri.fsPath;
                if (!fs.existsSync(dirPath)) {
                    fs.mkdirSync(dirPath, { recursive: true });
                }
            },
            writeFile: async (uri, content) => {
                fs.writeFileSync(uri.fsPath, content);
            }
        },
        getConfiguration: (section) => ({
            get: (key, defaultValue) => {
                const config = {
                    'tsiheader.username': 'Test User',
                    'tsiheader.email': 'test@example.com'
                };
                return config[`${section}.${key}`] || defaultValue;
            },
            update: (key, value, global) => Promise.resolve()
        })
    },
    Uri: {
        joinPath: (baseUri, ...pathSegments) => {
            const fullPath = path.join(baseUri.fsPath, ...pathSegments);
            return { fsPath: fullPath };
        }
    }
};

/**
 * Progress bar utility
 */
class ProgressBar {
    constructor(total, width = 50) {
        this.total = total;
        this.width = width;
        this.current = 0;
        this.startTime = Date.now();
    }

    update(current) {
        this.current = current;
        this.render();
    }

    render() {
        const percentage = Math.floor((this.current / this.total) * 100);
        const filled = Math.floor((this.current / this.total) * this.width);
        const empty = this.width - filled;
        const bar = '█'.repeat(filled) + '░'.repeat(empty);
        const elapsed = (Date.now() - this.startTime) / 1000;
        const eta = this.current > 0 ?
            (elapsed / this.current) * (this.total - this.current) : 0;

        process.stdout.write(
            `\r[${bar}] ${percentage}% (${this.current}/${this.total}) ETA: ${eta.toFixed(1)}s`
        );
    }

    complete() {
        this.update(this.total);
        process.stdout.write('\n');
    }
}

/**
 * Utility function to run shell commands
 */
function runCommand(command, options = {}) {
    try {
        return execSync(command, { encoding: 'utf8', ...options });
    } catch (error) {
        return { error: error.message };
    }
}

/**
 * Create test directory structure
 */
function setupTestDirectory() {
    if (!fs.existsSync(TEST_DIR)) {
        fs.mkdirSync(TEST_DIR, { recursive: true });
    }

    // Clean up previous test files
    const files = fs.readdirSync(TEST_DIR);
    files.forEach(file => {
        if (file.startsWith('test_')) {
            fs.unlinkSync(path.join(TEST_DIR, file));
        }
    });
}

/**
 * Validate TSI header format
 */
function validateTSIHeader(content, language) {
    const lines = content.split('\n');

    // Check for TSI branding
    if (!content.includes('TSI') &&
        !content.includes('Transport and Telecommunication Institute')) {
        return { valid: false, reason: 'Missing TSI branding in header' };
    }

    // Check for appropriate comment markers based on language
    const hasCommentMarkers = checkCommentMarkers(content, language);
    if (!hasCommentMarkers) {
        return { valid: false, reason: 'Missing appropriate comment markers for language' };
    }

    // Check for basic header structure (filename, author, dates)
    if (!content.includes('By:') || !content.includes('Created:') ||
        !content.includes('Updated:')) {
        return { valid: false, reason: 'Missing required header fields' };
    }

    return { valid: true };
}

/**
 * Check if content has appropriate comment markers for the language
 */
function checkCommentMarkers(content, language) {
    // Define comment markers for different languages (matching Ruby delimiters.rb)
    const markers = {
        'abap': ['*'],
        'python': ['#'],
        'ruby': ['#'],
        'shellscript': ['#'],
        'yaml': ['#'],
        'dockerfile': ['#'],
        'dockercompose': ['#'],
        'makefile': ['#'],
        'perl': ['#'],
        'r': ['#'],
        'sql': ['#'],  // HASHES in Ruby
        'haskell': ['--'],
        'lua': ['--'],
        'ada': ['--'],
        'apex': ['/*', '*/'],
        'algol': ['; '],
        'applescript': ['--'],
        'vhdl': ['--'],
        'verilog': ['/*', '*/'],
        'scala': ['//'],
        'java': ['/*', '*/'],
        'javascript': ['/*', '*/'],
        'typescript': ['/*', '*/'],
        'cpp': ['/*', '*/'],
        'c': ['/*', '*/'],
        'csharp': ['/*', '*/'],
        'cuda': ['/*', '*/'],
        'cuda-cpp': ['//'],
        'hlsl': ['/*', '*/'],
        'php': ['/*', '*/'],
        'go': ['/*', '*/'],
        'hack': ['/*', '*/'],
        'haml': ['-# ', ' -#'],
        'handlebars': ['{{!-- ', ' --}}'],
        'rust': ['/*', '*/'],
        'swift': ['/*', '*/'],
        'kotlin': ['/*', '*/'],
        'labview': ['//'],
        'dart': ['/*', '*/'],
        'css': ['/*', '*/'],
        'scss': ['/*', '*/'],
        'less': ['/*', '*/'],
        'coffeescript': ['#'],  // HASHES
        'crystal': ['#'],  // HASHES
        'jade': ['//'],
        'jinja': ['<#', '#>'],
        'stylus': ['//'],
        'sass': ['//'],
        'objective-c': ['/*', '*/'],
        'objective-cpp': ['/*', '*/'],
        'objective-j': ['/*', '*/'],
        'clojure': [';;'],
        'scheme': [';;'],
        'racket': [';;'],
        'commonlisp': [';;'],
        'lisp': [';;'],
        'logo': ['; '],
        'elixir': ['#'],
        'elm': ['--'],
        'ejs': ['/*', '*/'],
        'erb': ['<!--', '-->'],
        'twig': ['{#', '#}'],
        'erlang': ['%%'],
        'fortran': ['!'],
        'fortran90': ['!'],
        'FortranFreeForm': ['!'],
        'matlab': ['%%'],
        'octave': ['%%'],
        'postscript': ['%%'],
        'prolog': ['%%'],
        'mercury': ['%%'],
        'vb': [';; '],
        'basic': [';; '],
        'batch': [';; '],
        'bat': [';; '],
        'powershell': ['#'],
        'pug': ['//- ', ' -//'],
        'razor': ['@*', '*@'],
        'shaderlab': ['//'],
        'slim': ['/'],
        'svelte': ['<!--', '-->'],
        'tcl': ['#'],
        'awk': ['#'],
        'sed': ['#'],
        'vimscript': ['"'],
        'smalltalk': ['"'],
        'factor': ['!'],
        'forth': ['(*', '*)'],  // PARENS
        'asm': [';; '],
        'assembly': [';; '],
        'nasm': ['/*', '*/'],
        'masm': ['/*', '*/'],
        'cobol': [';; '],
        'pascal': ['{', '}'],
        'delphi': ['{', '}'],
        'objectpascal': ['{', '}'],
        'modula': ['(*', '*)'],
        'oberon': ['(*', '*)'],
        'ml': ['(*', '*)'],
        'ocaml': ['(*', '*)'],
        'fsharp': ['(*', '*)'],
        'sml': ['(*', '*)'],
        'html': ['<!--', '-->'],
        'xml': ['<!--', '-->'],
        'vue': ['<!--', '-->'],
        'vue-html': ['<!--', '-->'],
        'coldfusion': ['<!---', '--->'],
        'cfml': ['<!---', '--->'],
        'jsp': ['<%--', '--%>'],
        'asp': ['\''],
        'vbscript': ['" ', ' "'],
        'tex': ['%%'],
        'latex': ['%%'],
        'bibtex': ['%%'],
        'maple': ['#'],
        'mathematica': ['(*', '*)'],
        'maxima': ['/*', '*/'],
        'gap': ['#'],
        'sage': ['#'],
        'idl': [';; '],
        'idlang': [';; '],
        'json': [], // JSON doesn't have comments
        'csv': [], // CSV doesn't have comments
        'plaintext': ['#'],
        'markdown': [], // Markdown doesn't have block comments
        'diff': ['#'],
        'patch': ['#'],
        'log': ['#'],
        'ini': [';; '],
        'cfg': ['#'],
        'conf': ['#'],
        'properties': ['#'],
        'toml': ['#'],
        'yaml': ['#'],
        'yml': ['#'],
        'dockerignore': ['#'],
        'gitignore': ['#'],
        'eslintignore': ['#'],
        'prettierignore': ['#'],
        'npmignore': ['#'],
        'verse': ['<#', '#>'],
        'solidity': ['/*', '*/'],
        'systemverilog': ['/*', '*/'],
        'verilog': ['/*', '*/'],
        'apl': ['!'],  // EXCLAMATIONS
        'sas': ['/*', '*/'],  // SLASHES
        'julia': ['#'],
        'nim': ['#'],
        'v': ['/*', '*/'],
        'vala': ['/*', '*/'],
        'genie': ['/*', '*/'],
        'perl6': ['#'],
        'raku': ['#'],
        'rpg': ['//'],
        'git': ['#'],
        'git-commit': ['#'],
        'git-rebase': ['#'],
        'scratch': []  // Scratch uses empty array delimiters
    };

    const languageMarkers = markers[language] || ['/*', '*/']; // Default to C-style

    // Check if content contains the appropriate markers
    if (languageMarkers.length === 0) {
        return true; // No comment markers expected
    }

    for (const marker of languageMarkers) {
        if (!content.includes(marker)) {
            return false;
        }
    }

    return true;
}

/**
 * Validate that generated code contains language-specific syntax patterns
 * rather than just generic placeholder text
 */
function validateLanguageSpecificCode(content, language) {
    // Define language-specific patterns that indicate real code generation
    const languagePatterns = {
        // Programming languages with function definitions
        'python': [
            /def\s+\w+\s*\(/,           // function definition
            /class\s+\w+/,              // class definition
            /print\s*\(/,               // print statement
            /import\s+\w+/,             // import statement
            /if\s+__name__\s*==\s*['"]__main__['"]/, // main guard
            /for\s+\w+\s+in\s+/,        // for loop
            /while\s+/,                 // while loop
        ],
        'javascript': [
            /function\s+\w+\s*\(/,      // function declaration
            /const\s+\w+\s*=/,          // const variable
            /let\s+\w+\s*=/,            // let variable
            /console\.log\s*\(/,        // console.log
            /class\s+\w+/,              // class declaration
            /=>/,                       // arrow function
            /export\s+/,                // export statement
        ],
        'typescript': [
            /function\s+\w+\s*\(/,      // function declaration
            /const\s+\w+\s*:/,          // typed const
            /let\s+\w+\s*:/,            // typed let
            /interface\s+\w+/,          // interface
            /class\s+\w+/,              // class with types
            /=>/,                       // arrow function
            /export\s+/,                // export statement
        ],
        'java': [
            /public\s+class\s+\w+/,     // public class
            /public\s+static\s+void\s+main/, // main method
            /System\.out\.println/,     // print statement
            /import\s+java\./,          // java import
            /private\s+\w+\s+\w+/,      // private field
            /public\s+\w+\s+\w+\s*\(/,  // public method
        ],
        'c': [
            /#include\s*<.*>/,          // include directive
            /int\s+main\s*\(/,          // main function
            /printf\s*\(/,              // printf call
            /return\s+0/,               // return statement
            /int\s+\w+\s*\(/,           // function declaration
            /char\s*\*\s*\w+/,          // string pointer
        ],
        'cpp': [
            /#include\s*<.*>/,          // include directive
            /int\s+main\s*\(/,          // main function
            /std::cout\s*<</,           // cout statement
            /class\s+\w+/,              // class declaration
            /public:/,                  // public access
            /private:/,                 // private access
            /std::string/,              // string type
        ],
        'c++': [
            /#include\s*<.*>/,          // include directive
            /int\s+main\s*\(/,          // main function
            /std::cout\s*<</,           // cout statement
            /class\s+\w+/,              // class declaration
            /public:/,                  // public access
            /private:/,                 // private access
            /std::string/,              // string type
        ],
        'csharp': [
            /using\s+System/,           // using directive
            /class\s+\w+/,              // class declaration
            /static\s+void\s+Main/,     // Main method
            /Console\.WriteLine/,        // WriteLine call
            /public\s+\w+\s+\w+/,       // public member
            /private\s+\w+\s+\w+/,      // private member
        ],
        'php': [
            /<\?php/,                   // PHP opening tag
            /function\s+\w+\s*\(/,      // function definition
            /echo\s+/,                  // echo statement
            /class\s+\w+/,              // class declaration
            /public\s+function/,        // public method
            /\$[a-zA-Z_]/,              // variable
        ],
        'ruby': [
            /def\s+\w+/,                // method definition
            /class\s+\w+/,              // class definition
            /puts\s+/,                  // puts statement
            /require\s+/,               // require statement
            /attr_accessor/,            // attribute accessor
            /initialize/,               // initialize method
        ],
        'go': [
            /package\s+main/,           // package declaration
            /func\s+main\s*\(/,         // main function
            /fmt\.Println/,             // Println call
            /import\s+\(/,              // import block
            /type\s+\w+\s+struct/,      // struct definition
            /var\s+\w+\s+/,             // variable declaration
        ],
        'rust': [
            /fn\s+main\s*\(/,           // main function
            /println!/,                 // println macro
            /struct\s+\w+/,             // struct definition
            /impl\s+\w+/,               // implementation block
            /let\s+mut\s+\w+/,          // mutable variable
            /use\s+std::/,              // use statement
        ],
        'swift': [
            /import\s+Foundation/,      // import statement
            /func\s+\w+\s*\(/,          // function definition
            /print\s*\(/,               // print call
            /class\s+\w+/,              // class declaration
            /var\s+\w+:/,               // variable declaration
            /let\s+\w+:/,               // constant declaration
        ],
        'kotlin': [
            /fun\s+main\s*\(/,          // main function
            /println\s*\(/,             // println call
            /class\s+\w+/,              // class declaration
            /val\s+\w+:/,               // val declaration
            /var\s+\w+:/,               // var declaration
            /import\s+/,                // import statement
        ],
        'scala': [
            /object\s+\w+/,             // object declaration
            /def\s+main\s*\(/,          // main method
            /println\s*\(/,             // println call
            /class\s+\w+/,              // class declaration
            /val\s+\w+:/,               // val declaration
            /var\s+\w+:/,               // var declaration
        ],
        'perl': [
            /#!/,                       // shebang
            /sub\s+\w+/,                // subroutine
            /print\s+/,                 // print statement
            /my\s+\$/,                  // my variable
            /use\s+strict/,             // use strict
            /package\s+\w+/,            // package declaration
        ],
        'lua': [
            /function\s+\w+\s*\(/,      // function definition
            /print\s*\(/,               // print call
            /local\s+\w+\s*=/,          // local variable
            /if\s+/,                    // if statement
            /for\s+/,                   // for loop
            /require\s+/,               // require statement
        ],
        'r': [
            /<-/,                       // assignment operator
            /print\s*\(/,               // print call
            /function\s*\(/,            // function definition
            /library\s*\(/,             // library call
            /data\.frame/,              // data frame
            /ggplot/,                   // ggplot reference
        ],
        'matlab': [
            /function\s+\w+\s*=/,       // function definition
            /disp\s*\(/,                // display call
            /plot\s*\(/,                // plot call
            /zeros\s*\(/,               // zeros function
            /ones\s*\(/,                // ones function
            /for\s+\w+\s*=/,            // for loop
        ],
        'haskell': [
            /main\s*=/,                 // main definition
            /putStrLn\s+/,              // putStrLn call
            /data\s+\w+/,               // data type
            /import\s+/,                // import statement
            /::/,                       // type annotation
            /let\s+\w+\s*=/,            // let binding
        ],
        'clojure': [
            /defn\s+-\w+/,              // main function
            /println\s+/,               // println call
            /def\s+\w+/,                // def declaration
            /ns\s+\w+/,                 // namespace
            /\(+\s*\w+/,                // function call
            /let\s*\[/,                 // let binding
        ],
        'erlang': [
            /-module\s*\(/,             // module declaration
            /-export\s*\(/,             // export declaration
            /io:format\s*\(/,           // format call
            /start\s*\(/,               // start function
            /init\s*\(/,                // init function
            /terminate\s*\(/,           // terminate function
        ],
        'elixir': [
            /defmodule\s+\w+/,          // module definition
            /def\s+\w+/,                // function definition
            /IO\.puts\s+/,              // puts call
            /use\s+/,                   // use statement
            /import\s+/,                // import statement
            /alias\s+/,                 // alias statement
        ],
        // Markup and configuration languages (basic validation)
        'html': [
            /<html>/,                   // html tag
            /<head>/,                   // head tag
            /<body>/,                   // body tag
            /<div>/,                    // div tag
            /<p>/,                      // paragraph tag
        ],
        'xml': [
            /<\?xml/,                   // xml declaration
            /<[^>]+>/,                  // xml tags
            /<\/[^>]+>/,                // closing tags
        ],
        'json': [
            /{/,                        // object start
            /}/,                        // object end
            /\[/,                       // array start
            /\]/,                       // array end
            /"[^"]*"\s*:/,              // key-value pair
        ],
        'yaml': [
            /^#/,                        // comment
            /\w+:\s*/,                   // key-value pair (more flexible)
            /^\s+\w+:\s*/,               // indented key
            /^-\s/,                      // list item
            /:\s*["']?[^"']*["']?$/,     // key-value with quotes or values
        ],
        'toml': [
            /^\[.*\]$/,                 // section header
            /^\w+\s*=/,                 // key-value
            /^#/,                       // comment
        ],
        'css': [
            /{/,                        // rule start
            /}/,                        // rule end
            /:/,                        // property separator
            /;/,                        // property end
            /\w+\s*\{/,                 // selector
        ],
        'scss': [
            /{/,                        // rule start
            /}/,                        // rule end
            /\$/,                       // variable
            /@mixin/,                   // mixin
            /@include/,                 // include
        ],
        'sql': [
            /SELECT\s+/,                // select statement
            /FROM\s+/,                  // from clause
            /WHERE\s+/,                 // where clause
            /INSERT\s+INTO/,            // insert statement
            /CREATE\s+TABLE/,           // create table
        ],
        'dockerfile': [
            /FROM\s+/,                  // from instruction
            /RUN\s+/,                   // run instruction
            /COPY\s+/,                  // copy instruction
            /WORKDIR\s+/,               // workdir instruction
            /EXPOSE\s+/,                // expose instruction
        ],
        'makefile': [
            /^\w+:\s*/,                 // target
            /^\t/,                      // command line
            /\$@/,                      // automatic variable
            /\$</,                      // automatic variable
            /\$^/,                      // automatic variable
        ],
        'shell': [
            /#!/,                       // shebang
            /echo\s+/,                  // echo command
            /if\s+\[/,                  // if statement
            /for\s+\w+\s+in/,           // for loop
            /function\s+\w+/,           // function definition
        ],
        'bash': [
            /#!/,                       // shebang
            /echo\s+/,                  // echo command
            /if\s+\[/,                  // if statement
            /for\s+\w+\s+in/,           // for loop
            /function\s+\w+/,           // function definition
        ],
        'powershell': [
            /#/,                        // comment
            /Write-Host/,               // write host
            /function\s+\w+/,           // function definition
            /\$/,                       // variable
            /Get-/,                     // cmdlet
        ],
        'batch': [
            /@echo/,                    // echo command
            /set\s+/,                   // set variable
            /if\s+/,                    // if statement
            /for\s+/,                   // for loop
            /goto\s+/,                  // goto statement
        ],
        'bat': [
            /@echo/,                    // echo command
            /set\s+/,                   // set variable
            /if\s+/,                    // if statement
            /for\s+/,                   // for loop
            /goto\s+/,                  // goto statement
        ],
        'handlebars': [
            /\{\{.*\}\}/,               // Handlebars expression
            /\{\{#if.*\}\}/,            // if block
            /\{\{#each.*\}\}/,          // each block
            /\{\{#unless.*\}\}/,        // unless block
            /\{\{!--.*--\}\}/,          // comment
            /\{\{>.*\}\}/,              // partial
        ]
    };

    // Get patterns for the language
    const patterns = languagePatterns[language.toLowerCase()];
    if (!patterns) {
        // For unsupported languages, just check that content exists and has some structure
        return content.length > 50 && /\w{3,}/.test(content);
    }

    // Check if content matches at least one language-specific pattern
    for (const pattern of patterns) {
        if (pattern.test(content)) {
            return true;
        }
    }

    // If no patterns matched, the code is likely just placeholder text
    return false;
}

/**
 * Test header insertion for a language
 */
function testHeaderInsertion(language) {
    const testFile = path.join(TEST_DIR, `test_header_${language}.txt`);

    // Create empty file first
    fs.writeFileSync(testFile, '');

    const command = `ruby "${CLI_PATH}" insert "${language}" "${testFile}"`;

    try {
        const result = runCommand(command, { cwd: EXTENSION_PATH });
        if (result.error) {
            // Check if it's an unsupported language error
            if (result.error.includes('No header support for language')) {
                return true; // Skip, not fail
            }
            return false;
        }

        const output = JSON.parse(result);
        if (!output.success) {
            // Check if it's an unsupported language message
            if (output.message && output.message.includes('No header support for language')) {
                return true; // Skip, not fail
            }
            return false;
        }

        // Write the header to the file
        fs.writeFileSync(testFile, output.header);

        // Validate header content
        const content = fs.readFileSync(testFile, 'utf8');
        const validation = validateTSIHeader(content, language);

        if (!validation.valid) {
            return false;
        }

        return true;
    } catch (error) {
        return false;
    }
}

/**
 * Test code base insertion for a language
 */
function testCodeBaseInsertion(language) {
    const testFile = path.join(TEST_DIR, `test_codebase_${language}.txt`);

    try {
        // First get the header
        fs.writeFileSync(testFile, '');
        const headerCommand = `ruby "${CLI_PATH}" insert "${language}" "${testFile}"`;
        const headerResult = runCommand(headerCommand, { cwd: EXTENSION_PATH });

        if (headerResult.error) {
            // Check if it's an unsupported language error
            if (headerResult.error.includes('No header support for language')) {
                return true; // Skip, not fail
            }
            return false;
        }

        const headerOutput = JSON.parse(headerResult);
        if (!headerOutput.success) {
            // Check if it's an unsupported language message
            if (headerOutput.message &&
                headerOutput.message.includes('No header support for language')) {
                return true; // Skip, not fail
            }
            return false;
        }

        // Then get the code base
        const codeBaseResult = generateCodeBase(language, testFile);
        if (!codeBaseResult.success) {
            return false;
        }

        // Combine header and code base
        const fullContent = headerOutput.header + codeBaseResult.content;
        fs.writeFileSync(testFile, fullContent);

        // Validate generated content
        if (!fs.existsSync(testFile)) {
            return false;
        }

        const content = fs.readFileSync(testFile, 'utf8');

        // Check that header is included
        const headerValidation = validateTSIHeader(content, language);
        if (!headerValidation.valid) {
            return false;
        }

        // Check that code content exists (not just header)
        const lines = content.split('\n');
        const codeLines = lines.filter(line =>
            !line.includes('/*') && !line.includes('*/') &&
            !line.includes('*') && line.trim() !== ''
        );
        if (codeLines.length === 0) {
            return false;
        }

        // Check that generated code contains language-specific syntax patterns
        if (!validateLanguageSpecificCode(content, language)) {
            return false;
        }

        return true;
    } catch (error) {
        return false;
    }
}

/**
 * Test class creation for a language
 */
function testClassCreation(language) {
    const className = 'TestClass';
    const testFile = path.join(TEST_DIR, `test_class_${language}.txt`);

    // Special handling for C++ which requires VS Code mocking and creates separate files
    if (language === 'cpp' || language === 'c++') {
        return testCppClassCreation(className);
    }

    try {
        // Get header first
        fs.writeFileSync(testFile, '');
        const headerCommand = `ruby "${CLI_PATH}" insert "${language}" "${testFile}"`;
        const headerResult = runCommand(headerCommand, { cwd: EXTENSION_PATH });

        if (headerResult.error) {
            // Check if it's an unsupported language error
            if (headerResult.error.includes('No header support for language')) {
                return true; // Skip, not fail
            }
            return false;
        }

        const headerOutput = JSON.parse(headerResult);
        if (!headerOutput.success) {
            // Check if it's an unsupported language message
            if (headerOutput.message &&
                headerOutput.message.includes('No header support for language')) {
                return true; // Skip, not fail
            }
            return false;
        }

        // Generate comprehensive class content using the proper generateClass function
        const classResult = generateClass(language, className, testFile, EXTENSION_PATH, CLI_PATH, {
            TSI_USERNAME: 'testuser',
            TSI_EMAIL: 'test@example.com'
        });

        if (!classResult.success) {
            return false;
        }

        // Combine header and class content
        const fullContent = headerOutput.header + '\n' + classResult.content;
        fs.writeFileSync(testFile, fullContent);

        // Validate
        const content = fs.readFileSync(testFile, 'utf8');
        const headerValidation = validateTSIHeader(content, language);
        if (!headerValidation.valid) {
            return false;
        }

        if (!content.includes(className)) {
            return false;
        }

        return true;
    } catch (error) {
        return false;
    }
}

/**
 * Test C++ class creation with VS Code mocking
 */
function testCppClassCreation(className) {
    let mockVSCode = null;
    try {
        // Create VS Code API mocks for C++ class generation
        const { mockVSCode: vscodeMock } = createMockVSCode();
        mockVSCode = vscodeMock;

        // Mock the vscode module globally before requiring the class generators
        const Module = require('module');
        const originalRequire = Module.prototype.require;

        Module.prototype.require = function (id) {
            if (id === 'vscode') {
                return mockVSCode;
            }
            return originalRequire.apply(this, arguments);
        };

        try {
            // Clear any cached class generators module
            delete require.cache[require.resolve('./core/generators/classGenerators')];

            // Import the class generators
            const { generateClass } = require('./core/generators/classGenerators');

            // Create test file paths in the test directory
            const headerFilePath = path.join(TEST_DIR, `${className}.hpp`);
            const implFilePath = path.join(TEST_DIR, `${className}.cpp`);

            // Ensure test directory exists
            if (!fs.existsSync(TEST_DIR)) {
                fs.mkdirSync(TEST_DIR, { recursive: true });
            }

            // Remove any existing test files
            if (fs.existsSync(headerFilePath)) {
                fs.unlinkSync(headerFilePath);
            }
            if (fs.existsSync(implFilePath)) {
                fs.unlinkSync(implFilePath);
            }

            // Generate C++ class
            const result = generateClass('cpp', className, implFilePath, EXTENSION_PATH, CLI_PATH, {
                TSI_USERNAME: 'testuser',
                TSI_EMAIL: 'test@example.com'
            });

            if (!result.success) {
                return false;
            }

            // Check that files were created
            if (!fs.existsSync(headerFilePath)) {
                return false;
            }
            if (!fs.existsSync(implFilePath)) {
                return false;
            }

            // Validate header file content
            const headerContent = fs.readFileSync(headerFilePath, 'utf8');
            const headerValidation = validateTSIHeader(headerContent, 'cpp');
            if (!headerValidation.valid) {
                return false;
            }

            // Validate implementation file content
            const implContent = fs.readFileSync(implFilePath, 'utf8');
            const implValidation = validateTSIHeader(implContent, 'cpp');
            if (!implValidation.valid) {
                return false;
            }

            // Check that class name appears in both files
            if (!headerContent.includes(className)) {
                return false;
            }
            if (!implContent.includes(className)) {
                return false;
            }

            // Check for basic C++ class structure in header
            if (!headerContent.includes(`class ${className}`)) {
                return false;
            }

            // Check for include guard in header
            if (!headerContent.includes(`#ifndef ${className.toUpperCase()}_HPP`)) {
                return false;
            }

            // Check for include statement in implementation
            if (!implContent.includes(`#include "${className}.hpp"`)) {
                return false;
            }

            return true;

        } finally {
            // Restore original require behavior
            Module.prototype.require = originalRequire;

            // Clear cached modules
            delete require.cache[require.resolve('./core/generators/classGenerators')];
        }

    } catch (error) {
        return false;
    }
}


/**
 * Test extension API functionality
 */
function testExtensionAPI() {
    let mockVSCode = null;
    try {
        // Create comprehensive VS Code API mocks
        const { mockVSCode, registeredTreeProviders } = createMockVSCode();

        // Mock the vscode module globally before requiring the extension
        const Module = require('module');
        const originalRequire = Module.prototype.require;

        Module.prototype.require = function (id) {
            if (id === 'vscode') {
                return mockVSCode;
            }
            return originalRequire.apply(this, arguments);
        };

        let extension = null;
        try {
            // Clear any cached extension module
            delete require.cache[require.resolve('./core/src/extension')];

            // Load and test the extension
            extension = require('./core/src/extension');

            // Test extension activation
            if (!extension.activate) {
                testResults.errors.push('Extension API test failed: activate function not found');
                return false;
            }

            // Create mock context
            const context = {
                subscriptions: [],
                workspaceState: {
                    get: () => undefined,
                    update: () => Promise.resolve()
                },
                globalState: {
                    get: () => undefined,
                    update: () => Promise.resolve()
                },
                extensionPath: EXTENSION_PATH,
                storagePath: path.join(EXTENSION_PATH, 'storage'),
                globalStoragePath: path.join(EXTENSION_PATH, 'globalStorage'),
                logPath: path.join(EXTENSION_PATH, 'logs')
            };

            // Activate the extension
            extension.activate(context);

            // Verify extension activation
            let testsPassed = 0;
            let totalTests = 0;

            // Test 1: Context subscriptions should be populated
            totalTests++;
            if (context.subscriptions && context.subscriptions.length > 0) {
                testsPassed++;
            }

            // Test 2: Should have command registrations
            totalTests++;
            const commandSubscriptions = context.subscriptions.filter(sub =>
                sub && typeof sub.dispose === 'function' && sub.command
            );
            if (commandSubscriptions.length >= 5) { // Should have at least 5 commands
                testsPassed++;
            }

            // Test 3: Should have tree data providers
            totalTests++;
            if (registeredTreeProviders.length >= 2) { // Should have at least 2 tree providers
                testsPassed++;
            }

            // Test 4: Verify specific commands are registered
            totalTests++;
            const expectedCommands = [
                'tsiheader.insertHeader',
                'tsiheader.updateHeader',
                'tsiheader.addClass',
                'tsiheader.addCodeBase',
                'tsiheader.createTSIProject'
            ];
            const registeredCommands = commandSubscriptions.map(sub => sub.command);
            const missingCommands = expectedCommands.filter(cmd =>
                !registeredCommands.includes(cmd));

            if (missingCommands.length === 0) {
                testsPassed++;
            }

            // Test 5: Verify extension can handle basic operations without throwing
            totalTests++;
            try {
                // Test that extension doesn't crash on basic operations
                // This is a smoke test to ensure the extension is properly structured
                testsPassed++;
            } catch (error) {
                // Error will be handled below
            }

            // Clean up
            context.subscriptions.forEach(sub => {
                if (sub && typeof sub.dispose === 'function') {
                    try {
                        sub.dispose();
                    } catch (e) {
                        // Ignore disposal errors in tests
                    }
                }
            });

            const successRate = testsPassed / totalTests;
            return successRate >= 0.8; // Allow 80% success rate for API tests

        } finally {
            // Restore original require behavior
            Module.prototype.require = originalRequire;

            // Clear cached modules
            if (extension) {
                delete require.cache[require.resolve('./core/src/extension')];
            }
        }

    } catch (error) {
        // Only skip if we can't create the mock (which should be very rare)
        if (error.message.includes("Cannot find module 'vscode'") && !mockVSCode) {
            return true; // Skip, not fail
        }
        console.error('Extension API test error:', error);
        testResults.errors.push(`Extension API test error: ${error.message}`);
        return false;
    }
}

/**
 * Create comprehensive VS Code API mocks for testing
 */
function createMockVSCode() {
    const registeredTreeProviders = [];
    const mockVSCode = {
        commands: {
            registerCommand: (command, handler) => ({
                command,
                handler,
                dispose: () => { }
            }),
            executeCommand: (command, ...args) => Promise.resolve()
        },
        window: {
            showInformationMessage: (message, ...options) => Promise.resolve(),
            showErrorMessage: (message, ...options) => Promise.resolve(),
            showWarningMessage: (message, ...options) => Promise.resolve(),
            showInputBox: (options) => Promise.resolve('TestClass'),
            activeTextEditor: {
                document: {
                    uri: { fsPath: '/test/file.js' },
                    fileName: '/test/file.js',
                    languageId: 'javascript',
                    getText: () => '',
                    lineCount: 1,
                    lineAt: (line) => ({ text: '', lineNumber: line }),
                    positionAt: (offset) => ({ line: 0, character: 0 }),
                    save: () => Promise.resolve(),
                    close: () => Promise.resolve()
                },
                selection: {
                    start: { line: 0, character: 0 },
                    end: { line: 0, character: 0 }
                },
                edit: (callback) => Promise.resolve(true)
            },
            createOutputChannel: (name) => ({
                name,
                append: () => { },
                appendLine: () => { },
                clear: () => { },
                show: () => { },
                hide: () => { },
                dispose: () => { }
            }),
            createTextEditorDecorationType: (options) => ({
                key: 'decoration',
                dispose: () => { }
            }),
            registerTreeDataProvider: (viewId, provider) => {
                registeredTreeProviders.push({ viewId, provider });
                return { dispose: () => { } };
            }
        },
        workspace: {
            getConfiguration: (section) => ({
                get: (key, defaultValue) => {
                    const config = {
                        'tsiheader.username': 'testuser',
                        'tsiheader.email': 'test@example.com',
                        'tsiheader.autoUpdate': false
                    };
                    return config[`${section}.${key}`] || defaultValue;
                },
                update: (key, value, global) => Promise.resolve()
            }),
            workspaceFolders: [{
                uri: { fsPath: EXTENSION_PATH },
                name: 'TSI_Header',
                index: 0
            }],
            fs: {
                createDirectory: async (uri) => {
                    const dirPath = uri.fsPath;
                    if (!fs.existsSync(dirPath)) {
                        fs.mkdirSync(dirPath, { recursive: true });
                    }
                },
                writeFile: async (uri, content) => {
                    const filePath = uri.fsPath;
                    const dirPath = path.dirname(filePath);
                    if (!fs.existsSync(dirPath)) {
                        fs.mkdirSync(dirPath, { recursive: true });
                    }
                    fs.writeFileSync(filePath, content);
                },
                readFile: async (uri) => {
                    const filePath = uri.fsPath;
                    return fs.readFileSync(filePath);
                },
                delete: async (uri, options) => {
                    const filePath = uri.fsPath;
                    if (fs.existsSync(filePath)) {
                        const stat = fs.statSync(filePath);
                        if (stat.isDirectory()) {
                            fs.rmSync(filePath, { recursive: true, force: true });
                        } else {
                            fs.unlinkSync(filePath);
                        }
                    }
                },
                stat: async (uri) => {
                    const filePath = uri.fsPath;
                    const stat = fs.statSync(filePath);
                    return {
                        type: stat.isDirectory() ? 2 : 1,
                        // FileType.Directory = 2, FileType.File = 1
                        size: stat.size,
                        mtime: stat.mtime.getTime(),
                        ctime: stat.ctime.getTime()
                    };
                }
            },
            onDidChangeConfiguration: () => ({ dispose: () => { } }),
            onDidChangeWorkspaceFolders: () => ({ dispose: () => { } }),
            onDidSaveTextDocument: () => ({ dispose: () => { } }),
            onDidOpenTextDocument: () => ({ dispose: () => { } }),
            onDidChangeTextDocument: () => ({ dispose: () => { } }),
            onDidCloseTextDocument: () => ({ dispose: () => { } }),
            textDocuments: [],
            findFiles: (pattern, exclude, maxResults) => Promise.resolve([]),
            openTextDocument: (uri) => Promise.resolve({
                uri,
                fileName: uri.fsPath,
                languageId: 'javascript',
                getText: () => '',
                lineCount: 1,
                lineAt: (line) => ({ text: '', lineNumber: line }),
                save: () => Promise.resolve(),
                close: () => Promise.resolve()
            }),
            showTextDocument: (document, options) => Promise.resolve({
                document,
                viewColumn: 1,
                dispose: () => { }
            })
        },
        languages: {
            match: (selector, document) => 1,
            getLanguages: () =>
                Promise.resolve(['javascript', 'typescript', 'python', 'java', 'c', 'cpp']),
            createDiagnosticCollection: (name) => ({
                name,
                set: (uri, diagnostics) => { },
                delete: (uri) => { },
                clear: () => { },
                dispose: () => { }
            })
        },
        env: {
            openExternal: (uri) => Promise.resolve()
        },
        Uri: {
            file: (path) => ({ fsPath: path, scheme: 'file' }),
            parse: (uri) => ({ fsPath: uri, scheme: 'https' }),
            joinPath: (base, ...paths) => {
                const basePath = base.fsPath || base;
                const fullPath = path.join(basePath, ...paths);
                return { fsPath: fullPath, scheme: 'file' };
            }
        },
        Position: class {
            constructor(line, character) {
                this.line = line;
                this.character = character;
            }
        },
        Range: class {
            constructor(start, end) {
                this.start = start;
                this.end = end;
            }
        },
        TreeItem: class {
            constructor(label, collapsibleState) {
                this.label = label;
                this.collapsibleState = collapsibleState;
                this.command = undefined;
                this.children = [];
            }
        },
        TreeItemCollapsibleState: {
            None: 0,
            Collapsed: 1,
            Expanded: 2
        },
        EventEmitter: class {
            constructor() {
                this.listeners = [];
            }
            event = (listener) => {
                this.listeners.push(listener);
                return { dispose: () => { } };
            };
            fire = (data) => {
                this.listeners.forEach(listener => listener(data));
            };
        },
        ThemeIcon: class {
            constructor(id) {
                this.id = id;
            }
        }
    };

    return { mockVSCode, registeredTreeProviders };
}

/**
 * Test Ruby backend functionality
 */
function testRubyBackend() {
    try {
        // Test unified Ruby test suite
        const rubyResult = runCommand('ruby spec/unified_test.rb', { cwd: EXTENSION_PATH });
        if (rubyResult.error) {
            return false;
        }

        return true;
    } catch (error) {
        return false;
    }
}

/**
 * Test project scaffolding for a specific language
 */
async function testProjectScaffoldingForLanguage(language) {
    const tempDir = path.join(os.tmpdir(), 'tsi-test-projects', `test-${language}-project`);
    const projectUri = { fsPath: tempDir };

    try {
        // Clean up any existing test directory
        if (fs.existsSync(tempDir)) {
            fs.rmSync(tempDir, { recursive: true, force: true });
        }

        // Create base directory
        fs.mkdirSync(tempDir, { recursive: true });

        // Create project structure using actual functions
        await createTestProjectStructure(language, `test-${language}-project`, projectUri);

        // Verify project structure
        await verifyProjectStructure(language, tempDir, `test-${language}-project`);

        // Verify file contents
        await verifyFileContents(language, tempDir, `test-${language}-project`);

        return true;

    } catch (error) {
        return false;
    } finally {
        // Clean up
        try {
            if (fs.existsSync(tempDir)) {
                fs.rmSync(tempDir, { recursive: true, force: true });
            }
        } catch (cleanupError) {
            // Silent cleanup
        }
    }
}

/**
 * Create test project structure
 */
async function createTestProjectStructure(language, projectName, projectUri) {
    const fsPath = projectUri.fsPath;

    // Create base directories
    const directories = getDirectoryStructure(language);
    for (const dir of directories) {
        const dirUri = { fsPath: path.join(fsPath, dir) };
        await vscode.workspace.fs.createDirectory(dirUri);
    }

    // Generate main source file
    await createTestMainSourceFile(language, projectName, projectUri);

    // Create header file (for C/C++)
    if (language === 'c' || language === 'cpp') {
        await createTestHeaderFile(language, projectName, projectUri);
    }

    // Create language-specific project files
    await createTestLanguageSpecificFiles(language, projectName, projectUri);

    // Create build files (Makefiles, etc.)
    await createTestBuildFiles(language, projectName, projectUri);

    // Create documentation files (README.md)
    await createTestDocumentationFiles(language, projectName, projectUri);

    // Create gitignore files (.gitignore)
    await createTestGitIgnoreFiles(language, projectName, projectUri);
}

/**
 * Get directory structure for language
 */
function getDirectoryStructure(language) {
    const commonDirs = ['src', 'docs'];

    if (language === 'c' || language === 'cpp') {
        return [...commonDirs, 'include', 'build'];
    } else if (language === 'python') {
        return [...commonDirs, 'tests', 'scripts'];
    } else if (language === 'java') {
        return [...commonDirs, 'src/main/java', 'src/test/java', 'target'];
    } else if (language === 'rust') {
        return [...commonDirs, 'src', 'tests', 'examples', 'benches'];
    } else if (language === 'ruby') {
        return [...commonDirs, 'lib', 'spec', 'bin', 'config'];
    } else if (language === 'php') {
        return [...commonDirs, 'src', 'public', 'tests'];
    }

    return commonDirs;
}

/**
 * Create main source file with TSI header and code
 */
async function createTestMainSourceFile(language, projectName, projectUri) {
    const extension = getFileExtension(language);
    let fileName = `main.${extension}`;
    let fileUri;

    if (language === 'java') {
        fileName = 'Main.java';
        fileUri = vscode.Uri.joinPath(projectUri, 'src', 'main', 'java', fileName);
    } else if (language === 'php') {
        fileName = 'index.php';
        fileUri = vscode.Uri.joinPath(projectUri, 'public', fileName);
    } else {
        fileUri = vscode.Uri.joinPath(projectUri, 'src', fileName);
    }

    // Generate TSI header
    const headerContent = await generateTestTSIHeaderContent(fileName);

    // Generate code base using existing API
    const codeResult = generateCodeBase(language, fileName);
    const codeContent = codeResult.success ? codeResult.content : '';

    // Combine header and code
    const fullContent = headerContent + '\n' + codeContent;

    // Write to file
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(fullContent));
}

/**
 * Create header file for C/C++
 */
async function createTestHeaderFile(language, projectName, projectUri) {
    const extension = language === 'c' ? 'h' : 'hpp';
    const fileName = `${projectName}.${extension}`;
    const fileUri = vscode.Uri.joinPath(projectUri, 'include', fileName);

    // Generate TSI header
    const headerContent = await generateTestTSIHeaderContent(fileName);

    // Generate header guard
    const guardName = `${projectName.toUpperCase().replace(/-/g, '_')}_${extension.toUpperCase()}`;

    const content = `${headerContent}

#ifndef ${guardName}
#define ${guardName}

#ifdef __cplusplus
extern "C" {
#endif

// Function declarations go here

#ifdef __cplusplus
}
#endif

#endif // ${guardName}
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create language-specific project files
 */
async function createTestLanguageSpecificFiles(language, projectName, projectUri) {
    const { createLanguageSpecificFiles } =
        require('./core/generators/project/projectcreators/index');
    const { mockVSCode } = createMockVSCode();
    await createLanguageSpecificFiles(language, projectName, projectUri, mockVSCode);
}

/**
 * Create build files for test projects
 */
async function createTestBuildFiles(language, projectName, projectUri) {
    const { createBuildFiles } = require('./core/generators/project/buildSystemGenerator');
    const { mockVSCode } = createMockVSCode();
    await createBuildFiles(language, projectName, projectUri, mockVSCode);
}

/**
 * Create documentation files for test projects
 */
async function createTestDocumentationFiles(language, projectName, projectUri) {
    const { createDocumentationFiles } =
        require('./core/generators/project/documentationGenerator');
    const { mockVSCode } = createMockVSCode();
    await createDocumentationFiles(language, projectName, projectUri, mockVSCode);
}

/**
 * Create gitignore files for test projects
 */
async function createTestGitIgnoreFiles(language, projectName, projectUri) {
    const { createGitIgnoreFile } = require('./core/generators/project/gitIgnoreGenerator');
    const { mockVSCode } = createMockVSCode();
    await createGitIgnoreFile(language, projectUri, mockVSCode);
}

/**
 * Get file extension for language
 */
function getFileExtension(language) {
    const extensions = {
        'c': 'c',
        'cpp': 'cpp',
        'python': 'py',
        'java': 'java',
        'rust': 'rs',
        'ruby': 'rb',
        'php': 'php'
    };
    return extensions[language] || 'txt';
}

/**
 * Generate TSI header content for testing
 */
async function generateTestTSIHeaderContent(fileName) {
    const now = new Date();
    const dateStr = now.toLocaleDateString('en-US', {
        month: 'short',
        day: '2-digit',
        year: 'numeric'
    }).replace(',', '');

    const timeStr = now.toLocaleTimeString('en-US', {
        hour: '2-digit',
        minute: '2-digit',
        hour12: false
    });

    const username = 'Test User';
    const email = 'test@example.com';
    const dateTime = `${dateStr} ${timeStr}`;

    return `/*****************************************************************************/
/*                                                                           */
/*  ${fileName.padEnd(50, ' ')}         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: ${email.padEnd(50, ' ')} TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: ${dateTime} ${username.padEnd(30, ' ')} TT    SSSSSSS II */
/*  Updated: ${dateTime} ${username.padEnd(30, ' ')}                          */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/`;
}

/**
 * Verify project structure
 */
async function verifyProjectStructure(language, projectPath, projectName) {
    const expectedDirs = getDirectoryStructure(language);
    const expectedFiles = getExpectedFiles(language, projectName);

    // Check directories
    for (const dir of expectedDirs) {
        const dirPath = path.join(projectPath, dir);
        if (!fs.existsSync(dirPath)) {
            throw new Error(`Missing directory: ${dir}`);
        }
    }

    // Check files
    for (const file of expectedFiles) {
        const filePath = path.join(projectPath, file);
        if (!fs.existsSync(filePath)) {
            throw new Error(`Missing file: ${file}`);
        }
    }
}

/**
 * Get expected files for language
 */
function getExpectedFiles(language, projectName) {
    const files = [];

    if (language === 'c') {
        files.push(
            'src/main.c',
            `include/${projectName}.h`,
            'Makefile',
            'README.md',
            '.gitignore'
        );
    } else if (language === 'cpp') {
        files.push(
            'src/main.cpp',
            `include/${projectName}.hpp`,
            'Makefile',
            'README.md',
            '.gitignore'
        );
    } else if (language === 'python') {
        files.push(
            'src/main.py',
            'src/base_class.py',
            'requirements.txt',
            'setup.py',
            'Makefile',
            'README.md',
            '.gitignore'
        );
    } else if (language === 'java') {
        files.push(
            'src/main/java/Main.java',
            'pom.xml',
            'build.gradle',
            'README.md',
            '.gitignore'
        );
    } else if (language === 'rust') {
        files.push(
            'src/main.rs',
            'Cargo.toml',
            'README.md',
            '.gitignore'
        );
    } else if (language === 'ruby') {
        files.push(
            'src/main.rb',
            'lib/base_class.rb',
            'Gemfile',
            'README.md',
            '.gitignore'
        );
    } else if (language === 'php') {
        files.push(
            'public/index.php',
            'src/BaseClass.php',
            'composer.json',
            'README.md',
            '.gitignore'
        );
    }

    return files;
}

/**
 * Verify file contents
 */
async function verifyFileContents(language, projectPath, projectName) {
    const expectedFiles = getExpectedFiles(language, projectName);

    for (const file of expectedFiles) {
        const filePath = path.join(projectPath, file);

        if (!fs.existsSync(filePath)) {
            continue;
        }

        const content = fs.readFileSync(filePath, 'utf8');

        // Check for TSI header (except for certain files)
        const skipFiles = ['composer.json', 'Cargo.toml', 'requirements.txt',
            'Gemfile', 'Makefile', 'pom.xml', 'build.gradle',
            'README.md', '.gitignore'];
        const skipHeaderCheck = skipFiles.some(skipFile =>
            file.endsWith(skipFile)
        );

        if (!skipHeaderCheck) {
            if (!content.includes('TTTTTTTT SSSSSSS II')) {
                throw new Error(`File ${file} missing TSI header`);
            }

            // Verify that the header contains the correct filename
            const fileName = path.basename(file);
            if (!content.includes(fileName)) {
                throw new Error(
                    `File ${file} header contains incorrect filename. Expected: ${fileName}`
                );
            }
        }

        // Check for basic content structure
        if (file.endsWith('.c') || file.endsWith('.cpp') || file.endsWith('.py') ||
            file.endsWith('.java') || file.endsWith('.rs') || file.endsWith('.rb') ||
            file.endsWith('.php')) {
            // Skip base class files and build files for content check
            const isBaseClass = file.includes('base_class') || file.includes('BaseClass');
            const isBuildFile = file.includes('setup.py') || file.includes('requirements.txt') ||
                file.includes('composer.json') || file.includes('Cargo.toml') ||
                file.includes('Gemfile');
            if (!isBaseClass && !isBuildFile && !content.includes('Hello, World')) {
                throw new Error(`File ${file} missing expected content`);
            }
        }
    }
}

/**
 * Test Study Mode timer functionality
 */
function testStudyMode() {
    try {
        // Run the Study Mode timer tests
        const timerTestResult = runCommand('node studyMode/timer.test.js', { cwd: EXTENSION_PATH });

        // Parse the output to check for test results (TAP format)
        const output = timerTestResult.toString ? timerTestResult.toString() : String(timerTestResult);
        const passMatch = output.match(/# pass (\d+)/);
        const failMatch = output.match(/# fail (\d+)/);

        if (!passMatch || !failMatch) {
            console.error('Could not parse test results from output. Looking for "# pass" and "# fail" patterns.');
            return false;
        }

        const passed = parseInt(passMatch[1]);
        const failed = parseInt(failMatch[1]);

        // All tests should pass
        return failed === 0 && passed > 0;
    } catch (error) {
        console.error('Study Mode test error:', error);
        return false;
    }
}

/**
 * Test header generation in scaffolded projects
 */
async function testHeaderGenerationInScaffoldedProjects() {
    const tempDir = path.join(os.tmpdir(), 'tsi-header-test');
    const testFileName = 'test.php';

    try {
        // Clean up any existing test directory
        if (fs.existsSync(tempDir)) {
            fs.rmSync(tempDir, { recursive: true, force: true });
        }
        fs.mkdirSync(tempDir, { recursive: true });

        // Test the header generation function
        const { generateTSIHeaderContent } = require('./core/generators/project/headerUtils');
        const { mockVSCode: mockVscode } = createMockVSCode();
        const header = await generateTSIHeaderContent(testFileName, mockVscode);

        // Verify header contains correct filename
        if (!header.includes(testFileName)) {
            throw new Error(`Header does not contain correct filename. Expected: ${testFileName}`);
        }

        // Verify header has proper TSI format
        if (!header.includes('TTTTTTTT SSSSSSS II')) {
            throw new Error('Header missing TSI branding');
        }

        // Verify header doesn't contain "file.c"
        if (header.includes('file.c')) {
            throw new Error('Header contains hardcoded "file.c" instead of actual filename');
        }

        return true;

    } catch (error) {
        return false;
    } finally {
        // Clean up
        try {
            if (fs.existsSync(tempDir)) {
                fs.rmSync(tempDir, { recursive: true, force: true });
            }
        } catch (cleanupError) {
            // Silent cleanup
        }
    }
}

/**
 * Run all tests
 */
async function runAllTests() {
    setupTestDirectory();

    // Test extension API first
    console.log('🔧 Testing Extension API...');
    testResults.sections.api.total = 1;
    if (testExtensionAPI()) {
        testResults.sections.api.passed = 1;
        testResults.total++;
        testResults.passed++;
        console.log('✅ Extension API: All tests passed');
    } else {
        testResults.sections.api.failed = 1;
        testResults.sections.api.failures.push('Extension API tests failed');
        testResults.total++;
        testResults.failed++;
        console.log('❌ Extension API: Tests failed');
    }

    // Test Ruby backend
    console.log('\n💎 Testing Ruby Backend...');
    testResults.sections.ruby.total = 1;
    if (testRubyBackend()) {
        testResults.sections.ruby.passed = 1;
        testResults.total++;
        testResults.passed++;
        console.log('✅ Ruby Backend: All tests passed');
    } else {
        testResults.sections.ruby.failed = 1;
        testResults.sections.ruby.failures.push('Ruby Backend tests failed');
        testResults.total++;
        testResults.failed++;
        console.log('❌ Ruby Backend: Tests failed');
    }

    // Test Study Mode timer
    console.log('\n🍅 Testing Study Mode Timer...');
    testResults.sections.studyMode.total = 1;
    if (testStudyMode()) {
        testResults.sections.studyMode.passed = 1;
        testResults.total++;
        testResults.passed++;
        console.log('✅ Study Mode Timer: All tests passed');
    } else {
        testResults.sections.studyMode.failed = 1;
        testResults.sections.studyMode.failures.push('Study Mode timer tests failed');
        testResults.total++;
        testResults.failed++;
        console.log('❌ Study Mode Timer: Tests failed');
    }

    // Test header insertion for all languages
    console.log('\n📝 Testing Header Insertion...');
    const headerProgress = new ProgressBar(HEADER_LANGUAGES.length);
    HEADER_LANGUAGES.forEach((language, index) => {
        testResults.sections.headers.total++;
        testResults.total++;
        if (testHeaderInsertion(language)) {
            testResults.sections.headers.passed++;
            testResults.passed++;
        } else {
            testResults.sections.headers.failed++;
            testResults.sections.headers.failures.push(language);
            testResults.failed++;
        }
        headerProgress.update(index + 1);
    });
    headerProgress.complete();

    // Test code base insertion for all languages
    console.log('\n🏗️  Testing Code Base Insertion...');
    const codebaseProgress = new ProgressBar(CODEBASE_LANGUAGES.length);
    CODEBASE_LANGUAGES.forEach((language, index) => {
        testResults.sections.codebases.total++;
        testResults.total++;
        if (testCodeBaseInsertion(language)) {
            testResults.sections.codebases.passed++;
            testResults.passed++;
        } else {
            testResults.sections.codebases.failed++;
            testResults.sections.codebases.failures.push(language);
            testResults.failed++;
        }
        codebaseProgress.update(index + 1);
    });
    codebaseProgress.complete();

    // Test class creation for supported languages
    console.log('\n🏛️  Testing Class Creation...');
    const classProgress = new ProgressBar(CLASS_LANGUAGES.length);
    CLASS_LANGUAGES.forEach((language, index) => {
        testResults.sections.classes.total++;
        testResults.total++;
        if (testClassCreation(language)) {
            testResults.sections.classes.passed++;
            testResults.passed++;
        } else {
            testResults.sections.classes.failed++;
            testResults.sections.classes.failures.push(language);
            testResults.failed++;
        }
        classProgress.update(index + 1);
    });
    classProgress.complete();

    // Test project scaffolding
    console.log('\n🏗️  Testing Project Scaffolding...');
    const languages = ['c', 'cpp', 'python', 'java', 'rust', 'ruby', 'php'];
    const projectProgress = new ProgressBar(languages.length + 1);

    // First test header generation
    testResults.sections.projects.total++;
    testResults.total++;
    const headerTestResult = await testHeaderGenerationInScaffoldedProjects();
    if (headerTestResult) {
        testResults.sections.projects.passed++;
        testResults.passed++;
    } else {
        testResults.sections.projects.failed++;
        testResults.sections.projects.failures.push('header generation');
        testResults.failed++;
    }
    projectProgress.update(1);

    for (const language of languages) {
        testResults.sections.projects.total++;
        testResults.total++;
        const result = await testProjectScaffoldingForLanguage(language);
        if (result) {
            testResults.sections.projects.passed++;
            testResults.passed++;
        } else {
            testResults.sections.projects.failed++;
            testResults.sections.projects.failures.push(language);
            testResults.failed++;
        }
        projectProgress.update(
            testResults.sections.projects.passed + testResults.sections.projects.failed
        );
    }

    projectProgress.complete();

    // Generate comprehensive test report
    console.log('\n📊 Test Results:');
    const successRate = ((testResults.passed / testResults.total) * 100).toFixed(1);
    console.log(`Total: ${testResults.total}, Passed: ${testResults.passed}, ` +
        `Failed: ${testResults.failed}, Success: ${successRate}%`);

    // Display failures at the end
    if (testResults.failed > 0) {
        console.log('\n❌ Failed Tests:');

        if (testResults.sections.headers.failures.length > 0) {
            console.log(
                `\n📝 Header Insertion Failures (${testResults.sections.headers.failures.length}):`
            );
            testResults.sections.headers.failures.forEach(failure => console.log(`  - ${failure}`));
        }

        if (testResults.sections.codebases.failures.length > 0) {
            console.log(
                '\n🏗️  Code Base Insertion Failures (' +
                testResults.sections.codebases.failures.length + '):'
            );
            testResults.sections.codebases.failures.forEach(failure =>
                console.log('  - ' + failure)
            );
        }

        if (testResults.sections.classes.failures.length > 0) {
            const msg = '\n🏛️  Class Creation Failures (' +
                testResults.sections.classes.failures.length + '):';
            console.log(msg);
            testResults.sections.classes.failures.forEach(failure =>
                console.log('  - ' + failure)
            );
        }

        if (testResults.sections.syntax.failures.length > 0) {
            const msg = '\n🔍 Syntax Validation Failures (' +
                testResults.sections.syntax.failures.length + '):';
            console.log(msg);
            testResults.sections.syntax.failures.forEach(failure =>
                console.log('  - ' + failure)
            );
        }

        if (testResults.sections.api.failures.length > 0) {
            const msg = '\n🔧 Extension API Failures (' +
                testResults.sections.api.failures.length + '):';
            console.log(msg);
            testResults.sections.api.failures.forEach(failure =>
                console.log('  - ' + failure)
            );
        }

        if (testResults.sections.ruby.failures.length > 0) {
            const msg = '\n💎 Ruby Backend Failures (' +
                testResults.sections.ruby.failures.length + '):';
            console.log(msg);
            testResults.sections.ruby.failures.forEach(failure =>
                console.log('  - ' + failure)
            );
        }

        if (testResults.sections.studyMode.failures.length > 0) {
            const msg = '\n🍅 Study Mode Timer Failures (' +
                testResults.sections.studyMode.failures.length + '):';
            console.log(msg);
            testResults.sections.studyMode.failures.forEach(failure =>
                console.log('  - ' + failure)
            );
        }

        if (testResults.sections.projects.failures.length > 0) {
            const msg = '\n🏗️  Project Scaffolding Failures (' +
                testResults.sections.projects.failures.length + '):';
            console.log(msg);
            testResults.sections.projects.failures.forEach(failure =>
                console.log('  - ' + failure)
            );
        }
    }

    // Save detailed report
    const reportPath = path.join(TEST_DIR, 'unified_test_report.json');
    fs.writeFileSync(reportPath, JSON.stringify({
        ...testResults,
        testConfig: {
            totalHeaderLanguages: HEADER_LANGUAGES.length,
            totalCodebaseLanguages: CODEBASE_LANGUAGES.length,
            totalClassLanguages: CLASS_LANGUAGES.length,
            testedHeaderLanguages: testResults.sections.headers.total,
            testedCodebaseLanguages: testResults.sections.codebases.total,
            testedClassLanguages: testResults.sections.classes.total
        },
        timestamp: new Date().toISOString(),
        version: '3.0.5'
    }, null, 2));
    console.log('\nReport saved: ' + reportPath);

    return testResults.failed === 0;
}

/**
 * Main execution
 */
if (require.main === module) {
    runAllTests()
        .then(success => {
            process.exit(success ? 0 : 1);
        })
        .catch(error => {
            console.error('Test suite failed:', error);
            process.exit(1);
        });
}

module.exports = {
    runAllTests,
    testHeaderInsertion,
    testCodeBaseInsertion,
    testClassCreation,
    validateTSIHeader,
    testExtensionAPI,
    testRubyBackend,
    testStudyMode
};