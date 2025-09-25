/**
 * Code Base Generators Module
 * Contains language-specific code base generation logic
 */

// Import language-specific generators
const { generateCCodeBase } = require('./languages/c');
const { generateCppCodeBase } = require('./languages/cpp');
const { generatePythonCodeBase } = require('./languages/python');
const { generateJavaCodeBase } = require('./languages/java');
const { generateJavaScriptCodeBase } = require('./languages/javascript');
const { generateTypeScriptCodeBase } = require('./languages/typescript');

/**
 * Generates code base/boilerplate code for the specified language
 * @param {string} languageId - The language identifier
 * @param {string} fileName - The file name (for extension detection in plaintext)
 * @returns {object} Result object with success and content properties
 */
function generateCodeBase(languageId, fileName) {
    let content;
    try {
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
                if (typeof generateGoCodeBase === 'function') {
                    content = generateGoCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'php':
                if (typeof generatePhpCodeBase === 'function') {
                    content = generatePhpCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'swift':
                if (typeof generateSwiftCodeBase === 'function') {
                    content = generateSwiftCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'dart':
                content = generateDartCodeBase();
                break;
            case 'scala':
                content = generateScalaCodeBase();
                break;
            case 'sql':
                if (typeof generateSqlCodeBase === 'function') {
                    content = generateSqlCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'html':
                content = generateHtmlCodeBase();
                break;
            case 'delphi':
                if (typeof generateObjectPascalCodeBase === 'function') {
                    content = generateObjectPascalCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'pascal':
            case 'objectpascal':
                if (typeof generateObjectPascalCodeBase === 'function') {
                    content = generateObjectPascalCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'basic':
                if (typeof generateBasicCodeBase === 'function') {
                    content = generateBasicCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'fortran':
            case 'fortran90':
            case 'FortranFreeForm':
                content = generateFortranCodeBase();
                break;
            case 'r':
                if (typeof generateRCodeBase === 'function') {
                    content = generateRCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'matlab':
                if (typeof generateMatlabCodeBase === 'function') {
                    content = generateMatlabCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'assembly':
                if (typeof generateAssemblyCodeBase === 'function') {
                    content = generateAssemblyCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'cobol':
                if (typeof generateCobolCodeBase === 'function') {
                    content = generateCobolCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'prolog':
                content = generatePrologCodeBase();
                break;
            case 'makefile':
                if (typeof generateMakefileCodeBase === 'function') {
                    content = generateMakefileCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'javascript':
                content = generateJavaScriptCodeBase();
                break;
            case 'ada':
                if (typeof generateAdaCodeBase === 'function') {
                    content = generateAdaCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'apl':
                if (typeof generateAplCodeBase === 'function') {
                    content = generateAplCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'asm':
            case 'assembly':
                if (typeof generateAssemblyCodeBase === 'function') {
                    content = generateAssemblyCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'awk':
                if (typeof generateAwkCodeBase === 'function') {
                    content = generateAwkCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'bat':
            case 'batch':
                if (typeof generateBatchCodeBase === 'function') {
                    content = generateBatchCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'cfml':
            case 'coldfusion':
                if (typeof generateColdFusionCodeBase === 'function') {
                    content = generateColdFusionCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'clojure':
                if (typeof generateClojureCodeBase === 'function') {
                    content = generateClojureCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'coffeescript':
                if (typeof generateCoffeeScriptCodeBase === 'function') {
                    content = generateCoffeeScriptCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'css':
                if (typeof generateCssCodeBase === 'function') {
                    content = generateCssCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'dockerfile':
                if (typeof generateDockerfileCodeBase === 'function') {
                    content = generateDockerfileCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'elixir':
                if (typeof generateElixirCodeBase === 'function') {
                    content = generateElixirCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'erlang':
                if (typeof generateErlangCodeBase === 'function') {
                    content = generateErlangCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'factor':
                if (typeof generateFactorCodeBase === 'function') {
                    content = generateFactorCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'forth':
                if (typeof generateForthCodeBase === 'function') {
                    content = generateForthCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'fsharp':
                if (typeof generateFSharpCodeBase === 'function') {
                    content = generateFSharpCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'groovy':
                if (typeof generateGroovyCodeBase === 'function') {
                    content = generateGroovyCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'haskell':
            case 'Haskell':
                if (typeof generateHaskellCodeBase === 'function') {
                    content = generateHaskellCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'idl':
                if (typeof generateIdlCodeBase === 'function') {
                    content = generateIdlCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'ini':
                if (typeof generateIniCodeBase === 'function') {
                    content = generateIniCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'jade':
                if (typeof generateJadeCodeBase === 'function') {
                    content = generateJadeCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'javascriptreact':
                if (typeof generateJavaScriptReactCodeBase === 'function') {
                    content = generateJavaScriptReactCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'json':
                if (typeof generateJSONCodeBase === 'function') {
                    content = generateJSONCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'julia':
                if (typeof generateJuliaCodeBase === 'function') {
                    content = generateJuliaCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'latex':
                if (typeof generateLatexCodeBase === 'function') {
                    content = generateLatexCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'less':
                if (typeof generateLessCodeBase === 'function') {
                    content = generateLessCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'lisp':
                if (typeof generateLispCodeBase === 'function') {
                    content = generateLispCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'lua':
                if (typeof generateLuaCodeBase === 'function') {
                    content = generateLuaCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'markdown':
                if (typeof generateMarkdownCodeBase === 'function') {
                    content = generateMarkdownCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'maple':
                if (typeof generateMapleCodeBase === 'function') {
                    content = generateMapleCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'mathematica':
                if (typeof generateMathematicaCodeBase === 'function') {
                    content = generateMathematicaCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'mercury':
                if (typeof generateMercuryCodeBase === 'function') {
                    content = generateMercuryCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'objective-c':
                if (typeof generateObjectiveCCodeBase === 'function') {
                    content = generateObjectiveCCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'objective-cpp':
                if (typeof generateObjectiveCppCodeBase === 'function') {
                    content = generateObjectiveCppCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'ocaml':
                if (typeof generateOcamlCodeBase === 'function') {
                    content = generateOcamlCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'octave':
                if (typeof generateOctaveCodeBase === 'function') {
                    content = generateOctaveCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'perl':
            case 'perl6':
            case 'raku':
                if (typeof generatePerlCodeBase === 'function') {
                    content = generatePerlCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'postscript':
                if (typeof generatePostScriptCodeBase === 'function') {
                    content = generatePostScriptCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'powershell':
                if (typeof generatePowerShellCodeBase === 'function') {
                    content = generatePowerShellCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'scheme':
                if (typeof generateSchemeCodeBase === 'function') {
                    content = generateSchemeCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'scss':
                if (typeof generateScssCodeBase === 'function') {
                    content = generateScssCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'shellscript':
                if (typeof generateShellScriptCodeBase === 'function') {
                    content = generateShellScriptCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'smalltalk':
                if (typeof generateSmalltalkCodeBase === 'function') {
                    content = generateSmalltalkCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'solidity':
                if (typeof generateSolidityCodeBase === 'function') {
                    content = generateSolidityCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'tcl':
                if (typeof generateTclCodeBase === 'function') {
                    content = generateTclCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'typescriptreact':
                if (typeof generateTypeScriptReactCodeBase === 'function') {
                    content = generateTypeScriptReactCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'vb':
                if (typeof generateVbCodeBase === 'function') {
                    content = generateVbCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'vbscript':
                if (typeof generateVBScriptCodeBase === 'function') {
                    content = generateVBScriptCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'verilog':
            case 'systemverilog':
            case 'Verilog':
                if (typeof generateVerilogCodeBase === 'function') {
                    content = generateVerilogCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'vhdl':
                if (typeof generateVhdlCodeBase === 'function') {
                    content = generateVhdlCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'vue':
                if (typeof generateVueCodeBase === 'function') {
                    content = generateVueCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'xml':
                if (typeof generateXmlCodeBase === 'function') {
                    content = generateXmlCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'yaml':
            case 'yml':
                if (typeof generateYamlCodeBase === 'function') {
                    content = generateYamlCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'kotlin':
                if (typeof generateKotlinCodeBase === 'function') {
                    content = generateKotlinCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'typescript':
                content = generateTypeScriptCodeBase();
                break;
            case 'plaintext':
                // Handle plaintext files (may be detected incorrectly by VS Code)
                const fileExtension = fileName.split('.').pop().toLowerCase();
                
                // Ada
                if (fileExtension === 'adb' || fileExtension === 'ads') {
                    content = typeof generateAdaCodeBase === 'function' ? generateAdaCodeBase() : generateDefaultCodeBase(languageId);
                // APL
                } else if (fileExtension === 'apl') {
                    content = typeof generateAplCodeBase === 'function' ? generateAplCodeBase() : generateDefaultCodeBase(languageId);
                // Assembly
                } else if (fileExtension === 'asm') {
                    content = typeof generateAssemblyCodeBase === 'function' ? generateAssemblyCodeBase() : generateDefaultCodeBase(languageId);
                // AWK
                } else if (fileExtension === 'awk') {
                    content = typeof generateAwkCodeBase === 'function' ? generateAwkCodeBase() : generateDefaultCodeBase(languageId);
                // Basic
                } else if (fileExtension === 'bas') {
                    content = typeof generateBasicCodeBase === 'function' ? generateBasicCodeBase() : generateDefaultCodeBase(languageId);
                // Batch
                } else if (fileExtension === 'bat') {
                    content = typeof generateBatchCodeBase === 'function' ? generateBatchCodeBase() : generateDefaultCodeBase(languageId);
                // ColdFusion
                } else if (fileExtension === 'cfm') {
                    content = typeof generateColdFusionCodeBase === 'function' ? generateColdFusionCodeBase() : generateDefaultCodeBase(languageId);
                // Clojure
                } else if (fileExtension === 'clj') {
                    content = typeof generateClojureCodeBase === 'function' ? generateClojureCodeBase() : generateDefaultCodeBase(languageId);
                // COBOL
                } else if (fileExtension === 'cob') {
                    content = typeof generateCobolCodeBase === 'function' ? generateCobolCodeBase() : generateDefaultCodeBase(languageId);
                // CoffeeScript
                } else if (fileExtension === 'coffee') {
                    content = typeof generateCoffeeScriptCodeBase === 'function' ? generateCoffeeScriptCodeBase() : generateDefaultCodeBase(languageId);
                // Docker
                } else if (fileExtension === 'dockerfile') {
                    content = typeof generateDockerfileCodeBase === 'function' ? generateDockerfileCodeBase() : generateDefaultCodeBase(languageId);
                // Elixir
                } else if (fileExtension === 'ex') {
                    content = typeof generateElixirCodeBase === 'function' ? generateElixirCodeBase() : generateDefaultCodeBase(languageId);
                // Erlang
                } else if (fileExtension === 'erl') {
                    content = typeof generateErlangCodeBase === 'function' ? generateErlangCodeBase() : generateDefaultCodeBase(languageId);
                // Factor
                } else if (fileExtension === 'factor') {
                    content = typeof generateFactorCodeBase === 'function' ? generateFactorCodeBase() : generateDefaultCodeBase(languageId);
                // Forth
                } else if (fileExtension === 'fth') {
                    content = typeof generateForthCodeBase === 'function' ? generateForthCodeBase() : generateDefaultCodeBase(languageId);
                // Fortran
                } else if (fileExtension === 'f90' || fileExtension === 'f95' || fileExtension === 'f03') {
                    content = typeof generateFortranCodeBase === 'function' ? generateFortranCodeBase() : generateDefaultCodeBase(languageId);
                // F#
                } else if (fileExtension === 'fs') {
                    content = typeof generateFSharpCodeBase === 'function' ? generateFSharpCodeBase() : generateDefaultCodeBase(languageId);
                // Groovy
                } else if (fileExtension === 'groovy') {
                    content = typeof generateGroovyCodeBase === 'function' ? generateGroovyCodeBase() : generateDefaultCodeBase(languageId);
                // Haskell
                } else if (fileExtension === 'hs' || fileExtension === 'lhs') {
                    content = typeof generateHaskellCodeBase === 'function' ? generateHaskellCodeBase() : generateDefaultCodeBase(languageId);
                // IDL
                } else if (fileExtension === 'idl') {
                    content = typeof generateIdlCodeBase === 'function' ? generateIdlCodeBase() : generateDefaultCodeBase(languageId);
                // INI
                } else if (fileExtension === 'ini') {
                    content = typeof generateIniCodeBase === 'function' ? generateIniCodeBase() : generateDefaultCodeBase(languageId);
                // Jade
                } else if (fileExtension === 'jade') {
                    content = typeof generateJadeCodeBase === 'function' ? generateJadeCodeBase() : generateDefaultCodeBase(languageId);
                // Julia
                } else if (fileExtension === 'jl') {
                    content = typeof generateJuliaCodeBase === 'function' ? generateJuliaCodeBase() : generateDefaultCodeBase(languageId);
                // Kotlin
                } else if (fileExtension === 'kt') {
                    content = typeof generateKotlinCodeBase === 'function' ? generateKotlinCodeBase() : generateDefaultCodeBase(languageId);
                // LaTeX
                } else if (fileExtension === 'tex') {
                    content = typeof generateLatexCodeBase === 'function' ? generateLatexCodeBase() : generateDefaultCodeBase(languageId);
                // Less
                } else if (fileExtension === 'less') {
                    content = typeof generateLessCodeBase === 'function' ? generateLessCodeBase() : generateDefaultCodeBase(languageId);
                // Lisp
                } else if (fileExtension === 'lisp') {
                    content = typeof generateLispCodeBase === 'function' ? generateLispCodeBase() : generateDefaultCodeBase(languageId);
                // Lua
                } else if (fileExtension === 'lua') {
                    content = typeof generateLuaCodeBase === 'function' ? generateLuaCodeBase() : generateDefaultCodeBase(languageId);
                // Maple
                } else if (fileExtension === 'mpl') {
                    content = typeof generateMapleCodeBase === 'function' ? generateMapleCodeBase() : generateDefaultCodeBase(languageId);
                // MATLAB/Mathematica/Mercury/Objective-C (.m is ambiguous)
                } else if (fileExtension === 'm') {
                    // Try to detect based on file content or default to Objective-C
                    content = typeof generateObjectiveCCodeBase === 'function' ? generateObjectiveCCodeBase() : generateDefaultCodeBase(languageId);
                // Objective-C++
                } else if (fileExtension === 'mm') {
                    content = typeof generateObjectiveCppCodeBase === 'function' ? generateObjectiveCppCodeBase() : generateDefaultCodeBase(languageId);
                // Objective-J
                } else if (fileExtension === 'j') {
                    content = typeof generateObjectiveJCodeBase === 'function' ? generateObjectiveJCodeBase() : generateDefaultCodeBase(languageId);
                // OCaml
                } else if (fileExtension === 'ml') {
                    content = typeof generateOcamlCodeBase === 'function' ? generateOcamlCodeBase() : generateDefaultCodeBase(languageId);
                // Pascal/Delphi/ObjectPascal
                } else if (fileExtension === 'pas') {
                    content = typeof generateObjectPascalCodeBase === 'function' ? generateObjectPascalCodeBase() : generateDefaultCodeBase(languageId);
                // Perl/Prolog (.pl is ambiguous)
                } else if (fileExtension === 'pl') {
                    content = typeof generatePerlCodeBase === 'function' ? generatePerlCodeBase() : generateDefaultCodeBase(languageId);
                // Perl6
                } else if (fileExtension === 'pl6') {
                    content = typeof generatePerlCodeBase === 'function' ? generatePerlCodeBase() : generateDefaultCodeBase(languageId);
                // Raku
                } else if (fileExtension === 'raku') {
                    content = typeof generatePerlCodeBase === 'function' ? generatePerlCodeBase() : generateDefaultCodeBase(languageId);
                // PostScript
                } else if (fileExtension === 'ps') {
                    content = typeof generatePostScriptCodeBase === 'function' ? generatePostScriptCodeBase() : generateDefaultCodeBase(languageId);
                // PowerShell
                } else if (fileExtension === 'ps1') {
                    content = typeof generatePowerShellCodeBase === 'function' ? generatePowerShellCodeBase() : generateDefaultCodeBase(languageId);
                // Scala
                } else if (fileExtension === 'scala') {
                    content = typeof generateScalaCodeBase === 'function' ? generateScalaCodeBase() : generateDefaultCodeBase(languageId);
                // Scheme
                } else if (fileExtension === 'scm') {
                    content = typeof generateSchemeCodeBase === 'function' ? generateSchemeCodeBase() : generateDefaultCodeBase(languageId);
                // Sed
                } else if (fileExtension === 'sed') {
                    content = typeof generateSedCodeBase === 'function' ? generateSedCodeBase() : generateDefaultCodeBase(languageId);
                // Smalltalk
                } else if (fileExtension === 'st') {
                    content = typeof generateSmalltalkCodeBase === 'function' ? generateSmalltalkCodeBase() : generateDefaultCodeBase(languageId);
                // Solidity
                } else if (fileExtension === 'sol') {
                    content = typeof generateSolidityCodeBase === 'function' ? generateSolidityCodeBase() : generateDefaultCodeBase(languageId);
                // SQL
                } else if (fileExtension === 'sql') {
                    content = typeof generateSqlCodeBase === 'function' ? generateSqlCodeBase() : generateDefaultCodeBase(languageId);
                // TCL
                } else if (fileExtension === 'tcl') {
                    content = typeof generateTclCodeBase === 'function' ? generateTclCodeBase() : generateDefaultCodeBase(languageId);
                // Verilog
                } else if (fileExtension === 'v' || fileExtension === 'vh' || fileExtension === 'sv') {
                    content = typeof generateVerilogCodeBase === 'function' ? generateVerilogCodeBase() : generateDefaultCodeBase(languageId);
                // VHDL
                } else if (fileExtension === 'vhdl') {
                    content = typeof generateVhdlCodeBase === 'function' ? generateVhdlCodeBase() : generateDefaultCodeBase(languageId);
                // Verse
                } else if (fileExtension === 'verse') {
                    content = typeof generateVerseCodeBase === 'function' ? generateVerseCodeBase() : generateDefaultCodeBase(languageId);
                // Vimscript
                } else if (fileExtension === 'vim') {
                    content = typeof generateVimscriptCodeBase === 'function' ? generateVimscriptCodeBase() : generateDefaultCodeBase(languageId);
                // SAS
                } else if (fileExtension === 'sas') {
                    content = typeof generateSasCodeBase === 'function' ? generateSasCodeBase() : generateDefaultCodeBase(languageId);
                } else {
                    // Default to C for other plaintext files
                    content = generateCCodeBase();
                }
                break;
            case 'xsl':
                if (typeof generateXslCodeBase === 'function') {
                    content = generateXslCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'verse':
                if (typeof generateVerseCodeBase === 'function') {
                    content = generateVerseCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'vimscript':
                if (typeof generateVimscriptCodeBase === 'function') {
                    content = generateVimscriptCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'sed':
                if (typeof generateSedCodeBase === 'function') {
                    content = generateSedCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'sas':
                if (typeof generateSasCodeBase === 'function') {
                    content = generateSasCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'objective-j':
                if (typeof generateObjectiveJCodeBase === 'function') {
                    content = generateObjectiveJCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            default:
                // Default to C for unsupported languages
                content = generateDefaultCodeBase(languageId);
                break;
        }
    } catch (error) {
        // If any error occurs, use default code base
        content = generateDefaultCodeBase(languageId);
    }
    return { success: true, content: content };
}

/**
 * Generates C# code base
 */
function generateCSharpCodeBase() {
    return `\nusing System;\n\nclass Program\n{\n    static void Main(string[] args)\n    {\n        Console.WriteLine("Hello, World!");\n        Console.WriteLine("This is a basic C# program.");\n    }\n}\n`;
}

/**
 * Generates Ruby code base
 */
function generateRubyCodeBase() {
    return `\n#!/usr/bin/env ruby\n\n# Basic Ruby program\n\n# Main function - entry point of the program\ndef main\n  puts "Hello, World!"\n  puts "This is a basic Ruby program."\nend\n\n# Execute main function\nmain\n\n# Alternative direct approach\n# puts "Hello, World!"\n# puts "This is a basic Ruby program."\n\n# Example class definition\n# class Greeter\n#   def initialize(name = "World")\n#     @name = name\n#   end\n#   \n#   def greet\n#     "Hello, #{@name}!"\n#   end\n# end\n\n# Example usage\n# greeter = Greeter.new("TSI Student")\n# puts greeter.greet\n\n# Example with arrays and hashes\n# languages = ["Ruby", "Python", "JavaScript"]\n# config = {\n#   version: "1.0",\n#   debug: true,\n#   author: "TSI Student"\n# }\n\n# languages.each do |lang|\n#   puts "Language: #{lang}"\n# end\n\n# puts "Version: #{config[:version]}"\n`;
}

/**
 * Generates Rust code base
 */
function generateRustCodeBase() {
    return `\n//! Basic Rust program\n\n/// Main function - entry point of the program\nfn main() {\n    println!("Hello, World!");\n    println!("This is a basic Rust program.");\n}\n\n/// Example function with parameters\nfn greet(name: &str) -> String {\n    format!("Hello, {}!", name)\n}\n\n/// Example struct definition\n#[derive(Debug)]\nstruct Person {\n    name: String,\n    age: u32,\n}\n\nimpl Person {\n    /// Create a new Person\n    fn new(name: String, age: u32) -> Self {\n        Person { name, age }\n    }\n\n    /// Display person information\n    fn display(&self) {\n        println!("Name: {}, Age: {}", self.name, self.age);\n    }\n}\n\n/// Example usage (uncomment to use)\n/*\nfn main() {\n    // Basic greeting\n    println!("Hello, World!");\n    println!("This is a basic Rust program.");\n\n    // Function usage\n    let greeting = greet("TSI Student");\n    println!("{}", greeting);\n\n    // Struct usage\n    let person = Person::new("TSI Student".to_string(), 20);\n    person.display();\n    println!("Person: {:?}", person);\n}\n*/\n\n#[cfg(test)]\nmod tests {\n    use super::*;\n\n    #[test]\n    fn test_greet() {\n        assert_eq!(greet("World"), "Hello, World!");\n    }\n\n    #[test]\n    fn test_person() {\n        let person = Person::new("Test".to_string(), 25);\n        assert_eq!(person.name, "Test");\n        assert_eq!(person.age, 25);\n    }\n}\n`;
}
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
    return `
// Basic Groovy program

// Main method (optional in Groovy)
def main(args) {
    println "Hello, World!"
    println "This is a basic Groovy script."
}

// Groovy can also run without main method
// println "Hello, World!"
// println "This is a basic Groovy script."

// Call main method
main(args)

// Example of Groovy features
def greet(name = "World") {
    "Hello, " + name + "!"
}

// println greet("TSI Student")
`;
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
 * Generates JSON code base
 */
function generateJSONCodeBase() {
    return `{
  "name": "TSI Header Application",
  "version": "1.0.0",
  "description": "Basic JSON template for configuration or data structure",
  "main": "index.js",
  "scripts": {
    "start": "node index.js",
    "test": "npm test"
  },
  "author": "TSI Student",
  "license": "MIT",
  "dependencies": {},
  "devDependencies": {},
  "keywords": [
    "tsi",
    "template",
    "configuration"
  ],
  "repository": {
    "type": "git",
    "url": "https://github.com/example/tsi-project"
  },
  "config": {
    "environment": "development",
    "debug": true,
    "settings": {
      "theme": "default",
      "language": "en"
    }
  },
  "data": {
    "message": "Hello, World!",
    "timestamp": "2024-01-01T00:00:00.000Z",
    "status": "active"
  }
}`;
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
 * Generates VB.NET code base
 */
function generateVbCodeBase() {
    return `\n' Basic VB.NET program\n\nImports System\n\nModule HelloWorld\n    ' Main subroutine - entry point of the program\n    Sub Main()\n        Console.WriteLine("Hello, World!")\n        Console.WriteLine("This is a basic VB.NET program.")\n        \n        ' Wait for user input\n        Console.ReadLine()\n    End Sub\n    \n    ' Example function\n    ' Function Greet(name As String) As String\n    '     If String.IsNullOrEmpty(name) Then\n    '         name = "World"\n    '     End If\n    '     Return "Hello, " & name & "!"\n    ' End Function\n    \n    ' Example usage\n    ' Console.WriteLine(Greet("TSI Student"))\nEnd Module\n\n' Alternative class-based approach\n' Public Class HelloWorldClass\n'     Public Shared Sub Main(args() As String)\n'         Console.WriteLine("Hello, World!")\n'         Console.WriteLine("This is a basic VB.NET program.")\n'     End Sub\n' End Class\n`;
}

/**
 * Generates VBScript code base
 */
function generateVBScriptCodeBase() {
    return `\n' Basic VBScript program\n\n' Main subroutine - entry point of the program\nSub Main()\n    MsgBox "Hello, World!"\n    MsgBox "This is a basic VBScript program."\n    \n    ' Alternative console output (for WSH)\n    ' WScript.Echo "Hello, World!"\n    ' WScript.Echo "This is a basic VBScript program."\nEnd Sub\n\n' Execute main subroutine\nMain()\n\n' Example function with parameters\n' Function Greet(name)\n'     If IsEmpty(name) Or name = "" Then\n'         name = "World"\n'     End If\n'     Greet = "Hello, " & name & "!"\n' End Function\n\n' Example usage\n' MsgBox Greet("TSI Student")\n' WScript.Echo Greet("TSI Student")\n\n' Example with variables and conditional logic\n' Dim message\n' message = "Hello from TSI!"\n' If message <> "" Then\n'     MsgBox message\n' End If\n\n' Example with loops\n' Dim i\n' For i = 1 To 3\n'     MsgBox "Count: " & i\n' Next\n`;
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

/**
 * Generates Markdown code base
 */
function generateMarkdownCodeBase() {
    return `\n# Hello, World!\n\nThis is a basic Markdown document.\n\n## Welcome to TSI\n\n**Transport and Telecommunication Institute** - Riga, Latvia\n\n### Features\n\n- Professional education\n- Modern technology\n- International standards\n\n### Example Code Block\n\n\`\`\`javascript\nconsole.log("Hello, World!");\nconsole.log("This is a basic program.");\n\`\`\`\n\n### Links\n\n- [TSI Website](https://tsi.lv)\n- [Programming Courses](https://tsi.lv/courses)\n\n### Contact\n\n**Email:** info@tsi.lv  \n**Address:** Riga, Latvia\n\n---\n\n*Created by TSI Student*\n`;
}

/**
 * Generates Verilog code base
 */
function generateVerilogCodeBase() {
    return `\n// Basic Verilog module\n\nmodule hello_world(\n    input wire clk,\n    input wire reset,\n    output reg [7:0] data_out\n);\n\n// Internal registers\nreg [31:0] counter;\n\n// Main logic\nalways @(posedge clk or posedge reset) begin\n    if (reset) begin\n        counter <= 32'h0;\n        data_out <= 8'h0;\n    end else begin\n        counter <= counter + 1;\n        data_out <= counter[7:0];\n    end\nend\n\n// Example combinational logic\nalways @(*) begin\n    // Add combinational logic here\n    // data_out = some_function(inputs);\nend\n\n// Example initial block for simulation\ninitial begin\n    $display("Hello, World!");\n    $display("This is a basic Verilog module.");\nend\n\nendmodule\n\n// Testbench (uncomment for simulation)\n/*\nmodule hello_world_tb;\n\nreg clk, reset;\nwire [7:0] data_out;\n\n// Instantiate the module\nhello_world uut (\n    .clk(clk),\n    .reset(reset),\n    .data_out(data_out)\n);\n\n// Clock generation\nalways #5 clk = ~clk;\n\n// Test sequence\ninitial begin\n    clk = 0;\n    reset = 1;\n    #10 reset = 0;\n    #100 $finish;\nend\n\ninitial begin\n    $monitor("Time=%0t, data_out=%h", $time, data_out);\nend\n\nendmodule\n*/\n`;
}

/**
 * Generates Kotlin code base
 */
function generateKotlinCodeBase() {
    return `\n// Basic Kotlin program\n\n// Main function - entry point of the program\nfun main() {\n    println("Hello, World!")\n    println("This is a basic Kotlin program.")\n}\n\n// Alternative direct approach\n// println("Hello, World!")\n// println("This is a basic Kotlin program.")\n\n// Example function with parameters\nfun greet(name: String = "World"): String {\n    return "Hello, \$name!"\n}\n\n// Example usage\n// println(greet("TSI Student"))\n\n// Example class definition\nclass TSIStudent(val name: String, val program: String) {\n    fun introduce() {\n        println("Hello, I'm \$name from TSI!")\n    }\n}\n\n// Example usage\n// val student = TSIStudent("TSI Student", "Computer Science")\n// student.introduce()\n\n// Example with collections\n// val languages = listOf("Kotlin", "Java", "Python")\n// val config = mapOf(\n//     "version" to "1.0",\n//     "debug" to true,\n//     "author" to "TSI Student"\n// )\n\n// languages.forEach { lang ->\n//     println("Language: \$lang")\n// }\n\n// println("Version: \${config["version"]}")\n`;
}

/**
 * Generates Go code base
 */
function generateGoCodeBase() {
    return `\npackage main\n\nimport "fmt"\n\n// Main function - entry point of the program\nfunc main() {\n    fmt.Println("Hello, World!")\n    fmt.Println("This is a basic Go program.")\n}\n\n// Example function with parameters\nfunc greet(name string) string {\n    return "Hello, " + name + "!"\n}\n\n// Example struct definition\ntype Person struct {\n    Name string\n    Age  int\n}\n\n// Example method\nfunc (p Person) Introduce() {\n    fmt.Printf("Hello, I'm %s and I'm %d years old!\\n", p.Name, p.Age)\n}\n\n// Example usage (uncomment to use)\n/*\nfunc main() {\n    // Basic greeting\n    fmt.Println("Hello, World!")\n    fmt.Println("This is a basic Go program.")\n\n    // Function usage\n    greeting := greet("TSI Student")\n    fmt.Println(greeting)\n\n    // Struct usage\n    person := Person{Name: "TSI Student", Age: 20}\n    person.Introduce()\n}\n*/\n`;
}

/**
 * Generates PHP code base
 */
function generatePhpCodeBase() {
    return `\n<?php\n\n/**\n * Main function - entry point of the program\n */\nfunction main() {\n    echo "Hello, World!\\n";\n    echo "This is a basic PHP script.\\n";\n}\n\n// Execute main function\nmain();\n\n// Alternative direct approach\n// echo "Hello, World!\\n";\n// echo "This is a basic PHP script.\\n";\n\n/**\n * Example function with parameters\n */\nfunction greet($name = "World") {\n    return "Hello, " . $name . "!";\n}\n\n// Example usage\n// echo greet("TSI Student") . "\\n";\n\n/**\n * Example class definition\n */\nclass TSIStudent {\n    private $name;\n    private $program;\n    \n    public function __construct($name, $program) {\n        $this->name = $name;\n        $this->program = $program;\n    }\n    \n    public function introduce() {\n        echo "Hello, I'm " . $this->name . " from TSI!\\n";\n    }\n}\n\n// Example usage\n// $student = new TSIStudent("TSI Student", "Computer Science");\n// $student->introduce();\n\n?>\n`;
}

/**
 * Generates Swift code base
 */
function generateSwiftCodeBase() {
    return `\n// Basic Swift program\n\n// Main function - entry point of the program\nfunc main() {\n    print("Hello, World!")\n    print("This is a basic Swift program.")\n}\n\n// Execute main function\nmain()\n\n// Alternative direct approach\n// print("Hello, World!")\n// print("This is a basic Swift program.")\n\n// Example function with parameters\nfunc greet(name: String = "World") -> String {\n    return "Hello, \\(name)!"\n}\n\n// Example usage\n// print(greet(name: "TSI Student"))\n\n// Example class definition\nclass TSIStudent {\n    var name: String\n    var program: String\n    \n    init(name: String, program: String) {\n        self.name = name\n        self.program = program\n    }\n    \n    func introduce() {\n        print("Hello, I'm \\(name) from TSI!")\n    }\n}\n\n// Example usage\n// let student = TSIStudent(name: "TSI Student", program: "Computer Science")\n// student.introduce()\n\n// Example with arrays and optionals\n// let languages = ["Swift", "Objective-C", "Python"]\n// var optionalName: String? = "TSI Student"\n// if let name = optionalName {\n//     print("Hello, \\(name)!")\n// }\n`;
}

/**
 * Generates Dart code base
 */
function generateDartCodeBase() {
    return `\n// Basic Dart program\n\n// Main function - entry point of the program\nvoid main() {\n  print('Hello, World!');\n  print('This is a basic Dart program.');\n}\n\n// Alternative direct approach\n// void main() => print('Hello, World!');\n\n// Example function with parameters\nString greet(String name) {\n  return 'Hello, $name!';\n}\n\n// Example usage\n// void main() {\n//   print(greet('TSI Student'));\n// }\n\n// Example class definition\nclass TSIStudent {\n  String name;\n  String program;\n  \n  TSIStudent(this.name, this.program);\n  \n  void introduce() {\n    print('Hello, I\\'m $name from TSI!');\n  }\n}\n\n// Example usage\n// void main() {\n//   var student = TSIStudent('TSI Student', 'Computer Science');\n//   student.introduce();\n// }\n\n// Example with collections\n// void main() {\n//   var languages = ['Dart', 'Flutter', 'JavaScript'];\n//   var config = {\n//     'version': '1.0',\n//     'debug': true,\n//     'author': 'TSI Student'\n//   };\n//   \n//   languages.forEach((lang) => print('Language: $lang'));\n//   print('Version: \${config['version']}');\n// }\n`;
}

/**
 * Generates Scala code base
 */
function generateScalaCodeBase() {
    return `\n// Basic Scala program\n\n// Main object - entry point of the program\nobject HelloWorld {\n  def main(args: Array[String]): Unit = {\n    println("Hello, World!")\n    println("This is a basic Scala program.")\n  }\n}\n\n// Alternative direct approach\n// println("Hello, World!")\n// println("This is a basic Scala program.")\n\n// Example function with parameters\ndef greet(name: String = "World"): String = {\n  s"Hello, \$name!"\n}\n\n// Example usage\n// println(greet("TSI Student"))\n\n// Example class definition\nclass TSIStudent(val name: String, val program: String) {\n  def introduce(): Unit = {\n    println(s"Hello, I'm \$name from TSI!")\n  }\n}\n\n// Example usage\n// val student = new TSIStudent("TSI Student", "Computer Science")\n// student.introduce()\n\n// Example with collections\n// val languages = List("Scala", "Java", "Python")\n// val config = Map(\n//   "version" -> "1.0",\n//   "debug" -> true,\n//   "author" -> "TSI Student"\n// )\n\n// languages.foreach(lang => println(s"Language: \$lang"))\n// println(s"Version: \${config("version")}")\n`;
}

/**
 * Generates Assembly code base
 */
function generateAssemblyCodeBase() {
    return `\n; Basic Assembly program (x86-64 Linux)\n; Note: This is a basic assembly program template\n\nsection .data\n    hello db 'Hello, World!', 0xA  ; String with newline\n    hello_len equ $ - hello         ; Length of string\n    \n    message db 'This is a basic Assembly program.', 0xA\n    message_len equ $ - message\n\nsection .text\n    global _start\n\n_start:\n    ; Write "Hello, World!" to stdout\n    mov rax, 1          ; syscall: write\n    mov rdi, 1          ; file descriptor: stdout\n    mov rsi, hello      ; pointer to string\n    mov rdx, hello_len  ; length of string\n    syscall\n    \n    ; Write message to stdout\n    mov rax, 1          ; syscall: write\n    mov rdi, 1          ; file descriptor: stdout\n    mov rsi, message    ; pointer to message\n    mov rdx, message_len ; length of message\n    syscall\n    \n    ; Exit program\n    mov rax, 60         ; syscall: exit\n    xor rdi, rdi        ; exit code 0\n    syscall\n\n; Example function (uncomment to use)\n; greet:\n;     ; Function to print greeting\n;     ; Parameters would be passed in registers\n;     ret\n`;
}

/**
 * Generates COBOL code base
 */
function generateCobolCodeBase() {
    return `\n      * Basic COBOL program\n       IDENTIFICATION DIVISION.\n       PROGRAM-ID. HELLO-WORLD.\n       AUTHOR. TSI-STUDENT.\n       \n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  WS-MESSAGE         PIC X(50) VALUE 'Hello, World!'.\n       01  WS-PROGRAM         PIC X(50) VALUE 'This is a basic COBOL program.'.\n       \n       PROCEDURE DIVISION.\n       MAIN-PROCEDURE.\n           DISPLAY WS-MESSAGE.\n           DISPLAY WS-PROGRAM.\n           \n           STOP RUN.\n           \n       END PROGRAM HELLO-WORLD.\n\n      * Example subroutine (uncomment to use)\n      * IDENTIFICATION DIVISION.\n      * PROGRAM-ID. GREET.\n      * \n      * DATA DIVISION.\n      * LINKAGE SECTION.\n      * 01  LS-NAME            PIC X(20).\n      * \n      * PROCEDURE DIVISION USING LS-NAME.\n      *     DISPLAY 'Hello, ' LS-NAME '!'.\n      *     EXIT PROGRAM.\n      * END PROGRAM GREET.\n`;
}

/**
 * Generates Prolog code base
 */
function generatePrologCodeBase() {
    return `\n% Basic Prolog program\n\n% Main predicate - entry point\nhello_world :- \n    write('Hello, World!'), nl,\n    write('This is a basic Prolog program.'), nl.\n\n% Execute main predicate\n:- hello_world.\n\n% Alternative direct approach\n% :- write('Hello, World!'), nl,\n%    write('This is a basic Prolog program.'), nl.\n\n% Example predicate with parameters\n% greet(Name) :-\n%     write('Hello, '),\n%     write(Name),\n%     write('!'), nl.\n\n% Example usage\n% :- greet('TSI Student').\n\n% Example with facts and rules\n% student(tsi_student, computer_science).\n% student(jane_doe, electrical_engineering).\n% \n% is_computer_science_student(Name) :-\n%     student(Name, computer_science).\n% \n% Example query\n% ?- is_computer_science_student(tsi_student).\n% ?- student(Name, Program).\n`;
}

/**
 * Generates Makefile code base
 */
function generateMakefileCodeBase() {
    return `\n# Basic Makefile template\n\n# Compiler and flags\nCC = gcc\nCFLAGS = -Wall -Wextra -std=c99 -O2\nLDFLAGS = \n\n# Target executable\nTARGET = hello_world\n\n# Source files\nSRCS = main.c utils.c\n\n# Object files\nOBJS = $(SRCS:.c=.o)\n\n# Default target\nall: $(TARGET)\n\n# Link object files to create executable\n$(TARGET): $(OBJS)\n\t$(CC) $(LDFLAGS) -o $@ $^ \n\n# Compile source files to object files\n%.o: %.c\n\t$(CC) $(CFLAGS) -c $< -o $@\n\n# Clean build artifacts\nclean:\n\trm -f $(OBJS) $(TARGET)\n\n# Run the program\nrun: $(TARGET)\n\t./$(TARGET)\n\n# Debug build\ndebug: CFLAGS += -g -DDEBUG\ndebug: clean all\n\n# Install (example)\ninstall: $(TARGET)\n\tinstall -m 755 $(TARGET) /usr/local/bin/\n\n# Uninstall\nuninstall:\n\trm -f /usr/local/bin/$(TARGET)\n\n# Help target\nhelp:\n\t@echo "Available targets:"\n\t@echo "  all      - Build the program (default)"\n\t@echo "  clean    - Remove build artifacts"\n\t@echo "  run      - Build and run the program"\n\t@echo "  debug    - Build with debug symbols"\n\t@echo "  install  - Install the program"\n\t@echo "  help     - Show this help"\n\n.PHONY: all clean run debug install uninstall help\n`;
}

/**
 * Generates Basic code base
 */
function generateBasicCodeBase() {
    return `\n' Basic BASIC program\n\n' Main program - entry point\nPRINT "Hello, World!"\nPRINT "This is a basic BASIC program."\n\n' Example subroutine\n' SUB Greet(name$)\n'     PRINT "Hello, "; name$; "!"\n' END SUB\n\n' Example usage\n' CALL Greet("TSI Student")\n\n' Example with variables and loops\n' DIM languages$(3)\n' languages$(1) = "BASIC"\n' languages$(2) = "Pascal"\n' languages$(3) = "C"\n' \n' FOR i = 1 TO 3\n'     PRINT "Language: "; languages$(i)\n' NEXT i\n\nEND\n`;
}

/**
 * Generates Fortran code base
 */
function generateFortranCodeBase() {
    return `\n! Basic Fortran program\n\nPROGRAM HelloWorld\n    implicit none\n    \n    ! Variable declarations\n    character(len=50) :: message\n    character(len=50) :: program_desc\n    \n    ! Initialize variables\n    message = 'Hello, World!'\n    program_desc = 'This is a basic Fortran program.'\n    \n    ! Print messages\n    write(*,*) trim(message)\n    write(*,*) trim(program_desc)\n    \nEND PROGRAM HelloWorld\n\n! Alternative main program\n! program main\n!     write(*,*) 'Hello, World!'\n!     write(*,*) 'This is a basic Fortran program.'\n! end program main\n\n! Example subroutine (uncomment to use)\n! subroutine greet(name)\n!     character(len=*), intent(in) :: name\n!     write(*,*) 'Hello, ', trim(name), '!'\n! end subroutine greet\n\n! Example usage\n! call greet('TSI Student')\n\n! Example with arrays\n! program array_example\n!     implicit none\n!     integer :: i\n!     real, dimension(5) :: numbers = (/1.0, 2.0, 3.0, 4.0, 5.0/)\n!     \n!     do i = 1, 5\n!         write(*,*) 'Number:', numbers(i)\n!     end do\n! end program array_example\n`;
}

/**
 * Generates R code base
 */
function generateRCodeBase() {
    return `\n# Basic R program\n\n# Main function - entry point of the program\nmain <- function() {\n  cat("Hello, World!\\n")\n  cat("This is a basic R script.\\n")\n}\n\n# Execute main function\nmain()\n\n# Alternative direct approach\n# cat("Hello, World!\\n")\n# cat("This is a basic R script.\\n")\n\n# Example function with parameters\ngreet <- function(name = "World") {\n  return(paste("Hello,", name, "!"))\n}\n\n# Example usage\n# cat(greet("TSI Student"), "\\n")\n\n# Example with vectors and data frames\n# languages <- c("R", "Python", "JavaScript")\n# versions <- c(4.2, 3.9, 16)\n# \n# df <- data.frame(\n#   language = languages,\n#   version = versions\n# )\n# \n# print(df)\n# \n# # Plot example (uncomment to use)\n# # plot(df$language, df$version, \n# #      main = "Language Versions",\n# #      xlab = "Language", \n# #      ylab = "Version")\n\n# Example statistical operations\n# data <- rnorm(100, mean = 0, sd = 1)  # Generate random data\n# summary(data)  # Summary statistics\n# hist(data)     # Histogram\n`;
}

/**
 * Generates MATLAB code base
 */
function generateMatlabCodeBase() {
    return `\n% Basic MATLAB program\n\n% Main script - entry point\nfprintf('Hello, World!\\n');\nfprintf('This is a basic MATLAB script.\\n');\n\n% Example function definition (save as separate greet.m file)\n% function greet(name)\n%     if nargin < 1\n%         name = 'World';\n%     end\n%     fprintf('Hello, %s!\\n', name);\n% end\n\n% Example usage\n% greet('TSI Student');\n\n% Example with matrices and plotting\n% Clear workspace\n% clear; clc;\n% \n% % Create sample data\n% x = linspace(0, 2*pi, 100);\n% y = sin(x);\n% \n% % Plot the data\n% figure;\n% plot(x, y);\n% title('Sine Wave');\n% xlabel('x');\n% ylabel('sin(x)');\n% grid on;\n% \n% % Matrix operations\n% A = [1, 2, 3; 4, 5, 6; 7, 8, 9];\n% B = A * 2;\n% disp('Original matrix:');\n% disp(A);\n% disp('Doubled matrix:');\n% disp(B);\n\n% Example data analysis\n% data = randn(1000, 1);  % Generate random data\n% mean_val = mean(data);\n% std_val = std(data);\n% fprintf('Mean: %.4f, Standard deviation: %.4f\\n', mean_val, std_val);\n`;
}

/**
 * Generates Object Pascal/Delphi code base
 */
function generateObjectPascalCodeBase() {
    return `\n{ Basic Object Pascal/Delphi program }\n\nprogram HelloWorld;\n\n{$APPTYPE CONSOLE}\n\nuses\n  SysUtils;\n\ntype\n  { Example class definition }\n  TTSIStudent = class\n  private\n    FName: string;\n    FProgram: string;\n  public\n    constructor Create(const AName, AProgram: string);\n    procedure Introduce;\n    property Name: string read FName;\n    property Program: string read FProgram;\n  end;\n\n{ TTSIStudent implementation }\nconstructor TTSIStudent.Create(const AName, AProgram: string);\nbegin\n  FName := AName;\n  FProgram := AProgram;\nend;\n\nprocedure TTSIStudent.Introduce;\nbegin\n  Writeln('Hello, I''m ', FName, ' from TSI!');\nend;\n\n{ Main program }\nvar\n  Student: TTSIStudent;\nbegin\n  try\n    Writeln('Hello, World!');\n    Writeln('This is a basic Object Pascal program.');\n    \n    { Example usage }\n    Student := TTSIStudent.Create('TSI Student', 'Computer Science');\n    try\n      Student.Introduce;\n    finally\n      Student.Free;\n    end;\n    \n  except\n    on E: Exception do\n      Writeln(E.ClassName, ': ', E.Message);\n  end;\nend.\n`;
}

/**
 * Generates SQL code base
 */
function generateSqlCodeBase() {
    return `\n-- Basic SQL script template\n-- Note: This is a generic SQL template that works with most SQL databases\n\n-- Create database (if needed)\n-- CREATE DATABASE IF NOT EXISTS tsi_database;\n-- USE tsi_database;\n\n-- Create tables\nCREATE TABLE IF NOT EXISTS students (\n    id INTEGER PRIMARY KEY AUTO_INCREMENT,\n    name VARCHAR(100) NOT NULL,\n    program VARCHAR(100),\n    enrollment_year INTEGER,\n    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP\n);\n\nCREATE TABLE IF NOT EXISTS courses (\n    id INTEGER PRIMARY KEY AUTO_INCREMENT,\n    code VARCHAR(20) UNIQUE NOT NULL,\n    title VARCHAR(200) NOT NULL,\n    credits INTEGER,\n    semester INTEGER\n);\n\n-- Insert sample data\nINSERT INTO students (name, program, enrollment_year) VALUES\n('TSI Student', 'Computer Science', 2024),\n('Jane Doe', 'Electrical Engineering', 2023);\n\nINSERT INTO courses (code, title, credits, semester) VALUES\n('CS101', 'Programming Fundamentals', 6, 1),\n('MATH201', 'Advanced Mathematics', 4, 2);\n\n-- Basic queries\n-- SELECT * FROM students;\n-- SELECT * FROM courses WHERE credits >= 4;\n\n-- Example JOIN query\n-- SELECT s.name, s.program, c.title, c.credits\n-- FROM students s\n-- JOIN courses c ON s.program = 'Computer Science';\n\n-- Update example\n-- UPDATE students SET enrollment_year = 2025 WHERE name = 'TSI Student';\n\n-- Delete example (be careful!)\n-- DELETE FROM students WHERE name = 'Test Student';\n\n-- Drop tables (cleanup)\n-- DROP TABLE IF EXISTS students;\n-- DROP TABLE IF EXISTS courses;\n\n-- Comments:\n-- This script demonstrates basic SQL operations\n-- Modify table names, column names, and data types as needed\n-- Always backup your data before running DDL statements\n`;
}

/**
 * Generates HTML code base
 */
function generateHtmlCodeBase() {
    return `\n<!DOCTYPE html>\n<html lang="en">\n<head>\n    <meta charset="UTF-8">\n    <meta name="viewport" content="width=device-width, initial-scale=1.0">\n    <title>TSI Header - Basic HTML Template</title>\n    <style>\n        body {\n            font-family: Arial, sans-serif;\n            margin: 40px;\n            line-height: 1.6;\n            background-color: #f4f4f4;\n        }\n        .container {\n            max-width: 800px;\n            margin: 0 auto;\n            background-color: white;\n            padding: 20px;\n            border-radius: 5px;\n            box-shadow: 0 2px 5px rgba(0,0,0,0.1);\n        }\n        header {\n            text-align: center;\n            margin-bottom: 2rem;\n        }\n        h1 {\n            color: #2c3e50;\n        }\n        .content {\n            margin: 2rem 0;\n        }\n        footer {\n            text-align: center;\n            margin-top: 2rem;\n            padding-top: 1rem;\n            border-top: 1px solid #eee;\n            color: #666;\n        }\n    </style>\n</head>\n<body>\n    <div class="container">\n        <header>\n            <h1>Hello, World!</h1>\n            <p>This is a basic HTML page.</p>\n        </header>\n        \n        <main class="content">\n            <section>\n                <h2>Welcome to TSI</h2>\n                <p><strong>Transport and Telecommunication Institute</strong> - Riga, Latvia</p>\n                <p>Building the future of technology education and innovation.</p>\n                \n                <h3>Our Programs</h3>\n                <ul>\n                    <li>Computer Science</li>\n                    <li>Electrical Engineering</li>\n                    <li>Telecommunications</li>\n                    <li>Information Technology</li>\n                </ul>\n                \n                <h3>Contact Information</h3>\n                <p>\n                    <strong>Website:</strong> <a href="https://tsi.lv">https://tsi.lv</a><br>\n                    <strong>Email:</strong> info@tsi.lv<br>\n                    <strong>Address:</strong> Riga, Latvia\n                </p>\n            </section>\n        </main>\n        \n        <footer>\n            <p>&copy; 2025 Transport and Telecommunication Institute. All rights reserved.</p>\n        </footer>\n    </div>\n</body>\n</html>\n`;
}

/**
 * Generates default code base for unsupported languages
 */
function generateDefaultCodeBase(languageId) {
    return `\n// Basic ${languageId} program\n\n// Main function - entry point of the program\n// Note: This is a generic template for ${languageId}\n\n// Hello, World! implementation\n// This is a basic ${languageId} program.\n\n// Example comments for ${languageId}\n// Add your code here\n\n// End of program\n`;
}

// Export the main function
module.exports = {
    generateCodeBase
};