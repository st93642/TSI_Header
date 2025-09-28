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
const { generateElmCodeBase } = require('./languages/elm');
const { generateDCodeBase } = require('./languages/d');
const { generateZigCodeBase } = require('./languages/zig');
const { generateNimCodeBase } = require('./languages/nim');
const { generateVCodeBase } = require('./languages/v');
const { generateValaCodeBase } = require('./languages/vala');
const { generateGenieCodeBase } = require('./languages/genie');
const { generateGitCodeBase } = require('./languages/git');
const { generateAbapCodeBase } = require('./languages/abap');
const { generateAppleScriptCodeBase } = require('./languages/applescript');
const { generateRacketCodeBase } = require('./languages/racket');
const { generateTomlCodeBase } = require('./languages/toml');
const { generateAlgolCodeBase } = require('./languages/algol');
const { generateLogoCodeBase } = require('./languages/logo');
const { generateScratchCodeBase } = require('./languages/scratch');
const { generateHackCodeBase } = require('./languages/hack');

/**
 * Generates HAML code base
 */
const { generateHAMLCodeBase } = require('./languages/haml');

/**
 * Generates Handlebars code base
 */
const { generateHandlebarsCodeBase } = require('./languages/handlebars');

/**
 * Generates Razor code base
 */
const { generateRazorCodeBase } = require('./languages/razor');

/**
 * Generates ShaderLab code base
 */
const { generateShaderLabCodeBase } = require('./languages/shaderlab');

/**
 * Generates Slim code base
 */
const { generateSlimCodeBase } = require('./languages/slim');

/**
 * Generates Stylus code base
 */
const { generateStylusCodeBase } = require('./languages/stylus');

/**
 * Generates Svelte code base
 */
const { generateSvelteCodeBase } = require('./languages/svelte');

/**
 * Generates BibTeX code base
 */
const { generateBibTeXCodeBase } = require('./languages/bibtex');

/**
 * Generates Diff code base
 */
const { generateDiffCodeBase } = require('./languages/diff');

/**
 * Generates RPG code base
 */
const { generateRpgCodeBase } = require('./languages/rpg');

/**
 * Generates Apex code base
 */
const { generateApexCodeBase } = require('./languages/apex');

/**
 * Generates Jinja code base
 */
const { generateJinjaCodeBase } = require('./languages/jinja');

/**
 * Generates EJS code base
 */
const { generateEjsCodeBase } = require('./languages/ejs');

/**
 * Generates ERB code base
 */
const { generateErbCodeBase } = require('./languages/erb');

/**
 * Generates Twig code base
 */
const { generateTwigCodeBase } = require('./languages/twig');

/**
 * Generates Crystal code base
 */
const { generateCrystalCodeBase } = require('./languages/crystal');

/**
 * Generates LabVIEW code base
 */
const { generateLabVIEWCodeBase } = require('./languages/labview');

/**
 * Generates CUDA code base
 */
const { generateCUDACodeBase } = require('./languages/cuda');

/**
 * Generates HLSL code base
 */
const { generateHLSLCodeBase } = require('./languages/hlsl');

/**
 * Generates C# code base
 */
const { generateCSharpCodeBase } = require('./languages/csharp');

/**
 * Generates Ruby code base
 */
const { generateRubyCodeBase } = require('./languages/ruby');

/**
 * Generates Rust code base
 */
const { generateRustCodeBase } = require('./languages/rust');

/**
 * Generates Ada code base
 */
const { generateAdaCodeBase } = require('./languages/ada');

/**
 * Generates APL code base
 */
const { generateAplCodeBase } = require('./languages/apl');

/**
 * Generates AWK code base
 */
const { generateAwkCodeBase } = require('./languages/awk');

/**
 * Generates Batch code base
 */
const { generateBatchCodeBase } = require('./languages/batch');

/**
 * Generates ColdFusion code base
 */
const { generateColdFusionCodeBase } = require('./languages/coldfusion');

/**
 * Generates Clojure code base
 */
const { generateClojureCodeBase } = require('./languages/clojure');

/**
 * Generates CoffeeScript code base
 */
const { generateCoffeeScriptCodeBase } = require('./languages/coffeescript');

/**
 * Generates CSS code base
 */
const { generateCssCodeBase } = require('./languages/css');

/**
 * Generates Dockerfile code base
 */
const { generateDockerfileCodeBase } = require('./languages/dockerfile');

/**
 * Generates Docker Compose code base
 */
const { generateDockerComposeCodeBase } = require('./languages/dockercompose');

/**
 * Generates Elixir code base
 */
const { generateElixirCodeBase } = require('./languages/elixir');

/**
 * Generates Erlang code base
 */
const { generateErlangCodeBase } = require('./languages/erlang');

/**
 * Generates Factor code base
 */
const { generateFactorCodeBase } = require('./languages/factor');

/**
 * Generates Forth code base
 */
const { generateForthCodeBase } = require('./languages/forth');

/**
 * Generates F# code base
 */
const { generateFSharpCodeBase } = require('./languages/fsharp');

/**
 * Generates Groovy code base
 */
const { generateGroovyCodeBase } = require('./languages/groovy');

/**
 * Generates Haskell code base
 */
const { generateHaskellCodeBase } = require('./languages/haskell');

/**
 * Generates IDL code base
 */
const { generateIdlCodeBase } = require('./languages/idl');

/**
 * Generates INI code base
 */
const { generateIniCodeBase } = require('./languages/ini');

/**
 * Generates Jade code base
 */
const { generateJadeCodeBase } = require('./languages/jade');

/**
 * Generates JavaScript React code base
 */
const { generateJavaScriptReactCodeBase } = require('./languages/javascriptreact');

/**
 * Generates JSON code base
 */
const { generateJSONCodeBase } = require('./languages/json');

/**
 * Generates JSONC code base
 */
const { generateJSONCCodeBase } = require('./languages/jsonc');

/**
 * Generates Julia code base
 */
const { generateJuliaCodeBase } = require('./languages/julia');

/**
 * Generates LaTeX code base
 */
const { generateLatexCodeBase } = require('./languages/latex');

/**
 * Generates Less code base
 */
const { generateLessCodeBase } = require('./languages/less');

/**
 * Generates Lisp code base
 */
const { generateLispCodeBase } = require('./languages/lisp');

/**
 * Generates Lua code base
 */
const { generateLuaCodeBase } = require('./languages/lua');

/**
 * Generates Maple code base
 */
const { generateMapleCodeBase } = require('./languages/maple');

/**
 * Generates Mathematica code base
 */
const { generateMathematicaCodeBase } = require('./languages/mathematica');

/**
 * Generates Mercury code base
 */
const { generateMercuryCodeBase } = require('./languages/mercury');

/**
 * Generates Objective-C code base
 */
const { generateObjectiveCCodeBase } = require('./languages/objective-c');

/**
 * Generates Objective-C++ code base
 */
const { generateObjectiveCppCodeBase } = require('./languages/objective-cpp');

/**
 * Generates OCaml code base
 */
const { generateOcamlCodeBase } = require('./languages/ocaml');

/**
 * Generates Octave code base
 */
const { generateOctaveCodeBase } = require('./languages/octave');

/**
 * Generates Perl code base
 */
const { generatePerlCodeBase } = require('./languages/perl');

/**
 * Generates PostScript code base
 */
const { generatePostScriptCodeBase } = require('./languages/postscript');

/**
 * Generates PowerShell code base
 */
const { generatePowerShellCodeBase } = require('./languages/powershell');

/**
 * Generates Scheme code base
 */
const { generateSchemeCodeBase } = require('./languages/scheme');

/**
 * Generates SCSS code base
 */
const { generateScssCodeBase } = require('./languages/scss');

/**
 * Generates Shell script code base
 */
const { generateShellScriptCodeBase } = require('./languages/shellscript');

/**
 * Generates Smalltalk code base
 */
const { generateSmalltalkCodeBase } = require('./languages/smalltalk');

/**
 * Generates Solidity code base
 */
const { generateSolidityCodeBase } = require('./languages/solidity');

/**
 * Generates TCL code base
 */
const { generateTclCodeBase } = require('./languages/tcl');

/**
 * Generates TypeScript React code base
 */
const { generateTypeScriptReactCodeBase } = require('./languages/typescriptreact');

/**
 * Generates Go code base
 */
const { generateGoCodeBase } = require('./languages/go');

/**
 * Generates PHP code base
 */
const { generatePhpCodeBase } = require('./languages/php');

/**
 * Generates Swift code base
 */
const { generateSwiftCodeBase } = require('./languages/swift');

/**
 * Generates Scala code base
 */
const { generateScalaCodeBase } = require('./languages/scala');

/**
 * Generates SQL code base
 */
const { generateSqlCodeBase } = require('./languages/sql');

/**
 * Generates HTML code base
 */
const { generateHtmlCodeBase } = require('./languages/html');

/**
 * Generates Kotlin code base
 */
const { generateKotlinCodeBase } = require('./languages/kotlin');

/**
 * Generates YAML code base
 */
const { generateYamlCodeBase } = require('./languages/yaml');

/**
 * Generates Makefile code base
 */
const { generateMakefileCodeBase } = require('./languages/makefile');

/**
 * Generates MATLAB code base
 */
const { generateMatlabCodeBase } = require('./languages/matlab');

/**
 * Generates R code base
 */
const { generateRCodeBase } = require('./languages/r');

/**
 * Generates Dart code base
 */
const { generateDartCodeBase } = require('./languages/dart');

/**
 * Generates Fortran code base
 */
const { generateFortranCodeBase } = require('./languages/fortran');

/**
 * Generates Prolog code base
 */
const { generatePrologCodeBase } = require('./languages/prolog');

/**
 * Generates Markdown code base
 */
const { generateMarkdownCodeBase } = require('./languages/markdown');

/**
 * Generates Assembly code base
 */
const { generateAssemblyCodeBase } = require('./languages/assembly');

/**
 * Generates COBOL code base
 */
const { generateCobolCodeBase } = require('./languages/cobol');

/**
 * Generates Basic(Language) code base
 */
const { generateBasicCodeBase } = require('./languages/basic');

/**
 * Generates Object Pascal code base
 */
const { generateObjectPascalCodeBase } = require('./languages/object-pascal');

/**
 * Generates VB code base
 */
const { generateVbCodeBase } = require('./languages/vb');

/**
 * Generates VBScript code base
 */
const { generateVBScriptCodeBase } = require('./languages/vbscript');

/**
 * Generates Verilog code base
 */
const { generateVerilogCodeBase } = require('./languages/verilog');

/**
 * Generates VHDL code base
 */
const { generateVhdlCodeBase } = require('./languages/vhdl');

/**
 * Generates Vue code base
 */
const { generateVueCodeBase } = require('./languages/vue');

/**
 * Generates Vue HTML code base
 */
const { generateVueHtmlCodeBase } = require('./languages/vue-html');

/**
 * Generates XML code base
 */
const { generateXmlCodeBase } = require('./languages/xml');

/**
 * Generates XSL code base
 */
const { generateXslCodeBase } = require('./languages/xsl');

/**
 * Generates Verse code base
 */
const { generateVerseCodeBase } = require('./languages/verse');

/**
 * Generates Vimscript code base
 */
const { generateVimscriptCodeBase } = require('./languages/vimscript');

/**
 * Generates Sed code base
 */
const { generateSedCodeBase } = require('./languages/sed');

/**
 * Generates SAS code base
 */
const { generateSasCodeBase } = require('./languages/sas');

/**
 * Generates Objective-J code base
 */
const { generateObjectiveJCodeBase } = require('./languages/objective-j');

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
            case 'pug':
                content = generatePugCodeBase();
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
            case 'd':
                content = generateDCodeBase();
                break;
            case 'zig':
                content = generateZigCodeBase();
                break;
            case 'nim':
                content = generateNimCodeBase();
                break;
            case 'v':
                content = generateVCodeBase();
                break;
            case 'vala':
                content = generateValaCodeBase();
                break;
            case 'genie':
                content = generateGenieCodeBase();
                break;
            case 'git':
                content = generateGitCodeBase();
                break;
            case 'scala':
                content = generateScalaCodeBase();
                break;
            case 'scratch':
                content = generateScratchCodeBase();
                break;
            case 'sql':
                content = generateSqlCodeBase();
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
            case 'diff':
                content = generateDiffCodeBase();
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
                content = generateRCodeBase();
                break;
            case 'racket':
                content = generateRacketCodeBase();
                break;
            case 'razor':
                content = generateRazorCodeBase();
                break;
            case 'rpg':
                content = generateRpgCodeBase();
                break;
            case 'matlab':
                content = generateMatlabCodeBase();
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
                content = generateMakefileCodeBase();
                break;
            case 'javascript':
                content = generateJavaScriptCodeBase();
                break;
            case 'abap':
                content = generateAbapCodeBase();
                break;
            case 'ada':
                if (typeof generateAdaCodeBase === 'function') {
                    content = generateAdaCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'apex':
                content = generateApexCodeBase();
                break;
            case 'algol':
                content = generateAlgolCodeBase();
                break;
            case 'apl':
                if (typeof generateAplCodeBase === 'function') {
                    content = generateAplCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'applescript':
                content = generateAppleScriptCodeBase();
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
            case 'bibtex':
                content = generateBibTeXCodeBase();
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
            case 'crystal':
                content = generateCrystalCodeBase();
                break;
            case 'css':
                if (typeof generateCssCodeBase === 'function') {
                    content = generateCssCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'cuda':
                if (typeof generateCUDACodeBase === 'function') {
                    content = generateCUDACodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'hlsl':
                if (typeof generateHLSLCodeBase === 'function') {
                    content = generateHLSLCodeBase();
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
            case 'dockercompose':
                content = generateDockerComposeCodeBase();
                break;
            case 'elixir':
                if (typeof generateElixirCodeBase === 'function') {
                    content = generateElixirCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'elm':
                content = generateElmCodeBase();
                break;
            case 'ejs':
                if (typeof generateEjsCodeBase === 'function') {
                    content = generateEjsCodeBase();
                } else {
                    content = generateDefaultCodeBase(languageId);
                }
                break;
            case 'erb':
                content = generateErbCodeBase();
                break;
            case 'twig':
                content = generateTwigCodeBase();
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
            case 'hack':
                content = generateHackCodeBase();
                break;
            case 'haml':
                content = generateHAMLCodeBase();
                break;
            case 'handlebars':
                content = generateHandlebarsCodeBase();
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
            case 'jinja':
                if (typeof generateJinjaCodeBase === 'function') {
                    content = generateJinjaCodeBase();
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
            case 'jsonc':
                content = generateJSONCCodeBase();
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
            case 'tex':
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
            case 'logo':
                content = generateLogoCodeBase();
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
            case 'shaderlab':
                content = generateShaderLabCodeBase();
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
            case 'slim':
                content = generateSlimCodeBase();
                break;
            case 'stylus':
                content = generateStylusCodeBase();
                break;
            case 'svelte':
                content = generateSvelteCodeBase();
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
            case 'toml':
                content = generateTomlCodeBase();
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
            case 'vue-html':
                if (typeof generateVueHtmlCodeBase === 'function') {
                    content = generateVueHtmlCodeBase();
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
                content = generateYamlCodeBase();
                break;
            case 'kotlin':
                content = generateKotlinCodeBase();
                break;
            case 'labview':
                if (typeof generateLabVIEWCodeBase === 'function') {
                    content = generateLabVIEWCodeBase();
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
 * Generates default code base for unsupported languages
 */
function generateDefaultCodeBase(languageId) {
    return `\n// Basic ${languageId} program\n\n// Main function - entry point of the program\n// Note: This is a generic template for ${languageId}\n\n// Hello, World! implementation\n// This is a basic ${languageId} program.\n\n// Example comments for ${languageId}\n// Add your code here\n\n// End of program\n`;
}

// Export the main function
module.exports = {
    generateCodeBase
};