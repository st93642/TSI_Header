# TSI Header Extension - Supported Languages

## Overview

The TSI Header VS Code extension supports header generation for 60+ programming languages through comment delimiter definitions.

## Language Support Matrix

### Full Code Generation Support (Header + Class + Code Base)

| Language | File Extensions | Class Generation | Code Base Generation |
|----------|----------------|------------------|---------------------|
| Java | `.java` | ✅ | ✅ |
| C++ | `.cpp`, `.cxx`, `.cc`, `.c++` | ✅ (separate .hpp/.cpp) | ✅ |
| C# | `.cs` | ✅ | ✅ |
| Python | `.py` | ✅ | ✅ |
| JavaScript | `.js` | ✅ | ✅ |
| Kotlin | `.kt` | ✅ | ✅ |
| PHP | `.php` | ✅ | ✅ |

### Header-Only Support

| Language | File Extensions | Comment Style |
|----------|----------------|----------------|
| Ada | .ada, .adb, .ads | -- |
| Apl | various | ! |
| Asm | .asm, .s | ;; |
| Assembly | .asm, .s | ;; |
| Awk | .awk | # |
| Basic | .bas, .vb | ;; |
| Vb | .vb | ;; |
| Bat | .bat, .cmd | ;; |
| Batch | .bat, .cmd | ;; |
| C | .c, .h | /**/ |
| Cfml | .cfm, .cfc | <!--- ---> |
| Clojure | .clj, .cljs | ;; |
| Cobol | .cob, .cbl | ;; |
| Coffeescript | .coffee | # |
| Coldfusion | .cfm, .cfc | <!--- ---> |
| Cpp | .cpp, .cxx, .cc, .c++, .hpp, .hxx, .h++ | /**/ |
| Csharp | .cs | /**/ |
| Css | .css | /**/ |
| Dart | .dart | /**/ |
| Dockerfile | Dockerfile | # |
| Elixir | .ex, .exs | # |
| Erlang | .erl, .hrl | %% |
| Factor | .factor | ! |
| Forth | .fs, .fth | (**) |
| Fortran | .f, .for, .f90 | ! |
| Fsharp | .fs, .fsx | (**) |
| Go | .go | /**/ |
| Groovy | .groovy, .gvy | /**/ |
| Haskell | .hs, .lhs | -- |
| Html | .html, .htm | <!-- --> |
| Idl | .idl | ;; |
| Ini | .ini, .cfg | ;; |
| Jade | .jade | /**/ |
| Java | .java | /**/ |
| Javascript | .js | /**/ |
| Javascriptreact | .jsx | /**/ |
| Julia | .jl | # |
| Kotlin | .kt | /**/ |
| Latex | .tex, .latex | %% |
| Less | .less | /**/ |
| Lisp | .lisp, .lsp | ;; |
| Lua | .lua | -- |
| Makefile | Makefile, makefile | # |
| Maple | .mpl, .maple | # |
| Mathematica | .nb, .m | (**) |
| Matlab | .m | %% |
| Mercury | .m | %% |
| Objective-c | .m, .mm | /**/ |
| Objective-cpp | .mm | /**/ |
| Objective-j | .j | /**/ |
| Ocaml | .ml, .mli | (**) |
| Octave | .m | %% |
| Pascal | .pas, .pp | { } |
| Perl | .pl, .pm | # |
| Perl6 | .pl6, .pm6 | # |
| Raku | .raku, .pm6 | # |
| Php | .php | /**/ |
| Plaintext | (various) | # |
| Postscript | .ps | %% |
| Powershell | .ps1 | # |
| Prolog | .pl, .pro | %% |
| Python | .py | # |
| R | .r, .R | # |
| Ruby | .rb | # |
| Rust | .rs | /**/ |
| Sas | .sas | /**/ |
| Scheme | .scm, .ss | ;; |
| Scss | .scss | /**/ |
| Sed | .sed | # |
| Shellscript | .sh, .bash | # |
| Smalltalk | .st | " " |
| Solidity | .sol | /**/ |
| Sql | .sql | # |
| Swift | .swift | /**/ |
| Tcl | .tcl | # |
| Typescript | .ts | /**/ |
| Typescriptreact | .tsx | /**/ |
| Verse | .verse | <# #> |
| Vhdl | .vhdl, .vhd | -- |
| Vimscript | .vim | " " |
| Vue | .vue | <!-- --> |
| Xml | .xml | <!-- --> |
| Xsl | .xsl, .xslt | /**/ |
| Yaml | .yaml, .yml | # |
| Yml | .yaml, .yml | # |

## Usage Notes

### Header Generation

- **Supported**: All languages listed above
- **Command**: `TSI Header: Insert Header` or `TSI Header: Update Header`
- **Auto-save**: Headers automatically update timestamps when files are saved

### Class Generation

- **Supported**: Java, C++, C#, Python, JavaScript, Kotlin, PHP
- **Command**: `TSI Header: Add Class`
- **Features**:
  - Language-specific syntax and conventions
  - Proper constructors, getters/setters
  - Documentation comments
  - C++ creates separate .hpp/.cpp files

### Code Base Generation

- **Supported**: Java, C++, C#, Python, JavaScript, Kotlin, PHP, C
- **Command**: `TSI Header: Add Code Base`
- **Features**: Basic program templates with main functions

## File Extension Detection

The extension automatically detects languages based on:

1. VS Code's language identification
2. File extension fallback
3. Plaintext handling for misidentified files (.kt files detected as plaintext)

## Adding New Language Support

To add support for a new language:

1. Add entry to `LANGUAGE_DELIMITERS` in `lib/tsi_header/delimiters.rb`
2. Add class generation logic to `src/extension.js` (optional)
3. Add code base generation logic to `src/extension.js` (optional)
4. Update this documentation

## Technical Details

- **Backend**: Ruby CLI for header generation
- **Frontend**: VS Code extension (JavaScript/Node.js)
- **Comment Styles**: 14 different comment delimiter patterns supported
- **Header Format**: 79-character wide TSI-branded headers
- **Configuration**: Username/email via VS Code settings or git config

---
*Generated on: September 24, 2025*
*Extension Version: 1.2.0*
*Supported Languages: 60+*
