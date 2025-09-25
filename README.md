# TSI Header Extension for VS Code

![TSI Logo](tsi.jpg)

## Code Generation & Professional Headers for TSI Students

Generate complete classes, boilerplate code, and professional TSI-branded headers for **88 programming languages** with **comprehensive quality assurance**. Access all features through the dedicated TSI Header panel in the left sidebar.

## 🏗️ **Primary Features**

### **Code Generation**

- **Generate Classes**: Complete class templates with constructors, methods, and documentation
- **Generate Code Boilerplates**: Ready-to-run program templates for any language
- **Create TSI Projects**: Full project scaffolding with build systems, documentation, and language-specific structure
- **88 Languages Supported** for code generation (see full list below)
- **Quality Assured**: Comprehensive testing ensures correct code generation

### **Professional Headers**

- TSI-branded headers with institutional information
- Auto-update timestamps and author info
- Headers can be deleted if auto-update is disabled
- **88 Languages Supported** for headers

### **Project Management**

- **Complete Project Creation**: Generate full project structures with build files, documentation, and starter code
- **Language-Specific Scaffolding**: Proper project layout for C, C++, Java, Python, Ruby, Rust, and more
- **Build System Integration**: Automatic generation of Makefiles, Maven/Gradle files, and other build configurations
- **Documentation Templates**: README files and project documentation generation

## 🧪 **Quality Assurance**

This extension has undergone comprehensive testing:

- ✅ **88/88 languages** generate correct, language-specific code
- ✅ **100% success rate** in automated testing (225/225 tests pass)
- ✅ All generated code follows language best practices
- ✅ Comprehensive test suite included for ongoing quality verification
- ✅ Cross-platform validation (Linux, macOS, Windows support)

## 📋 **Supported Languages**

### **Class Generation** (13 Languages)

Java, C++, C#, Python, JavaScript, Kotlin, PHP, TypeScript, Ruby, Go, Swift, Dart, Scala

### **Code Boilerplate Generation** (88 Languages)

Ada, APL, Assembly, AWK, Basic, Batch, C, C++, CoffeeScript, ColdFusion, Clojure, COBOL, CSS, Dart, Delphi, Dockerfile, Elixir, Erlang, F#, Factor, Forth, Fortran, Fortran90, FortranFreeForm, Go, Groovy, Haskell, HTML, IDL, INI, Jade, Java, JavaScript, JavaScript React, JSON, Julia, Kotlin, LaTeX, Less, Lisp, Lua, Maple, Markdown, Mathematica, MATLAB, Mercury, Objective-C, Objective-C++, Objective-J, Object Pascal, OCaml, Octave, Pascal, Perl, Perl6, PHP, PostScript, PowerShell, Prolog, Python, R, Raku, Ruby, Rust, SAS, SCSS, Scheme, Sed, Shell Script, Smalltalk, Solidity, SQL, Swift, SystemVerilog, TCL, TypeScript, TypeScript React, VB, VBScript, Verse, Verilog, VHDL, Vimscript, Vue, XML, XSL, YAML

### **Project Creation** (6 Languages)

C, C++, Java, Python, Ruby, Rust - complete project scaffolding with build systems and documentation

## 🚀 **Quick Start**

### **Installation**

1. Install from VS Code Marketplace or download .vsix file
2. Ensure Ruby 2.7+ is installed
3. Configure your credentials (see Configuration below)
4. **Access Features**: Look for the mortar board icon (🎓) in the left sidebar to open the TSI Header panel

### **Usage**

#### **Accessing Features**

All TSI Header features are available through the **TSI Header icon** in the left sidebar (Activity Bar). Click the mortar board icon to open the TSI Header panel with two sections:

- **TSI Commands**: Header management and code generation tools
- **TSI Projects**: Project creation and scaffolding tools

#### **Header Management**

- **Insert Header**: Click "Insert Header" in TSI Commands view or use `Ctrl+Alt+H`
- **Update Header**: Click "Update Header" in TSI Commands view or use `Ctrl+Alt+U`
- **Note**: Headers can be manually deleted if auto-update is disabled

#### **Code & Class Generation**

- **Add Class**: Click "Add class" in TSI Commands view to generate complete class templates
- **Add Code Base**: Click "Add code base" in TSI Commands view to generate boilerplate code

#### **Project Creation**

- **Create TSI Project**: Click "Create TSI Project" in TSI Projects view to generate complete project structures
- **Supported Languages**: C, C++, Java, Python, Ruby, Rust with full build system integration
- **Project Features**: Build files, documentation, starter code, and proper directory structure

### **Configuration**

```json
{
  "tsiheader.username": "your_username",
  "tsiheader.email": "your_email@students.tsi.lv",
  "tsiheader.autoUpdate": false
}
```

Or use Git config:

```bash
git config --global user.name "Your Full Name"
git config --global user.email "your_email@students.tsi.lv"
```

## 🧪 **Testing & Quality Assurance**

### **Comprehensive Test Suite**

The extension includes a comprehensive test suite that validates:

- Header generation for all 88 supported languages
- Code base generation for all 88 supported languages
- Class generation for 13 object-oriented languages
- Comment marker validation for each language
- Syntax correctness of generated code
- Cross-platform compatibility

### **Running Tests**

```bash
# Test all languages (comprehensive)
node comprehensive_test_suite.js --all

# Test first N languages
node comprehensive_test_suite.js --max=10

# Test specific functionality
node comprehensive_test_suite.js --headers-only
node comprehensive_test_suite.js --codebase-only
node comprehensive_test_suite.js --classes-only
```

### **Test Results**

- **Total Tests**: 225 automated tests
- **Success Rate**: 100% (all tests pass)
- **Coverage**: All supported languages and features
- **Platforms**: Linux, macOS, Windows

## 📄 **Example Header**

```c
/*****************************************************************************/
/*                                                                           */
/*  filename.c                                           TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: username@students.tsi.lv                            TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 11:47 Full Name                    TT    SSSSSSS II */
/*  Updated: Sep 24 2025 11:47 Full Name                                     */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/
```
