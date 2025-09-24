# TSI Header Extension for VS Code

![TSI Logo](tsi.jpg)

A professional VS Code extension that generates standardized headers for source code files with Transport and Telecommunication Institute (TSI) branding. Supports 84+ programming languages with comprehensive code generation features.

## ✨ Features

### 🎯 **Header Management**

- **Insert Headers**: Add professional TSI-branded headers to any source file
- **Update Headers**: Automatically update timestamps and author information
- **Auto-Save Updates**: Optional automatic header updates when files are saved
- **Smart Detection**: Only processes files with existing TSI headers during auto-updates

### 🏗️ **Code Generation**

- **Class Generation**: Create complete class templates with constructors, getters/setters, and methods
- **Code Base Generation**: Generate boilerplate code structures (main functions, basic program templates)
- **Multi-Language Support**: Full class and code base generation for 7 languages:
  - Java (with package support)
  - C++ (header + implementation files)
  - C# (with properties and methods)
  - Python (with property decorators)
  - JavaScript (ES6 classes with modules)
  - Kotlin (data classes with companion objects)
  - PHP (with namespaces and methods)

### 🌍 **Language Support (84+ Languages)**

Supports header generation for: C, C++, Rust, Go, Assembly, JavaScript, TypeScript, HTML, CSS, Python, Ruby, Java, Swift, Kotlin, Haskell, OCaml, F#, Clojure, R, MATLAB, Julia, PHP, Perl, Shell, and many more...

### 🎨 **Smart Formatting**

- **79-Character Alignment**: Perfect alignment for all header elements
- **Multi-Character Delimiters**: Proper comment syntax for each language
- **Dynamic Spacing**: Intelligent spacing algorithms for different comment styles

**Standard Format (C/Java/JavaScript):**

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

**Python Frame Format:**

```python
#*****************************************************************************#
#                                                                             #
#  filename.py                                            TTTTTTTT SSSSSSS II #
#                                                            TT    SS      II #
#  By: username@students.tsi.lv                              TT    SSSSSSS II #
#                                                            TT         SS II #
#  Created: Sep 24 2025 11:47 Full Name                      TT    SSSSSSS II #
#  Updated: Sep 24 2025 11:47 Full Name                                       #
#                                                                             #
#   Transport and Telecommunication Institute - Riga, Latvia                  #
#                       https://tsi.lv                                        #
#*****************************************************************************#
```

## 🚀 Installation & Usage

### **Installation**

1. Install from VS Code Marketplace or download .vsix file
2. Ensure Ruby 2.7+ is installed on your system
3. Configure your credentials (see Configuration section)

### **Usage**

#### **Header Commands**

- **Insert Header**: `Ctrl+Alt+H` (Cmd+Alt+H on Mac) or right-click → "TSI Header: Insert Header"
- **Update Header**: `Ctrl+Alt+U` (Cmd+Alt+U on Mac) or right-click → "TSI Header: Update Header"
- **Command Palette**: `Ctrl+Shift+P` → Search for "TSI Header"

#### **Code Generation**

- **Add Class**: Right-click in editor → "TSI Header: Add class" (available for Java, C++, C#, Python, JavaScript, Kotlin, PHP)
- **Add Code Base**: Right-click in editor → "TSI Header: Add code base" (works with any file)

### **Configuration**

The extension automatically detects credentials in this priority order:

1. VS Code settings (`tsiheader.username`, `tsiheader.email`)
2. Git configuration (`git config user.name/email`)
3. Environment variables (`TSI_USERNAME`, `TSI_EMAIL`)

#### **VS Code Settings**

```json
{
  "tsiheader.username": "your_username",
  "tsiheader.email": "your_email@students.tsi.lv",
  "tsiheader.autoUpdate": false
}
```

#### **Git Configuration**

```bash
git config --global user.name "Your Full Name"
git config --global user.email "your_email@students.tsi.lv"
```

## 🏛️ **Institutional Branding**

- Official TSI ASCII logo
- Complete university information
- Professional academic formatting
- Riga, Latvia location details
- Official TSI website reference

## 🛠️ **Technical Architecture**

- **Frontend**: JavaScript VS Code extension
- **Backend**: Ruby CLI for header generation
- **Architecture**: Modular design with separated concerns
- **Core Components**:
  - `header_generator.rb` - Template rendering engine
  - `delimiters.rb` - Language-specific comment syntax
  - `classGenerators.js` - Multi-language class templates
  - `codeBaseGenerators.js` - Boilerplate code generation
  - `contentAnalyzer.js` - Smart content analysis utilities

## 🧪 **Quality Assurance**

- **84+ Languages Tested**: Comprehensive test coverage
- **Perfect Alignment**: 79-character formatting validated
- **Multi-Character Support**: All comment delimiters verified
- **Production Ready**: All features thoroughly tested

## 🤝 Contributing

Contributions are welcome! Areas for improvement:

- Adding support for new programming languages
- Enhancing existing code generation templates
- Improving user experience and error handling
- Performance optimizations
- Documentation updates

## 📄 License

This project is licensed under the MIT License.

## 🙏 Acknowledgments

This extension builds upon the original [42 header extension](https://github.com/kube/vscode-42header) concept, completely reimplemented with TSI branding, expanded language support, and enhanced functionality including comprehensive code generation features.

---

## Developed for the Transport and Telecommunication Institute Computer Science program
