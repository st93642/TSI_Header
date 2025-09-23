# Changelog

All notable changes to the TSI Header extension will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-09-23

### 🎉 Initial Release

First public release of TSI Header extension for Visual Studio Code.

### ✨ Features Added
- **84+ Programming Languages Support** - Comprehensive language coverage including C, C++, Python, JavaScript, TypeScript, Java, Ruby, Go, Rust, and many more
- **TSI Institutional Branding** - Official Transport and Telecommunication Institute formatting with ASCII logo
- **Git Configuration Integration** - Automatic user detection from git config with intelligent fallback chain
- **Multiple Access Methods**:
  - Right-click context menu: "Insert Header" / "Update Header"
  - Keyboard shortcuts: `Ctrl+Alt+H` (insert), `Ctrl+Alt+U` (update)
  - Command palette: "TSI Header" commands
- **Smart Formatting Engine**:
  - Perfect 79-character alignment
  - Language-specific comment delimiters
  - Dynamic spacing algorithms
  - Multi-character delimiter support
- **Professional Header Templates**:
  - Standard format for C/Java/JavaScript style languages
  - Python frame format for hash-comment languages
  - Institutional contact information and website
- **Robust Error Handling** - Comprehensive validation and user-friendly error messages
- **VS Code Integration** - Native menu integration and settings support

### 🛠 Technical Implementation
- **Architecture**: Ruby CLI backend with JavaScript VS Code wrapper
- **Language Detection**: Automatic language identification via VS Code language IDs
- **Configuration**: Git config file reading with environment variable fallback
- **Testing**: Comprehensive test coverage across all 84+ supported languages

### 📋 Requirements
- Ruby 2.7+ (for header generation engine)
- VS Code 1.74.0+
- Git (optional, for automatic user configuration)

### 🎯 Installation
Install directly from VS Code Marketplace:
1. Open VS Code Extensions (`Ctrl+Shift+X`)
2. Search for "TSI Header"
3. Install extension by `st93642`

### 🙏 Acknowledgments
This extension is a complete rework of the original [42 header extension](https://github.com/kube/vscode-42header) created by 42 students, released under the MIT License. The original concept has been completely reimplemented with TSI branding, expanded language support, and enhanced functionality.

### 📊 Statistics
- **Languages Tested**: 84+
- **Extension Size**: 156.51KB
- **Files Included**: 19
- **Test Coverage**: Production-ready with stress testing

---

## Unreleased

### Planned Features
- Additional programming languages support
- Custom header templates
- Advanced configuration options
- Integration with other TSI development tools

---

**Note**: This extension is developed for the Transport and Telecommunication Institute Computer Science program.