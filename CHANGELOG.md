# Changelog

All notable changes to the TSI Header extension will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.2.1] - 2025-09-24

### ✨ Added

- **Class Generation Support**: Added class generation for 13 programming languages (Java, C++, C#, Python, JavaScript, Kotlin, PHP, TypeScript, Ruby, Go, Swift, Dart, Scala)
- **Code Base Generation**: Added boilerplate code generation for 23+ programming languages with language-specific templates
- **Enhanced Menu Integration**: Context-aware command availability based on file extensions for all supported languages

### 🐛 Fixed

- **Class Generation Menu Conditions**: Fixed menu visibility for Dart and Scala files (`.dart`, `.scala` extensions now properly recognized)
- **Code Base Generation**: Fixed 15+ languages that were falling back to C boilerplate instead of using proper language-specific templates:
  - PHP (`.php`) - Now generates proper PHP class structure
  - Swift (`.swift`) - Now generates proper Swift class structure  
  - Dart (`.dart`) - Now generates proper Dart class structure
  - Scala (`.scala`) - Now generates proper Scala class structure
  - Python (`.py`) - Now generates proper Python class structure
  - Ruby (`.rb`) - Now generates proper Ruby class structure
  - Go (`.go`) - Now generates proper Go struct/function structure
  - Rust (`.rs`) - Now generates proper Rust struct/impl structure
  - And 7+ additional languages with proper boilerplate

### 🛠️ Improved

- **Language-Specific Boilerplate**: All code base generation now uses appropriate language syntax and conventions instead of generic C fallbacks
- **Menu System Reliability**: File extension regex patterns updated to include all supported languages for class and code base generation
- **Code Quality**: Eliminated generic fallbacks in favor of comprehensive language-specific implementations

### 📋 Technical Changes

- Added `generateClass()` function with 13 language-specific implementations
- Added `generateCodeBase()` function with 23+ language-specific templates
- Updated menu conditions in `package.json` to include Dart and Scala extensions
- Enhanced language detection logic for proper template selection
- Improved error handling for unsupported language edge cases

### 🎯 Compatibility

None - fully backward compatible with v1.1.0

### 📊 Statistics

- **Header Languages**: 84+ (unchanged)
- **Code Base Languages**: 23+ (new feature)
- **Class Languages**: 13 (new feature)
- **Test Coverage**: 22 languages validated with 100% pass rate

## [1.1.0] - 2025-09-24

### ✨ New Features

- **Auto-Save Functionality**: New `tsiheader.autoUpdate` setting enables automatic header updates when files are saved
- **Smart Header Detection**: Only files with existing TSI headers are processed during auto-updates  
- **Proactive Credential Checking**: Extension now validates credentials before execution and provides helpful setup guidance
- **Multi-Source Credential Detection**: Checks VS Code settings, git config, and environment variables automatically
- **Configuration Assistant**: Friendly setup instructions with direct links to settings and git config help
- **Silent Background Operations**: Auto-updates work quietly without interrupting workflow

### 🛠️ Improvements  

- **Enhanced Error Handling**: Replaced error messages with welcoming configuration instructions
- **User Experience**: Eliminated scary error popups in favor of helpful guidance with emojis and clear steps
- **Performance Optimization**: Quick header detection without full file parsing
- **Credential Management**: Graceful fallback chain from VS Code settings → git config → environment variables

### 🔧 Technical Changes

- Added `onDidSaveTextDocument` listener for auto-update functionality
- Enhanced credential validation logic with comprehensive error handling
- Implemented selective file processing based on existing TSI header detection
- Added comprehensive logging for debugging auto-update operations
- Improved extension activation and command registration flow

### 📋 New Configuration Options

- `tsiheader.autoUpdate` (boolean, default: false) - Enable automatic header updates on file save

### 🎯 Breaking Changes

None - fully backward compatible with v1.0.1

### 🐛 Bug Fixes

- Credential detection edge cases where git config might be missing
- Error message handling for various failure scenarios
- Extension stability during background auto-update operations

## [1.0.1] - 2025-09-23

### 🔧 Fixed

- **Repository Links** - Added proper homepage, issues, and bugs URLs pointing to GitHub repository
- **VS Code Marketplace Integration** - Fixed broken links in extension details page
- **User Support** - Added clear issue reporting channels for better user experience

### 📋 Metadata Updates

- Added `homepage` field pointing to README
- Added `bugs` object with GitHub issues URL and support email
- Updated package.json metadata for better marketplace presentation

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

### 📊 Statistics (1.0.0)

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
