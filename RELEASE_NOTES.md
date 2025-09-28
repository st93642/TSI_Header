# TSI Header Extension - Release Notes

## Version 4.0.0 (September 28, 2025)

### 🚀 Major Release: PHP Support & Header Generation Overhaul

This major release introduces PHP project scaffolding, fixes critical header generation issues, and significantly improves the user experience with streamlined project creation workflows.

### ✨ New Features

#### PHP Project Scaffolding

- **Complete PHP Support** - Added PHP to the 7 supported languages for project creation
- **Composer Integration** - Automatic `composer.json` generation with proper dependencies
- **PHP Class Templates** - Professional BaseClass.php with TSI headers and PSR-4 autoloading
- **Web Project Structure** - Proper `public/`, `src/`, and `tests/` directory organization

#### Enhanced Project Creation

- **7 Languages Supported**: C, C++, Java, Python, Ruby, Rust, PHP
- **Direct Language Templates** - Click any language in TSI Projects panel to go directly to project name input
- **Streamlined Workflow** - Eliminated unnecessary language selection steps
- **Improved UX** - Faster project creation with better visual feedback

#### Header Generation Fixes

- **Ruby CLI API Integration** - Fixed corrupted headers showing "file.c" instead of actual filenames
- **Proper Language Detection** - Added PHP and other missing language detection
- **Consistent Header Format** - All scaffolded files now have correct TSI headers with proper filenames

### 🔧 Technical Improvements

#### Code Quality & Testing

- **Header Generation Tests** - New comprehensive tests for header correctness
- **Project Scaffolding Tests** - Complete test coverage for all 7 languages
- **Filename Validation** - Ensures headers contain correct file paths
- **Ruby CLI Integration** - Proper backend API usage throughout

#### UI/UX Enhancements

- **Context Menu Support** - Right-click access for all operations on files and folders
- **TSI Projects Panel** - Direct language template access without dropdowns
- **Visual Documentation** - Added UI screenshots to README
- **Improved Navigation** - Multiple access methods for different user preferences

#### Development & Maintenance

- **Modular Architecture** - Better separation of concerns in project creators
- **Error Handling** - Improved error messages and debugging information
- **Code Organization** - Cleaner file structure and imports
- **Documentation Updates** - Comprehensive README with usage examples

### 📊 Metrics & Achievements

- **Languages Supported:** 147+ (100% VS Code coverage maintained)
- **Project Languages:** 7 (C, C++, Java, Python, Ruby, Rust, PHP)
- **Automated Tests:** 320+ (100% pass rate maintained)
- **Header Accuracy:** 100% correct filenames in generated headers
- **User Experience:** Streamlined project creation workflow

### 🐛 Bug Fixes

- **Header Corruption Fix** - Resolved "file.c" appearing in all generated headers
- **Import Error Resolution** - Fixed `createLanguageSpecificFiles` undefined error
- **Language Detection** - Added missing PHP and other language detection
- **UI Responsiveness** - Improved panel loading and command execution

### 📚 Documentation

- **Visual Examples** - Added UI screenshots showing panel and project creation
- **Context Menu Documentation** - Complete guide to right-click access methods
- **Workflow Improvements** - Updated usage instructions for new features
- **API Documentation** - Enhanced developer documentation

### 🔄 Migration Notes

- **Major Version Bump** - Due to significant new features and UX improvements
- **Backward Compatible** - All existing functionality preserved
- **Enhanced Features** - New PHP support and improved workflows
- **No Breaking Changes** - Safe upgrade with additional capabilities

### 📦 Installation

#### VS Code Marketplace (Recommended)

1. Open VS Code
2. Press `Ctrl+Shift+X` to open Extensions
3. Search for "TSI Header"
4. Find "TSI Header - st93642" and click Install

#### Local Installation

Download the `tsi-header-4.0.0.vsix` file and install locally:

```bash
code --install-extension tsi-header-4.0.0.vsix
```

### 🎯 Key Highlights

- **PHP Support**: Complete project scaffolding for PHP with composer integration
- **Fixed Headers**: No more corrupted headers in scaffolded projects
- **Better UX**: Direct language template access in projects panel
- **Quality Assurance**: 320+ tests ensuring reliability
- **Multiple Access Methods**: Activity bar, context menus, and command palette

### 🤝 Feedback & Support

- **Issues**: [GitHub Issues](https://github.com/st93642/TSI_Header/issues)
- **Discussions**: [GitHub Discussions](https://github.com/st93642/TSI_Header/discussions)
- **Documentation**: [README](https://github.com/st93642/TSI_Header#readme)

---

**Developed for students with institutional branding and comprehensive language support. Now featuring 100% VS Code language coverage with 147+ programming languages and 7 project scaffolding languages.**
