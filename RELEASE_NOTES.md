# TSI Header Extension v1.2.1 Release Notes

## 🚀 **What's New in v1.2.1**

### ✨ **Major New Features**

- **Class Generation**: Generate classes for 13 programming languages (Java, C++, C#, Python, JavaScript, Kotlin, PHP, TypeScript, Ruby, Go, Swift, Dart, Scala)
- **Code Base Generation**: Create boilerplate code structures for 23+ languages with language-specific templates
- **Enhanced Menu Integration**: Context-aware commands that appear based on your file type

### 🐛 **Critical Fixes**

- **C++ Code Generation**: Fixed C++ files (`.cpp`) generating generic C code instead of proper C++ boilerplate with `std::cout` and `<iostream>`
- **Scala Language Detection**: Fixed Scala files (`.scala`) being detected as plaintext, now properly generates Scala-specific code and classes via extension-based fallback
- **Plaintext File Handling**: Enhanced plaintext file detection to support both Kotlin (`.kt`) and Scala (`.scala`) files with appropriate language-specific generation

### 📊 **Extension Statistics**

- **Header Generation**: 84+ programming languages
- **Code Base Generation**: 23+ languages with proper templates
- **Class Generation**: 13 languages with object-oriented structures
- **Test Coverage**: 22 languages validated (100% pass rate)

## 🎯 **How to Use**

### Class Generation

1. Open any supported file (`.java`, `.cpp`, `.py`, `.js`, etc.)
2. Right-click → "TSI Header: Add Class"
3. Enter class name when prompted

### Code Base Generation

1. Open any supported file
2. Right-click → "TSI Header: Add Code Base"
3. Get language-appropriate boilerplate code

### Header Operations (unchanged)

- Right-click → "TSI Header: Insert Header" or "TSI Header: Update Header"
- Keyboard: `Ctrl+Alt+H` (insert), `Ctrl+Alt+U` (update)

## 🔧 **Technical Details**

- **Version**: 1.2.1
- **Published**: September 24, 2025
- **Publisher**: st93642
- **Marketplace**: [https://marketplace.visualstudio.com/items?itemName=st93642.tsi-header](https://marketplace.visualstudio.com/items?itemName=st93642.tsi-header)
- **Compatibility**: VS Code 1.74.0+
- **Requirements**: Ruby 2.7+ for header generation

## 🎓 **For TSI Students**

This release significantly expands the extension's capabilities for the TSI Computer Science program, providing professional code generation tools for modern development workflows.

---

**Installation**: Search for "TSI Header" in VS Code Extensions or install via ID: `st93642.tsi-header`
