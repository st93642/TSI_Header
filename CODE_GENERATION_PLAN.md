# Code Generation Feature - Implementation Plan

## 🎯 **Feature Overview**

Add "TSI Header/Create class/base code" to context menu for generating complete file structures with headers.

## 📋 **Implementation Phases**

### **Phase 1: Foundation (Basic C Support)**

- [ ] Add new command `tsiheader.createCodeStructure`
- [ ] Create basic template system
- [ ] Implement C file generation with main function
- [ ] Add context menu integration
- [ ] Basic file creation with conflict detection

### **Phase 2: Enhanced C Support**

- [ ] Add header file (.h) generation
- [ ] Create header/implementation pairs
- [ ] Smart file naming (header guards, etc.)
- [ ] Include guards for header files

### **Phase 3: Multi-Language Support**

- [ ] Python class templates
- [ ] Java class templates
- [ ] C++ class templates
- [ ] Language-specific project structures

### **Phase 4: Advanced Features**

- [ ] Template customization UI
- [ ] Project structure awareness
- [ ] Custom template import/export
- [ ] Integration with VS Code project wizards

---

## 🛠️ **Technical Architecture**

### **Template System**

```text
templates/
├── c/
│   ├── main.c.template
│   ├── header.h.template
│   └── class.c.template
├── cpp/
│   ├── main.cpp.template
│   ├── header.hpp.template
│   └── class.cpp.template
├── python/
│   ├── main.py.template
│   ├── class.py.template
│   └── module.py.template
└── java/
    ├── Main.java.template
    └── Class.java.template
```

### **Template Variables**

- `{{FILENAME}}` - Base filename
- `{{CLASSNAME}}` - PascalCase class name
- `{{USERNAME}}` - From git config or VS Code settings
- `{{EMAIL}}` - From git config or VS Code settings
- `{{DATE}}` - Current date
- `{{HEADER_GUARD}}` - For C/C++ headers
- `{{PACKAGE}}` - For Java packages

### **Command Flow**

1. User right-clicks in Explorer → "TSI Header/Create class/base code"
2. Show language-specific template selection dialog
3. Prompt for filename/classname if needed
4. Generate files with headers and basic code structure
5. Open generated files in editor

---

## 🔧 **Current Implementation Status**

### **✅ Completed**

- Feature concept and requirements defined
- Added to roadmap as priority #0
- Basic technical architecture planned
- Template directory structure created
- C main function template implemented
- Command added to package.json (context menu, command palette)
- Basic file creation logic implemented in extension.js
- Template variable substitution working
- Credential detection integrated
- File conflict detection added
- Feature committed to dedicated branch

### **� In Progress**

- Testing the basic C file generation
- Refining user experience (prompts, feedback)
- Adding more template types

### **📋 Next Steps**

1. Test the basic C file generation functionality
2. Refine the user prompts and error handling
3. Add more C templates (header files, classes)
4. Implement multi-language support
5. Add template customization options
