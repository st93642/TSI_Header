# 🚀 TSI Project Creator - Feature Implementation Plan

## 🎯 **Feature Overview**

Add "Create TSI Project" option to VS Code's right-click context menu in empty workspace areas, allowing users to create complete project structures with TSI headers and boilerplate code.

### **Target User Experience:**

```
Right-click in empty workspace → "Create TSI Project" → Language Selection → Project Structure Created
```

---

## 📋 **Step-by-Step Implementation Plan**

### **Phase 1: VS Code Extension API Integration**

#### **1.1 Update package.json Configuration**

```json
{
  "contributes": {
    "commands": [
      {
        "command": "tsiheader.createTSIProject",
        "title": "Create TSI Project",
        "category": "TSI Header"
      }
    ],
    "menus": {
      "explorer/context": [
        {
          "command": "tsiheader.createTSIProject",
          "when": "explorerResourceIsFolder || !explorerResourceIsFolder",
          "group": "navigation@1"
        }
      ],
      "commandPalette": [
        {
          "command": "tsiheader.createTSIProject",
          "when": "workspaceFolderCount > 0"
        }
      ]
    },
    "submenus": {
      "tsiprojects": {
        "id": "tsiprojects",
        "label": "Create TSI Project"
      }
    }
  }
}
```

#### **1.2 Command Registration in extension.js**

```javascript
// Register the new command
const createProjectDisposable = vscode.commands.registerCommand(
    'tsiheader.createTSIProject', 
    createTSIProject
);
context.subscriptions.push(createProjectDisposable);
```

### **Phase 2: Project Templates System**

#### **2.1 Create Project Templates Module**

**File:** `lib/tsi_project_templates.rb`

```ruby
class TSIProjectTemplates
  SUPPORTED_LANGUAGES = {
    'c' => {
      name: 'C Project',
      extension: '.c',
      build_system: 'makefile',
      structure: ['src/', 'include/', 'build/', 'docs/']
    },
    'cpp' => {
      name: 'C++ Project', 
      extension: '.cpp',
      build_system: 'makefile',
      structure: ['src/', 'include/', 'build/', 'docs/', 'lib/']
    }
  }

  def self.create_project(language, project_name, base_path)
    template = SUPPORTED_LANGUAGES[language]
    return false unless template

    # Create directory structure
    create_directory_structure(template[:structure], base_path, project_name)
    
    # Generate main files
    create_main_file(language, project_name, base_path)
    create_build_files(template[:build_system], project_name, base_path)
    create_documentation_files(project_name, base_path)
    
    true
  end
end
```

#### **2.2 Directory Structure Templates**

```ruby
def create_directory_structure(structure, base_path, project_name)
  project_root = File.join(base_path, project_name)
  
  structure.each do |dir|
    dir_path = File.join(project_root, dir)
    FileUtils.mkdir_p(dir_path)
  end
end
```

### **Phase 3: VS Code File System Integration**

#### **3.1 JavaScript Project Creator**

**File:** `generators/projectCreator.js`

```javascript
const vscode = require('vscode');
const path = require('path');

async function createTSIProject() {
    try {
        // Step 1: Show language selection
        const language = await showLanguageQuickPick();
        if (!language) return;

        // Step 2: Get project name
        const projectName = await showProjectNameInput();
        if (!projectName) return;

        // Step 3: Select workspace location
        const workspaceUri = await selectWorkspaceLocation();
        if (!workspaceUri) return;

        // Step 4: Create project structure
        await createProjectStructure(language, projectName, workspaceUri);
        
        // Step 5: Open created project
        await openProject(workspaceUri, projectName);
        
        vscode.window.showInformationMessage(
            `TSI ${language.toUpperCase()} project "${projectName}" created successfully!`
        );
        
    } catch (error) {
        vscode.window.showErrorMessage(`Failed to create TSI project: ${error.message}`);
    }
}
```

#### **3.2 Language Selection UI**

```javascript
async function showLanguageQuickPick() {
    const languages = [
        {
            label: '$(symbol-class) C Project',
            description: 'Traditional C programming with GCC',
            detail: 'Creates: main.c, Makefile, headers, documentation',
            value: 'c'
        },
        {
            label: '$(symbol-class) C++ Project', 
            description: 'Modern C++ with object-oriented design',
            detail: 'Creates: main.cpp, classes, Makefile, documentation',
            value: 'cpp'
        }
    ];

    const selected = await vscode.window.showQuickPick(languages, {
        placeHolder: 'Select programming language for your TSI project',
        matchOnDescription: true,
        matchOnDetail: true
    });

    return selected ? selected.value : undefined;
}
```

#### **3.3 Project Structure Creation**

```javascript
async function createProjectStructure(language, projectName, workspaceUri) {
    const projectUri = vscode.Uri.joinPath(workspaceUri, projectName);
    
    // Create base directories
    const directories = getDirectoryStructure(language);
    for (const dir of directories) {
        const dirUri = vscode.Uri.joinPath(projectUri, dir);
        await vscode.workspace.fs.createDirectory(dirUri);
    }
    
    // Generate main source file
    await createMainSourceFile(language, projectName, projectUri);
    
    // Create build system files
    await createBuildFiles(language, projectName, projectUri);
    
    // Create documentation
    await createDocumentationFiles(language, projectName, projectUri);
}
```

### **Phase 4: File Content Generation (Reuse Existing APIs)**

#### **4.1 Integrate with Existing Generators**

```javascript
// Reuse existing TSI header and code generation
async function createMainSourceFile(language, projectName, projectUri) {
    const fileName = `main.${getFileExtension(language)}`;
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', fileName);
    
    // Generate TSI header using existing API
    const headerContent = await generateTSIHeader(language, fileName);
    
    // Generate code base using existing API  
    const codeContent = await generateCodeBase(language, fileName);
    
    // Combine header and code
    const fullContent = headerContent + '\n' + codeContent;
    
    // Write to file
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(fullContent));
}
```

#### **4.2 Build System Generation**

```javascript
async function createBuildFiles(language, projectName, projectUri) {
    if (language === 'c' || language === 'cpp') {
        await createMakefile(language, projectName, projectUri);
    }
    
    // Future: CMake, VS Code tasks.json, etc.
}

async function createMakefile(language, projectName, projectUri) {
    const makefileContent = generateMakefileContent(language, projectName);
    const makefileUri = vscode.Uri.joinPath(projectUri, 'Makefile');
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(makefileUri, encoder.encode(makefileContent));
}
```

### **Phase 5: Project Templates Content**

#### **5.1 C Project Template**

```
project_name/
├── src/
│   └── main.c              (TSI header + C boilerplate)
├── include/
│   └── project_name.h      (Header file with TSI header)
├── build/                  (Empty directory for build artifacts)
├── docs/
│   └── README.md          (Project documentation)
├── Makefile               (Build configuration)
└── .gitignore            (C/C++ gitignore template)
```

#### **5.2 C++ Project Template**

```
project_name/
├── src/
│   ├── main.cpp           (TSI header + C++ boilerplate)
│   └── project_name.cpp   (Main class implementation)
├── include/
│   └── project_name.hpp   (Header file with TSI header)
├── lib/                   (Empty directory for libraries)
├── build/                 (Empty directory for build artifacts)
├── docs/
│   └── README.md         (Project documentation)
├── Makefile              (Build configuration)
└── .gitignore           (C/C++ gitignore template)
```

### **Phase 6: Makefile Generation**

#### **6.1 C Makefile Template**

```makefile
# TSI Header C Project Makefile
# Generated by TSI Header Extension
# Project: {PROJECT_NAME}
# Created: {DATE}

CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -Iinclude
SRCDIR = src
BUILDDIR = build
SOURCES = $(wildcard $(SRCDIR)/*.c)
OBJECTS = $(SOURCES:$(SRCDIR)/%.c=$(BUILDDIR)/%.o)
TARGET = $(BUILDDIR)/{PROJECT_NAME}

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(OBJECTS) | $(BUILDDIR)
 $(CC) $(OBJECTS) -o $(TARGET)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c | $(BUILDDIR)
 $(CC) $(CFLAGS) -c $< -o $@

$(BUILDDIR):
 mkdir -p $(BUILDDIR)

clean:
 rm -rf $(BUILDDIR)

run: $(TARGET)
 ./$(TARGET)

install: $(TARGET)
 cp $(TARGET) /usr/local/bin/

.PHONY: help
help:
 @echo "TSI Header C Project - Available targets:"
 @echo "  all     - Build the project"
 @echo "  clean   - Remove build artifacts"
 @echo "  run     - Build and run the project"
 @echo "  install - Install to system"
```

### **Phase 7: Documentation Generation**

#### **7.1 README.md Template**

```markdown
# {PROJECT_NAME}

**Transport and Telecommunication Institute - Programming Project**

## 📋 Project Information

- **Language**: {LANGUAGE}
- **Author**: {USERNAME}
- **Email**: {EMAIL}
- **Created**: {DATE}
- **Institution**: Transport and Telecommunication Institute (TSI)

## 🚀 Quick Start

### Build the Project
```bash
make
```

### Run the Project

```bash
make run
```

### Clean Build Artifacts

```bash
make clean
```

## 📁 Project Structure

```
{PROJECT_NAME}/
├── src/           # Source code files
├── include/       # Header files
├── build/         # Build artifacts (generated)
├── docs/          # Documentation
├── Makefile       # Build configuration
└── README.md      # This file
```

## 🎓 TSI Academic Requirements

This project follows TSI programming standards and includes:

- ✅ Professional TSI headers in all source files
- ✅ Proper code structure and organization
- ✅ Build system configuration
- ✅ Documentation and README

## 📚 Resources

- [TSI Website](https://tsi.lv)
- [Programming Course Materials](https://tsi.lv/study)
- [C Programming Reference](https://en.cppreference.com/w/c)

---

*Generated by TSI Header Extension for VS Code*

```

### **Phase 8: Integration with Existing Systems**

#### **8.1 Reuse Existing Ruby CLI**
```javascript
// Call existing Ruby CLI for header generation
async function generateTSIHeader(language, fileName) {
    return new Promise((resolve, reject) => {
        const ruby = spawn('ruby', [
            'lib/tsi_header_cli.rb', 
            '--language', language,
            '--filename', fileName,
            '--header-only'
        ]);

        let output = '';
        ruby.stdout.on('data', (data) => {
            output += data.toString();
        });

        ruby.on('close', (code) => {
            if (code === 0) {
                resolve(output);
            } else {
                reject(new Error(`Header generation failed with code ${code}`));
            }
        });
    });
}
```

#### **8.2 Reuse Existing Code Generators**

```javascript
// Use existing JavaScript generators
const { generateCodeBase } = require('./generators/codeBaseGenerators');

async function generateCodeBaseContent(language, fileName) {
    const result = generateCodeBase(language, fileName);
    return result.success ? result.content : '';
}
```

---

## 🎯 **Implementation Priority**

### **Phase 1 (MVP)**: Basic Project Creation

- ✅ Command registration
- ✅ Language selection (C, C++)
- ✅ Directory structure creation
- ✅ Basic file generation

### **Phase 2**: Enhanced Templates  

- ✅ Complete Makefile generation
- ✅ Documentation templates
- ✅ .gitignore templates
- ✅ Header file generation

### **Phase 3**: Advanced Features

- ✅ CMake support
- ✅ VS Code tasks.json integration
- ✅ More languages (Java, Python, etc.)
- ✅ Custom project templates

---

## 🛠️ **Technical Requirements**

### **VS Code Extension APIs Needed:**

- ✅ `vscode.commands.registerCommand()` - Command registration
- ✅ `vscode.window.showQuickPick()` - Language selection
- ✅ `vscode.window.showInputBox()` - Project name input
- ✅ `vscode.workspace.fs.createDirectory()` - Directory creation
- ✅ `vscode.workspace.fs.writeFile()` - File writing
- ✅ `vscode.Uri.joinPath()` - Path manipulation

### **Reused Components:**

- ✅ **Ruby CLI** (`lib/tsi_header_cli.rb`) - Header generation
- ✅ **Code Generators** (`generators/codeBaseGenerators.js`) - Boilerplate code
- ✅ **Delimiters** (`lib/tsi_header/delimiters.rb`) - Comment syntax

### **New Components Needed:**

- ✅ **Project Creator** (`generators/projectCreator.js`) - Main logic
- ✅ **Template System** (`lib/tsi_project_templates.rb`) - Project structures  
- ✅ **Build System Generator** (`generators/buildSystemGenerator.js`) - Makefiles, etc.

---

## 🎓 **Educational Benefits**

### **For TSI Students:**

- ✅ **Instant Project Setup** - Skip boilerplate, focus on coding
- ✅ **Professional Structure** - Learn industry-standard project organization
- ✅ **Build System Learning** - Understand Makefiles and compilation
- ✅ **TSI Branding** - Maintain institutional identity
- ✅ **Best Practices** - Follow coding standards from project start

### **Academic Integration:**

- ✅ **Assignment Ready** - Projects include proper TSI headers
- ✅ **Build System** - Students learn compilation process
- ✅ **Documentation** - Professional project documentation included
- ✅ **Scalable** - Easy to extend for more languages and frameworks

---

## 🚀 **Future Enhancements**

### **Additional Languages:**

- Java (Maven/Gradle projects)
- Python (pip, virtual environments)
- JavaScript (npm, Node.js)
- C# (.NET projects)

### **Advanced Features:**

- Custom project templates
- Git repository initialization
- VS Code workspace configuration
- Debugging configuration
- Unit testing setup

---

**🎯 This feature would make TSI Header Extension a complete project management tool for TSI students, providing professional project structures with a single right-click!**
