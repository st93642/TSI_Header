# Code Header Extension Project Map

This document provides a comprehensive overview of the Code Header extension project, detailing all modules, APIs, interfaces, and their interconnections. It serves as a reference for debugging and development.

---

## Top-Level Structure

- `CHANGELOG.md`, `LICENSE`, `PROJECT_MAP.md`, `README.md`: Project documentation and metadata.
- `package.json`: Node.js package configuration.
- `unified_test_suite.js`: Comprehensive unified test suite for all 102+ supported languages.
- `tsi.jpg`, `tsi.png`: Project images.

---

## Main Directories & Their Responsibilities

### 1. `examples/`

- **Purpose:** Example usage of TSI_Header in different languages.
- **Files:**
  - `demo.c`: C example.
  - `demo.py`: Python example.

### 2. `generators/`

- **Purpose:** Contains code generators for various project components.
- **Modules:**
  - `classGenerators.js`: Generates class templates for 13 object-oriented languages.
  - `codeBaseGenerators.js`: Generates codebase scaffolding and imports language-specific generators for 101+ languages.
  - `languages/`: Modular language-specific code base generators.
    - `c.js`: C language code base generator.
    - `cpp.js`: C++ language code base generator.
    - `python.js`: Python language code base generator.
    - `java.js`: Java language code base generator.
    - `javascript.js`: JavaScript language code base generator.
    - `typescript.js`: TypeScript language code base generator.
  - `project/`
    - `buildSystemGenerator.js`: Main build system generator that imports language-specific modules.
    - `buildsystems/`: Modular build system generators.
      - `c.js`: C Makefile generation.
      - `cppBuildSystem.js`: C++ Makefile generation.
      - `javaBuildSystem.js`: Java Maven/Gradle build files generation.
      - `pythonBuildSystem.js`: Python Makefile generation.
    - `documentationGenerator.js`: Generates documentation files.
    - `gitIgnoreGenerator.js`: Generates `.gitignore` files.
    - `headerUtils.js`: Header utility functions.
    - `projectCreator.js`: Orchestrates project creation using other generators.
    - `projectcreators/`: Language-specific project file creators.
      - `cProjectCreator.js`: C-specific project file generation (no classes).
      - `cppProjectCreator.js`: C++-specific project file generation (includes BaseClass).
      - `index.js`: Unified interface for language-specific project creation.
      - `javaProjectCreator.js`: Java-specific project file generation.
      - `pythonProjectCreator.js`: Python-specific project file generation.
      - `rubyProjectCreator.js`: Ruby-specific project file generation.
      - `rustProjectCreator.js`: Rust-specific project file generation.

#### Interconnections

- `projectCreator.js` imports and coordinates other generators to create a full project structure.
- `codeBaseGenerators.js` imports language-specific generators from `languages/` for modular code base generation.
- `buildSystemGenerator.js` imports build system generators from `buildsystems/` for modular build file generation.
- `projectcreators/index.js` routes to language-specific project creators (C, C++, Java, Python, Ruby, Rust) for appropriate file generation.

### 3. `lib/`

- **Purpose:** Ruby CLI and core library for TSI_Header header generation.
- **Files:**
  - `tsi_header_cli.rb`: Command-line interface for TSI_Header (supports 102+ languages).
  - `tsi_header.rb`: Main Ruby library entry point.
  - `tsi_header/`
    - `configuration.rb`: Handles configuration parsing and management.
    - `delimiters.rb`: Defines header delimiters for 102+ supported languages.
    - `header_extractor.rb`: Extracts headers from source files.
    - `header_generator.rb`: Generates headers for source files (supports plain text for languages without comments).
    - `header_info.rb`: Data structure for header information.
    - `header_parser.rb`: Parses header blocks from files.

#### Ruby Library Interconnections

- `tsi_header_cli.rb` uses `tsi_header.rb` as its backend.
- `tsi_header.rb` loads modules from `tsi_header/` for specific tasks.
- `header_extractor.rb` and `header_generator.rb` use `delimiters.rb` and `header_info.rb` for parsing and data management.
- `header_generator.rb` includes special handling for languages without comment delimiters (JSON, Markdown).

### 4. `resources/`

- **Purpose:** Project icons and images for UI/branding.
- **Files:**
  - `tsi-24.png`: Small icon for UI elements.
  - `tsi-icon.png`: Main icon image.
  - `tsi-icon.svg`: Scalable vector icon.

### 5. `scripts/`

- **Purpose:** Ruby scripts for compilation and testing.
- **Files:**
  - `compile.rb`: Compiles project files.
  - `comprehensive_language_test.rb`: Runs language compatibility tests.
  - `watch.rb`: Watches files for changes and triggers actions.

### 6. `spec/`

- **Purpose:** Ruby test suite for TSI_Header.
- **Files:**
  - `unified_test.rb`: Unified Ruby test suite.

### 7. `src/`

- **Purpose:** JavaScript source code for VS Code extension.
- **Files:**
  - `extension.js`: Main entry point for the VS Code extension.
  - `tsiViewProvider.js`: Provides custom views in the VS Code UI.

#### VS Code Extension Interconnections

- `extension.js` registers commands and UI elements, using `tsiViewProvider.js` for custom views.

### 8. `test_output/`

- **Purpose:** Output directory for unified test suite results.
- **Files:**
  - `test_*.txt`: Generated test files for each language.
  - `unified_test_report.json`: Detailed test results and statistics.
  - `TestClass.hpp`, `TestClass.cpp`: C++ class generation test files.

### 9. `utils/`

- **Purpose:** Utility functions for code analysis.
- **Files:**
  - `contentAnalyzer.js`: Analyzes file content for headers and structure.

## APIs & Interfaces

### Ruby CLI/API (Primary Header Generation)

- **Entry Point:** `lib/tsi_header_cli.rb`
- **Commands:**
  - `insert <language> <file>`: Insert new header
  - `update <language> <file>`: Update existing header
- **Interfaces:**
  - Command-line arguments for header generation, extraction, and parsing.
  - Supports 102+ programming languages with appropriate comment delimiters.
  - Uses classes from `lib/tsi_header/` for implementation.
  - Returns JSON responses for programmatic use.

### JavaScript Extension API

- **Entry Point:** `src/extension.js`
- **Interfaces:**
  - VS Code commands and events.
  - Custom views via `tsiViewProvider.js`.

### Unified Test Suite API

- **Entry Point:** `unified_test_suite.js`
- **Interfaces:**
  - Single command execution with comprehensive reporting.
  - Validates header generation, code base generation, class creation, and Extension API integration.
  - Generates detailed JSON reports and console output.
  - Supports 102+ languages for headers and code bases, 13 for classes, 6 for projects.

### Generators API

- **Entry Point:** `generators/project/projectCreator.js`
- **Interfaces:**
  - Functions to create project files, documentation, and build systems.
  - Calls other generator modules for specific tasks.

---

## Interconnections & Data Flow

- **Project Creation:**
  - `projectCreator.js` (JS) orchestrates file generation using other generator modules.
  - `projectcreators/index.js` routes to appropriate language-specific creators (C, C++, Java, Python, Ruby, Rust) for appropriate file generation.
  - Generated files may be used by Ruby CLI for header management.

- **Header Management:**
  - Ruby CLI (`tsi_header_cli.rb`) parses, generates, and extracts headers using core library modules.
  - Unified test suite (`unified_test_suite.js`) validates all header functionality across 101+ languages.
  - Utility scripts and tests (`scripts/`, `spec/`) validate header functionality.

- **Quality Assurance:**
  - `comprehensive_test_suite.js` runs automated tests covering all supported languages and features.
  - Test results saved to `test_output/test_report.json` for analysis.
  - 100% success rate validation ensures reliability.

- **VS Code Extension:**
  - `extension.js` provides UI and command integration for header management in VS Code.
  - May invoke Ruby CLI or JS generators for backend tasks.

---

## Module Responsibilities Summary

| Module/Folder                | Responsibility                                      |
|------------------------------|-----------------------------------------------------|
| `unified_test_suite.js`      | Comprehensive testing for all 102+ languages        |
| `generators/`                | Code/project file generation                        |
| `generators/project/projectcreators/` | Language-specific project file creation (C, C++, Java, Python, Ruby, Rust) |
| `lib/tsi_header_cli.rb`      | Ruby CLI for header management (102+ languages)     |
| `lib/tsi_header/`            | Core header parsing/generation logic                |
| `src/extension.js`           | VS Code extension entry point                       |
| `src/tsiViewProvider.js`     | Custom VS Code views                                |
| `test_output/`               | Unified test suite output                           |
| `utils/contentAnalyzer.js`   | File content analysis                               |
| `scripts/`                   | Compilation and testing scripts                     |
| `spec/`                      | Ruby test suite                                     |
| `resources/`                 | Project images/icons                                |
| `examples/`                  | Example usage in C/Python                          |

---

## Algorithm for Adding New Language Support

### Step 1: Add Language to Ruby CLI (Header Generation)

**File:** `lib/tsi_header/delimiters.rb`

1. Add language delimiters to `LANGUAGE_DELIMITERS` hash:

```ruby
'your_language' => ['/*', '*/', '/*', '*/', '/*', '*/']  # 6-element array: [top_left, top_right, left, right, bottom_left, bottom_right]
```

1. Test the CLI:

```bash
ruby lib/tsi_header_cli.rb insert your_language test.txt
```

### Step 2: Add Language to JavaScript Generators (Code Base Generation)

**File:** `generators/codeBaseGenerators.js`

1. Add language case to the switch statement:

```javascript
case 'your_language':
    content = generateYourLanguageCodeBase();
    break;
```

1. Create language-specific generator file: `generators/languages/your_language.js`

```javascript
function generateYourLanguageCodeBase() {
    return `\n// Basic YourLanguage program\n\n// Main function\nconsole.log("Hello, World!");\nconsole.log("This is a basic YourLanguage program.");\n`;
}

module.exports = { generateYourLanguageCodeBase };
```

1. Import the new generator at the top of `codeBaseGenerators.js`:

```javascript
const { generateYourLanguageCodeBase } = require('./languages/your_language');
```

### Step 3: Add Language to Class Generators (Optional)

**File:** `generators/classGenerators.js`

1. Add language case to the switch statement if it supports classes:

```javascript
case 'your_language':
    content = generateYourLanguageClass(className);
    break;
```

1. Implement the class generation function.

### Step 4: Add Language to Project Creation (Optional)

**File:** `generators/project/projectcreators/index.js`

1. Add language case to the switch statement:

```javascript
case 'your_language':
    return createYourLanguageProject(projectName, projectDir);
```

1. Create project creator file: `generators/project/projectcreators/yourLanguageProjectCreator.js`

### Step 5: Add Language to Build System Generation (Optional)

**File:** `generators/project/buildSystemGenerator.js`

1. Add language case to the switch statement:

```javascript
case 'your_language':
    return createYourLanguageBuildSystem(projectName, projectDir);
```

1. Create build system file: `generators/project/buildsystems/yourLanguageBuildSystem.js`

### Step 6: Update Test Suite

**File:** `unified_test_suite.js`

1. Add language to appropriate arrays:
   - `HEADER_LANGUAGES` (for header testing)
   - `CODEBASE_LANGUAGES` (for code base testing)
   - `CLASS_LANGUAGES` (for class testing, if applicable)
   - `PROJECT_LANGUAGES` (for project testing, if applicable)

### Step 7: Test the New Language

1. Run the unified test suite:

```bash
node unified_test_suite.js
```

1. Verify the new language appears in test results and passes all applicable tests.

### Step 8: Update Documentation

1. Update `README.md` with the new language support
1. Update `CHANGELOG.md` with the new language addition
1. Update this `PROJECT_MAP.md` if new files/modules were added

---

## Quality Assurance & Testing

### Unified Test Suite

- **Location**: `unified_test_suite.js`
- **Coverage**: 102+ programming languages for headers and code bases, 13 for classes, 6 for projects
- **Test Types**:
  - Header insertion validation
  - Code base generation validation
  - Class creation validation (13 languages including C++)
  - Project creation validation (6 languages)
  - Extension API integration testing with VS Code mocking
  - Comment marker validation
  - Syntax correctness checking
- **Output**: `test_output/unified_test_report.json`
- **Success Rate**: 100% (229/229 tests pass)

### Ruby Test Suite

- **Location**: `spec/`
- **Coverage**: Core Ruby library modules
- **Test Types**: Unit tests for configuration, delimiters, header operations

### JavaScript Test Suite

- **Location**: `test/` (if exists)
- **Coverage**: VS Code extension APIs and generators

---

## Notes

- This map will be updated as the project evolves.
- Use this document for debugging, onboarding, and architectural reference.
- The project now supports 102+ programming languages with comprehensive testing.
- Extension API testing is now fully implemented with VS Code mocking.
- C++ class generation testing is now fully implemented.
- Project creation testing covers 6 major languages (C, C++, Python, Java, Ruby, Rust).
- Ruby CLI API is the primary interface for header generation across all supported languages.
- Elm language support has been successfully added following the systematic algorithm.
