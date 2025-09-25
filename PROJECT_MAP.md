# TSI_Header Project Map

This document provides a comprehensive overview of the TSI_Header project, detailing all modules, APIs, interfaces, and their interconnections. It serves as a reference for debugging and development.

---

## Top-Level Structure

- `CHANGELOG.md`, `LICENSE`, `README.md`: Project documentation and metadata.
- `package.json`: Node.js package configuration.
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
  - `classGenerators.js`: Generates class templates.
  - `codeBaseGenerators.js`: Generates codebase scaffolding and imports language-specific generators.
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
      - `cpp.js`: C++ Makefile generation.
      - `python.js`: Python Makefile generation.
      - `java.js`: Java Maven/Gradle build files generation.
    - `documentationGenerator.js`: Generates documentation files.
    - `gitIgnoreGenerator.js`: Generates `.gitignore` files.
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

- **Purpose:** Ruby CLI and core library for TSI_Header.
- **Files:**
  - `tsi_header_cli.rb`: Command-line interface for TSI_Header.
  - `tsi_header.rb`: Main Ruby library entry point.
  - `tsi_header/`
    - `configuration.rb`: Handles configuration parsing and management.
    - `delimiters.rb`: Defines header delimiters for parsing.
    - `header_extractor.rb`: Extracts headers from source files.
    - `header_generator.rb`: Generates headers for source files.
    - `header_info.rb`: Data structure for header information.
    - `header_parser.rb`: Parses header blocks from files.

#### Ruby Library Interconnections

- `tsi_header_cli.rb` uses `tsi_header.rb` as its backend.
- `tsi_header.rb` loads modules from `tsi_header/` for specific tasks.
- `header_extractor.rb` and `header_generator.rb` use `delimiters.rb` and `header_info.rb` for parsing and data management.

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
  - `production_test.rb`, `simple_test.rb`, `stress_test.rb`: Different test scenarios.
  - `unit/`: Unit tests for individual Ruby modules.
    - `configuration_spec.rb`: Tests for configuration module.
    - `delimiters_spec.rb`: Tests for delimiters module.
    - `header_extractor_spec.rb`: Tests for header extraction.
    - `header_generator_spec.rb`: Tests for header generation.
    - `header_info_spec.rb`: Tests for header info data structure.
    - `header_parser_spec.rb`: Tests for header parsing.
    - `cli_integration_spec.rb`: Tests for CLI integration.

### 7. `src/`

- **Purpose:** JavaScript source code for VS Code extension.
- **Files:**
  - `extension.js`: Main entry point for the VS Code extension.
  - `tsiViewProvider.js`: Provides custom views in the VS Code UI.

#### VS Code Extension Interconnections

- `extension.js` registers commands and UI elements, using `tsiViewProvider.js` for custom views.

### 8. `utils/`

- **Purpose:** Utility functions for code analysis.
- **Files:**
  - `contentAnalyzer.js`: Analyzes file content for headers and structure.

### 9. `test/`

- **Purpose:** JavaScript test suite for extension APIs, generators, and cross-language validation.
- **Files:**
  - `extension_api.test.js`: Tests for VS Code extension APIs.
  - `generators.test.js`: Tests for class and code base generators.
  - `cross_language.test.js`: Cross-language header and class validation.
  - `vscode_integration.test.js`: VS Code integration tests.

## APIs & Interfaces

### Ruby CLI/API

- **Entry Point:** `lib/tsi_header_cli.rb`
- **Interfaces:**
  - Command-line arguments for header generation, extraction, and parsing.
  - Uses classes from `lib/tsi_header/` for implementation.

### JavaScript Extension API

- **Entry Point:** `src/extension.js`
- **Interfaces:**
  - VS Code commands and events.
  - Custom views via `tsiViewProvider.js`.

### Generators API

- **Entry Point:** `generators/project/projectCreator.js`
- **Interfaces:**
  - Functions to create project files, documentation, and build systems.
  - Calls other generator modules for specific tasks.

---

## Interconnections & Data Flow

- **Project Creation:**
  - `projectCreator.js` (JS) orchestrates file generation using other generator modules.
  - `projectcreators/index.js` routes to appropriate language-specific creators (C projects get C-only files, C++ projects get C++ classes).
  - Generated files may be used by Ruby CLI for header management.

- **Header Management:**
  - Ruby CLI (`tsi_header_cli.rb`) parses, generates, and extracts headers using core library modules.
  - Utility scripts and tests (`scripts/`, `spec/`) validate header functionality.

- **VS Code Extension:**
  - `extension.js` provides UI and command integration for header management in VS Code.
  - May invoke Ruby CLI or JS generators for backend tasks.

---

## Module Responsibilities Summary

| Module/Folder                | Responsibility                                      |
|------------------------------|-----------------------------------------------------|
| `generators/`                | Code/project file generation                        |
| `generators/project/projectcreators/` | Language-specific project file creation (C, C++, Java, Python, Ruby, Rust) |
| `lib/tsi_header_cli.rb`      | Ruby CLI for header management                      |
| `lib/tsi_header/`            | Core header parsing/generation logic                |
| `src/extension.js`           | VS Code extension entry point                       |
| `src/tsiViewProvider.js`     | Custom VS Code views                                |
| `utils/contentAnalyzer.js`   | File content analysis                               |
| `scripts/`                   | Compilation and testing scripts                     |
| `spec/`                      | Ruby test suite                                     |
| `test/`                      | JavaScript test suite                               |
| `resources/`                 | Project images/icons                                |
| `examples/`                  | Example usage in C/Python                          |

---

## Notes

- This map will be updated as the project evolves.
- Use this document for debugging, onboarding, and architectural reference.
