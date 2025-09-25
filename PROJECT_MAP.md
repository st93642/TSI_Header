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
  - `codeBaseGenerators.js`: Generates codebase scaffolding.
  - `project/`
    - `buildSystemGenerator.js`: Generates build system files (e.g., Makefile, CMake).
    - `documentationGenerator.js`: Generates documentation files.
    - `gitIgnoreGenerator.js`: Generates `.gitignore` files.
    - `projectCreator.js`: Orchestrates project creation using other generators.

#### Interconnections

- `projectCreator.js` imports and coordinates other generators to create a full project structure.

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

#### Interconnections

- `tsi_header_cli.rb` uses `tsi_header.rb` as its backend.
- `tsi_header.rb` loads modules from `tsi_header/` for specific tasks.
- `header_extractor.rb` and `header_generator.rb` use `delimiters.rb` and `header_info.rb` for parsing and data management.

### 4. `resources/`

- **Purpose:** Project icons and images for UI/branding.
- **Files:** Various PNG and SVG images.

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

### 7. `src/`

- **Purpose:** JavaScript source code for VS Code extension.
- **Files:**
  - `extension.js`: Main entry point for the VS Code extension.
  - `tsiViewProvider.js`: Provides custom views in the VS Code UI.

#### Interconnections

- `extension.js` registers commands and UI elements, using `tsiViewProvider.js` for custom views.

### 8. `utils/`

- **Purpose:** Utility functions for code analysis.
- **Files:**
  - `contentAnalyzer.js`: Analyzes file content for headers and structure.

---

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
| `lib/tsi_header_cli.rb`      | Ruby CLI for header management                      |
| `lib/tsi_header/`            | Core header parsing/generation logic                |
| `src/extension.js`           | VS Code extension entry point                       |
| `src/tsiViewProvider.js`     | Custom VS Code views                                |
| `utils/contentAnalyzer.js`   | File content analysis                               |
| `scripts/`                   | Compilation and testing scripts                     |
| `spec/`                      | Ruby test suite                                     |
| `resources/`                 | Project images/icons                                |
| `examples/`                  | Example usage in C/Python                          |

---

## Notes

- This map will be updated as the project evolves.
- Use this document for debugging, onboarding, and architectural reference.
