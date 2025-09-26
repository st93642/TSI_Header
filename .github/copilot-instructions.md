# TSI Header Extension - AI Coding Guidelines

## Architecture Overview

**Hybrid Architecture**: VS Code extension (JavaScript/Node.js) with Ruby backend for header generation. The extension provides professional code headers and scaffolding for 120+ programming languages with institutional branding.

**Core Components**:
- **Ruby CLI** (`lib/tsi_header_cli.rb`): Primary header generation engine supporting 120+ languages
- **VS Code Extension** (`src/extension.js`): UI integration, command registration, and code generators
- **Modular Generators** (`generators/`): Language-specific code and project scaffolding
- **Unified Test Suite** (`unified_test_suite.js`): Comprehensive validation across all languages

## Critical Workflows

### Testing
```bash
npm test                    # Run all 277 tests (277/277 must pass)
node unified_test_suite.js  # Direct test execution
ruby spec/unified_test.rb    # Ruby backend tests only
```
**Requirement**: All tests must pass with 100% success rate. Never commit with failing tests.

### Building
```bash
npm run compile            # Ruby compilation script
ruby scripts/compile.rb    # Direct compilation
```

### Header Operations
```bash
ruby lib/tsi_header_cli.rb insert <language> <file>  # Generate new header
ruby lib/tsi_header_cli.rb update <language> <file>  # Update existing header
```

## Configuration Patterns

**User Credentials**: Required for header generation, resolved in order:
1. VS Code settings (`tsiheader.username`, `tsiheader.email`)
2. Git config (`git config user.name/email`)
3. Environment variables (`TSI_USERNAME`, `TSI_EMAIL`)

**Language Support**: 
- **Headers**: 120+ languages via Ruby CLI with language-specific delimiters
- **Classes**: 13 OOP languages (Java, C++, C#, Python, JS, Kotlin, PHP, TS, Ruby, Go, Swift, Dart, Scala)
- **Projects**: 6 languages (C, C++, Java, Python, Ruby, Rust) with full scaffolding

## Code Generation Patterns

### Adding Language Support

**For Headers** (Ruby backend):
1. Add delimiters to `lib/tsi_header/delimiters.rb` LANGUAGE_DELIMITERS hash
2. Test: `ruby lib/tsi_header_cli.rb insert <lang> test.txt`

**For Code Generation** (JavaScript):
1. Add case to `generators/codeBaseGenerators.js` switch statement
2. Create `generators/languages/<lang>.js` with `generate<Lang>CodeBase()` function
3. Import and export in main generator file

**For Classes** (if OOP language):
1. Add case to `generators/classGenerators.js`
2. Implement `generate<Lang>Class(className)` function

### Generator Structure
```javascript
// generators/languages/example.js
function generateExampleCodeBase() {
    return `\n// Example program\n\n// Main logic\nconsole.log("Hello, World!");\n`;
}

module.exports = { generateExampleCodeBase };
```

## Quality Standards

**Testing Requirements**:
- Update `HEADER_LANGUAGES`, `CODEBASE_LANGUAGES`, `CLASS_LANGUAGES` arrays in test suite
- Validate syntax correctness and TSI header format
- Ensure 100% test pass rate

**Code Style**:
- Ruby: Standard Ruby conventions with TSI header comments
- JavaScript: Modular exports, error handling with try/catch
- Consistent date formatting: `"Sep 24 2025 02:32"` (matches Ruby generator)

**Error Handling**:
- Return structured objects: `{success: true/false, content/message}`
- Graceful fallbacks for missing credentials
- Silent failures for auto-update operations

## Key Files Reference

- `lib/tsi_header/delimiters.rb`: Language-specific comment markers
- `generators/classGenerators.js`: OOP class templates
- `generators/codeBaseGenerators.js`: Boilerplate code patterns
- `src/extension.js`: VS Code API integration
- `unified_test_suite.js`: Comprehensive validation
- `PROJECT_MAP.md`: Detailed architecture documentation

## UI/UX Patterns

### IDE Integration

**Activity Bar Position**: Extension appears as mortar board icon (🎓) in VS Code's Activity Bar (left sidebar)

**View Structure**:
- **TSI Commands**: Core functionality (headers, classes, code bases)
- **TSI Projects**: Project creation and templates

### Menu Options & Commands

**Core Commands** (in TSI Commands view):
- `Insert Header` - Generate new TSI header for current file
- `Update Header` - Refresh existing header timestamps
- `Add Class` - Create class with header (13 OOP languages)
- `Add Code Base` - Generate boilerplate code with header (120+ languages)
- `Create TSI Project` - Full project scaffolding (6 languages)

**Project Templates** (in TSI Projects view):
- 🚀 Create New Project
- 📋 Recent Projects (placeholder)
- ⚙️ Project Templates (C/C++, C projects)

### User Interaction Algorithm

**For Header Operations**:
1. Check active editor exists
2. Validate user credentials (VS Code settings → Git config → Environment)
3. Show configuration dialog if credentials missing
4. Execute Ruby CLI with language detection
5. Insert/update header at file beginning
6. Show success message

**For Class/Code Base Generation**:
1. Check active editor and language support
2. Generate TSI header via Ruby CLI
3. Show input dialog for class name (if applicable)
4. Generate language-specific code via JavaScript generators
5. Handle file conflicts with warning dialog
6. Insert at cursor or replace content
7. Show success feedback

**For Project Creation**:
1. Show input dialog for project name
2. Select language from supported list
3. Generate directory structure and files
4. Create build systems and documentation
5. Show completion message

### Adding/Changing UI Options

**To Add New Command**:
1. Register in `package.json` contributes.commands
2. Implement in `src/extension.js` with `registerCommand`
3. Add to tree view in `src/tsiViewProvider.js`
4. Add icon and tooltip via `TSITreeItem` class

**To Add New View**:
1. Define in `package.json` contributes.views
2. Create new `DataProvider` class extending tree patterns
3. Register in `extension.js` with `registerTreeDataProvider`

**Configuration Settings**:
- Add to `package.json` contributes.configuration.properties
- Access via `vscode.workspace.getConfiguration('tsiheader')`
- Support resource scope for per-workspace settings

### User Feedback Patterns

**Success Messages**: `vscode.window.showInformationMessage()` for completions
**Warnings**: `vscode.window.showWarningMessage()` for conflicts with action buttons
**Errors**: `vscode.window.showErrorMessage()` for failures
**Input Dialogs**: `vscode.window.showInputBox()` for user input with validation