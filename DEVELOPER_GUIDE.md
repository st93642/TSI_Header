# Uni Header Extension - Developer Guide

## Getting Started

This guide will help you set up a development environment for the Uni Header extension and understand how to contribute to the project.

## Prerequisites

### Required Software

- **Node.js**: Version 16 or later
- **VS Code**: Version 1.74.0 or later
- **Ruby**: Version 2.7 or later (for header generation)
- **Git**: For version control

### Optional Dependencies

- **GCC/Clang**: For C/C++ learning exercises
- **Rust Compiler**: For Rust learning exercises
- **Python**: For Python learning exercises

## Development Setup

### 1. Clone the Repository

```bash
git clone https://github.com/st93642/TSI_Header.git
cd TSI_Header
```

### 2. Install Dependencies

```bash
npm install
```

### 3. Install Ruby Dependencies

```bash
# Install required Ruby gems
gem install bundler
bundle install
```

### 4. Build the Extension

```bash
# Compile the extension
npm run compile

# Watch for changes during development
npm run watch
```

### 5. Run Tests

```bash
# Run all tests
npm test

# Run specific test suites
node studyMode/timer.test.js
ruby spec/unified_test.rb
```

## Project Structure

```
/workspace/
├── core/                    # Core functionality
│   ├── generators/         # Code generation templates
│   │   ├── classGenerators.js
│   │   ├── codeBaseGenerators.js
│   │   ├── languages/     # 122+ language templates
│   │   └── project/       # Project scaffolding
│   ├── lib/               # Ruby CLI and utilities
│   │   ├── tsi_header.rb
│   │   ├── tsi_header_cli.rb
│   │   └── tsi_header/
│   ├── src/               # Main extension logic
│   │   ├── extension.js   # Main extension file (122k+ lines)
│   │   ├── studyModeExtension.js
│   │   └── tsiViewProvider.js
│   └── utils/             # Content analysis utilities
│       └── contentAnalyzer.js
├── learn/                  # Learning platform
│   ├── curriculum/        # Language curricula
│   │   ├── c/            # C curriculum (41 files)
│   │   ├── cpp/          # C++ curriculum (79 files)
│   │   ├── dsa_cpp/      # C++ DSA curriculum (61 files)
│   │   ├── ruby/         # Ruby curriculum (112 files)
│   │   ├── rust/         # Rust curriculum (103 files)
│   │   ├── git/          # Git curriculum (37 files)
│   │   └── mathematics/  # Mathematics curriculum (42 files)
│   ├── lib/              # Learning management
│   │   ├── learn_manager.js
│   │   ├── progress_tracker.js
│   │   └── exercise_runner.js
│   └── tests/            # Learning tests
│       └── no_md_tables.js
├── calendar/              # Calendar functionality
│   ├── resources/        # Calendar UI assets
│   │   ├── calendar.css
│   │   ├── calendar.html
│   │   └── calendar.js
│   └── src/             # Calendar logic
│       ├── calendarManager.js
│       ├── calendarDataManager.js
│       ├── calendarEventManager.js
│       └── calendarWebviewProvider.js
├── studyMode/            # Pomodoro timer
│   ├── timer.js         # Core timer logic
│   ├── timer.test.js    # Timer tests
│   └── package.json     # Study mode dependencies
├── top/                  # Odin Project integration
│   ├── curriculum.json  # Odin Project curriculum (2.4k+ lines)
│   └── odin_manager.js  # Odin Project management
├── resources/            # Extension resources
│   ├── tsi.png         # Extension icon
│   ├── main-picture.png # Overview image
│   ├── calendar.png    # Calendar image
│   ├── highlightjs/    # Syntax highlighting assets
│   └── dsa_cpp/        # DSA visual assets (74 SVG files)
├── scripts/             # Build scripts
│   ├── compile.rb      # Ruby compilation script
│   └── watch.rb        # File watching script
├── spec/               # Test specifications
│   └── unified_test.rb # Ruby test suite
├── TEST_Suite/         # Test suite
│   ├── full_test_suite.rb
│   ├── test_class_creation.rb
│   ├── test_codebase_insertion.rb
│   ├── test_header_insertion.rb
│   ├── test_learn_module.rb
│   └── test_project_creation.rb
├── package.json        # Extension manifest
├── README.md          # User documentation
└── LICENSE            # MIT License
```

## Development Workflow

### 1. Making Changes

#### Core Functionality

- Edit files in `/core/src/` for main extension logic
- Update generators in `/core/generators/` for code templates
- Modify utilities in `/core/utils/` for helper functions

#### Learning Module

- Add curricula in `/learn/curriculum/[language]/`
- Update learning logic in `/learn/lib/`
- Add tests in `/learn/tests/`

#### Calendar Module

- Modify calendar logic in `/calendar/src/`
- Update UI assets in `/calendar/resources/`

#### Study Mode

- Edit timer logic in `/studyMode/timer.js`
- Update tests in `/studyMode/timer.test.js`

### 2. Testing Changes

#### Unit Tests

```bash
# Test study mode timer
node studyMode/timer.test.js

# Test learning module
node learn/tests/no_md_tables.js
```

#### Integration Tests

```bash
# Run Ruby test suite
ruby spec/unified_test.rb

# Run full test suite
ruby TEST_Suite/full_test_suite.rb
```

#### Manual Testing

1. Open VS Code
2. Press `F5` to launch extension in debug mode
3. Test functionality in the new VS Code window
4. Check console for errors and debug output

### 3. Building and Packaging

#### Development Build

```bash
npm run compile
```

#### Production Build

```bash
# Install vsce (VS Code Extension Manager)
npm install -g vsce

# Package the extension
vsce package
```

## Code Style Guidelines

### JavaScript/Node.js

- Use ES6+ features
- Follow async/await patterns
- Use meaningful variable names
- Add JSDoc comments for public methods
- Handle errors gracefully

```javascript
/**
 * Generates a TSI header for a file
 * @param {string} languageId - VS Code language ID
 * @param {string} fileName - File name
 * @param {Object} env - Environment variables
 * @returns {Promise<Object>} - {success: boolean, header?: string, message?: string}
 */
async generateHeader(languageId, fileName, env = {}) {
    try {
        // Implementation
        return { success: true, header: generatedHeader };
    } catch (error) {
        return { success: false, message: error.message };
    }
}
```

### Ruby

- Follow Ruby style guidelines
- Use meaningful method names
- Add comments for complex logic
- Handle exceptions properly

```ruby
# Generates a TSI header for the given language and file
# @param language [String] The programming language
# @param filename [String] The name of the file
# @return [Hash] Result hash with success status and header content
def generate_header(language, filename)
  begin
    # Implementation
    { success: true, header: generated_header }
  rescue => e
    { success: false, message: e.message }
  end
end
```

## Adding New Features

### 1. New Language Support

#### Add Language Template

1. Create template file in `/core/generators/languages/[language].js`
2. Define language-specific header format
3. Add to language detection logic

```javascript
// core/generators/languages/example.js
module.exports = {
    generateHeader: (fileName, env) => {
        return `// TSI Header for ${fileName}
// Generated: ${new Date().toISOString()}
// Author: ${env.TSI_USERNAME || 'Unknown'}`;
    }
};
```

#### Update Language Detection

```javascript
// core/index.js
detectLanguageFromExtension(languageId, fileName) {
    const ext = path.extname(fileName).toLowerCase();
    if (ext === '.example') {
        return 'example';
    }
    // ... existing logic
}
```

### 2. New Learning Curriculum

#### Create Curriculum Structure

```
/learn/curriculum/new_language/
├── lessons/
│   ├── lesson1.json
│   ├── lesson2.json
│   └── ...
├── exercises/
│   ├── exercise1.json
│   ├── exercise2.json
│   └── ...
└── README.md
```

#### Curriculum File Format

```json
{
    "id": "lesson1",
    "title": "Introduction to New Language",
    "description": "Learn the basics of the new language",
    "duration": 30,
    "difficulty": "beginner",
    "exercises": [
        {
            "id": "hello-world",
            "title": "Hello World",
            "description": "Write your first program",
            "tests": ["console.log('Hello, World!')"],
            "solution": "console.log('Hello, World!');"
        }
    ]
}
```

### 3. New Calendar Features

#### Add New Event Type

1. Update `CalendarEventManager` class
2. Add UI components in webview
3. Update data persistence logic

```javascript
// calendar/src/calendarEventManager.js
async addCustomEventType(eventData) {
    // Implementation for new event type
    const event = {
        id: Date.now().toString(),
        type: 'custom',
        ...eventData,
        createdAt: new Date().toISOString()
    };
    
    await this.dataManager.saveEvent(event);
    return event;
}
```

## Debugging

### VS Code Debugging

1. Open the project in VS Code
2. Go to Run and Debug view (Ctrl+Shift+D)
3. Select "Launch Extension" configuration
4. Set breakpoints in your code
5. Press F5 to start debugging

### Console Logging

```javascript
// Use console.log for debugging
console.log('Debug info:', debugData);

// Use VS Code's output channel
const outputChannel = vscode.window.createOutputChannel('Uni Header');
outputChannel.appendLine('Debug message');
```

### Error Handling

```javascript
try {
    // Risky operation
    const result = await someAsyncOperation();
} catch (error) {
    console.error('Operation failed:', error);
    vscode.window.showErrorMessage(`Error: ${error.message}`);
}
```

## Performance Optimization

### Large File Handling

The main extension file (`core/src/extension.js`) is very large (122k+ lines). Consider:

1. Breaking it into smaller modules
2. Using lazy loading for features
3. Implementing code splitting

### Memory Management

```javascript
// Properly dispose of resources
dispose() {
    if (this.timerInterval) {
        clearInterval(this.timerInterval);
    }
    if (this.statusBarItem) {
        this.statusBarItem.dispose();
    }
}
```

### Caching

```javascript
// Implement caching for expensive operations
const cache = new Map();

async getCachedData(key) {
    if (cache.has(key)) {
        return cache.get(key);
    }
    
    const data = await expensiveOperation();
    cache.set(key, data);
    return data;
}
```

## Testing Strategies

### Unit Testing

```javascript
// studyMode/timer.test.js
const { StudyModeTimer } = require('./timer.js');

describe('StudyModeTimer', () => {
    test('should start timer correctly', () => {
        const timer = new StudyModeTimer(null, null, { workDuration: 25 });
        timer.start();
        expect(timer.isRunning).toBe(true);
    });
});
```

### Integration Testing

```javascript
// Test complete workflows
test('complete learning workflow', async () => {
    const learn = new Learn(context, vscode);
    await learn.startLearning('ruby');
    
    const exercise = { /* exercise data */ };
    const result = await learn.runExercise('ruby', exercise);
    
    expect(result.passed).toBe(true);
});
```

### End-to-End Testing

```javascript
// Test user interactions
test('user can create project', async () => {
    // Simulate user command
    await vscode.commands.executeCommand('tsiheader.createCProject');
    
    // Verify project was created
    const files = await vscode.workspace.findFiles('**/*.c');
    expect(files.length).toBeGreaterThan(0);
});
```

## Contributing

### Pull Request Process

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

### Code Review Checklist

- [ ] Code follows style guidelines
- [ ] Tests are included and passing
- [ ] Documentation is updated
- [ ] No breaking changes (or properly documented)
- [ ] Performance impact is considered

### Issue Reporting

When reporting issues, include:

- VS Code version
- Extension version
- Steps to reproduce
- Expected vs actual behavior
- Console output/logs

## Deployment

### Version Management

1. Update version in `package.json`
2. Update changelog
3. Tag the release
4. Publish to VS Code Marketplace

### Release Process

```bash
# Update version
npm version patch  # or minor, major

# Build and package
npm run compile
vsce package

# Publish
vsce publish
```

## Troubleshooting

### Common Issues

#### Extension Not Activating

- Check VS Code version compatibility
- Verify all dependencies are installed
- Check console for error messages

#### Ruby CLI Errors

- Ensure Ruby 2.7+ is installed
- Verify Ruby gems are installed
- Check file permissions

#### Learning Module Issues

- Verify curriculum files exist
- Check file permissions
- Ensure compilers are installed for exercises

#### Calendar Import/Export Issues

- Check file format compatibility
- Verify network connectivity for URL imports
- Check SMTP configuration for email notifications

### Getting Help

- Check existing issues on GitHub
- Review the documentation
- Ask questions in discussions
- Contact maintainers

## Resources

### Documentation

- [VS Code Extension API](https://code.visualstudio.com/api)
- [VS Code Extension Guidelines](https://code.visualstudio.com/api/extension-guides/overview)
- [Node.js Documentation](https://nodejs.org/docs/)
- [Ruby Documentation](https://ruby-doc.org/)

### Tools

- [VS Code Extension Generator](https://code.visualstudio.com/api/get-started/your-first-extension)
- [VS Code Extension Manager (vsce)](https://code.visualstudio.com/api/working-with-extensions/publishing-extension)
- [Node.js Testing Framework (Jest)](https://jestjs.io/)
- [Ruby Testing Framework (RSpec)](https://rspec.info/)

### Community

- [VS Code Extension Community](https://github.com/microsoft/vscode-extension-samples)
- [Stack Overflow](https://stackoverflow.com/questions/tagged/vscode-extensions)
- [VS Code Discord](https://discord.gg/vscode)
