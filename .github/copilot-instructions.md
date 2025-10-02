# TSI Header - AI Coding Agent Instructions

## ⚠️ CRITICAL RULES
**DO NOT CREATE SUMMARY FILES** - Never create files like SUMMARY.md, STATUS.md, CHANGES.md, EXERCISES_SUMMARY.md, or any documentation files unless explicitly requested by the user.
**ALWAYS APPLY TDD** - Before changing implementation code, add or update automated tests that should initially fail, then implement the changes to make them pass and rerun the suite.

Learn module
- **Chapter Text Source & Formatting Reminder**: Every chapter rewrite must draw primary source material from `ref.txt`, paraphrasing and expanding it, and all code snippets must stay in plain text with stylistic emphasis (color/italic/bold) instead of fenced code blocks—apply this consistently to every chapter in the curriculum.
- Exercises to be added from the reference text (or online source, or your database) must include TDD-style tests that initially fail, then implement the solution to make them pass. Each exercise should be self-contained and runnable.


## Project Overview
VS Code extension for code generation, header management, and productivity features supporting 147+ programming languages. Dual-language architecture: Ruby backend for header generation, JavaScript frontend for VS Code integration.

## Architecture
- **Ruby Backend** (`core/lib/`): Header generation with language-specific comment delimiters
- **JavaScript Frontend** (`core/src/`): VS Code API integration, UI, command registration
- **Communication**: JS spawns `core/lib/tsi_header_cli.rb` via `execSync`, receives JSON responses

## Key Components
1. **Header Generation**: Ruby processes language IDs → TSI-branded headers with proper syntax
2. **Code Generation**: JS modules in `core/generators/` create templates for 147+ languages
3. **Project Scaffolding**: Full project creation for C, C++, Python, Java, Rust, Ruby, PHP
4. **Study Mode**: Pomodoro timer (`studyMode/`) with persistent state across VS Code sessions
5. **Learn Mode**: Interactive learning platform (`learn/`) with curriculum, exercises, progress tracking

## Critical Patterns

### Header Generation Flow
```javascript
// core/src/extension.js
const result = execSync(`ruby "${cliPath}" insert "${languageId}" "${fileName}"`, { env });
const response = JSON.parse(result); // {success: bool, header?: string, message?: string}
```

### Study Mode Timer Architecture
- **Elapsed Time Tracking**: Stores `elapsedTime` (milliseconds) as source of truth, not wall clock times
- **Monotonic Timing**: Uses `process.hrtime.bigint()` for accurate timing via `lastTickTime`
- **Persistence**: Saves `elapsedTime` to `context.globalState`, survives VS Code restarts
- **Key Files**: `studyMode/timer.js`, `core/src/studyModeExtension.js`

### Language Support System
- **147+ languages**: Each has module in `core/generators/languages/` (e.g., `python.js`)
- **Export pattern**: `module.exports = { generate<Language>CodeBase }`
- **Ruby side**: Auto-supports if delimiters exist in `core/lib/tsi_header/delimiters.rb`

### Configuration Precedence
1. VS Code settings (`tsiheader.username`, `tsiheader.email`)
2. Git config (`git config user.name`, `git config user.email`)
3. Environment variables (`TSI_USERNAME`, `TSI_EMAIL`)

## Development Workflows

### Testing
```bash
ruby TEST_Suite/full_test_suite.rb    # 394 tests, 100% coverage
cd studyMode && npm test              # Timer tests
```

### Building
```bash
npm run compile  # scripts/compile.rb - copies files, no transpilation
npm run watch    # Monitors changes for auto-copy
```

### Packaging & Deployment
```bash
# Package the extension (runs prepublish/compile automatically)
npx vsce package

# Install the freshly-built VSIX into the local VS Code instance
code --install-extension tsi-header-<version>.vsix --force

# Publish to the Marketplace (VSCE_PAT is already configured)
npx vsce publish
```
Notes:
- `vsce package` emits `tsi-header-<version>.vsix` in the repo root; update `<version>` if `package.json` changes.
- Local installs require the VSIX filename to match the generated artifact exactly.
- `npx vsce publish` works with the repo’s baked-in authentication—no extra environment variables or PAT prompts needed.

### Adding Language Support
1. Create `core/generators/languages/newlang.js` with `generate<Language>CodeBase()` function
2. Add import and case in `core/generators/codeBaseGenerators.js`
3. Test with `ruby TEST_Suite/test_header_insertion.rb`

## Project-Specific Conventions

### TSI Header Format
- **Width**: 79 characters total (including comment delimiters)
- **Logo**: ASCII "TTTTTTTT SSSSSSS II" right-aligned
- **Institution**: "Transport and Telecommunication Institute - Riga, Latvia"
- **Timestamps**: "Sep 23 2025 11:39" format (no seconds)

### File Structure Rules
- Entry point: `core/src/extension.js` (registered in `package.json` as `main`)
- Core API: `core/index.js` exports `TSICore` class
- Ruby CLI: `core/lib/tsi_header_cli.rb` is the ONLY Ruby file JS should call
- Tree views: `core/src/tsiViewProvider.js` manages Activity Bar panels

### Study Mode Integration
- **Persistence**: Timer state in `context.globalState`
- **User Confirmation**: All timer actions require modal dialog confirmation BEFORE state change
- **Status Bar**: Right side (priority 1000), shows phase emoji + countdown

### Learn Mode Integration
- **Architecture**: LearnManager (curriculum), ProgressTracker (gamification), ExerciseRunner (testing)
- **Curriculum**: JSON-based with modules, lessons (Markdown), exercises (JSON with tests)
- **Testing**: Dual modes - return value tests (default) and output tests (new, uses StringIO capture)
- **Progress**: Stored in `context.globalState` with key `learn_progress_{language}`

## Common Tasks

### Modifying Header Format
1. Edit `core/lib/tsi_header/header_generator.rb`
2. Update `generateFallbackHeader()` in `core/index.js`
3. Run `ruby TEST_Suite/full_test_suite.rb`

### Adding VS Code Command
1. Register in `package.json` under `contributes.commands`
2. Implement handler in `core/src/extension.js`: `vscode.commands.registerCommand()`
3. Add to `context.subscriptions.push()`

## Integration Points
- **VS Code APIs**: `vscode.workspace.fs`, `vscode.window.activeTextEditor`, `vscode.commands.registerCommand()`, `context.globalState`, `vscode.window.createStatusBarItem()`
- **External Dependencies**: Ruby 2.7+ required (not bundled), `execSync` for CLI calls
- **Extension Activation**: `activationEvents: ["*"]` - always active, lazy loads Study Mode

## Quality Assurance
- **100% language coverage**: All 147 languages tested
- **Cross-platform**: Linux, macOS, Windows (Ruby PATH varies)
- **No code quality enforcement module**: Previously existed, removed

## Debugging Tips
- Ruby errors in JS `catch` blocks - check `execSync` stderr
- Study Mode: Check `context.globalState.get('studyModeState')`
- Timer accuracy: Verify `elapsedTime` persisted and `lastTickTime` reset on resume
- Header format: Verify delimiter lengths in `delimiters.rb` (sum to 79 chars)
- Learn Mode: Check `context.globalState.get('learn_progress_{language}')`

## Project Overview

TSI Header is a VS Code extension providing code generation, header management, and productivity features for 147+ programming languages. It combines Ruby backend processing with JavaScript VS Code integration, supporting institutional branding for Transport and Telecommunication Institute (TSI).

## Architecture

### Dual-Language System
- **Ruby Backend** (`core/lib/`): Header generation engine with language-specific comment delimiters
- **JavaScript Frontend** (`core/src/`): VS Code extension API integration, UI, and command registration
- **Communication**: JS spawns Ruby CLI (`core/lib/tsi_header_cli.rb`) via `execSync`, receives JSON responses

### Key Components
1. **Header Generation**: Ruby processes language IDs → generates TSI-branded headers with proper comment syntax
2. **Code Generation**: JS modules in `core/generators/` create class templates and boilerplate for 147+ languages
3. **Project Scaffolding**: Full project creation for C, C++, Python, Java, Rust, Ruby, PHP with build files/docs
4. **Study Mode**: Standalone Pomodoro timer (`studyMode/`) with persistent state across VS Code sessions
5. **Learn Mode**: Interactive programming language learning platform (`learn/`) with structured curriculum, exercises, progress tracking, and gamification

## Critical Patterns

### Header Generation Flow
```javascript
// JS side: core/src/extension.js
const env = { TSI_USERNAME: username, TSI_EMAIL: email };
const result = execSync(`ruby "${cliPath}" insert "${languageId}" "${fileName}"`, { env });
const response = JSON.parse(result); // {success: bool, header?: string, message?: string}
```

```ruby
# Ruby side: core/lib/tsi_header_cli.rb
delimiters = Delimiters.for_language(language_id) # Returns [top_left, top_right, left, right, bottom_left, bottom_right]
header = HeaderGenerator.render_header(language_id, header_info)
```

### Study Mode Timer Architecture
- **Elapsed Time Tracking**: Timer stores `elapsedTime` (milliseconds) as the source of truth, not wall clock times
- **Monotonic Timing**: Uses `process.hrtime.bigint()` for accurate, drift-free timing between ticks via `lastTickTime`
- **Persistence Model**: Saves `elapsedTime` and `phaseStartTimestamp` (for logs only) to `context.globalState`
- **Resume Logic**: When resumed, resets `lastTickTime` and continues from stored `elapsedTime`
- **Wall Clock Separation**: `phaseStartTimestamp` used ONLY for session logging, never for timer calculations

**Key Files:**
- `studyMode/timer.js`: Core timer with `elapsedTime`/`lastTickTime` tracking
- `core/src/studyModeExtension.js`: Persistence integration with `loadPersistedState()`/`savePersistedState()`

### Language Support System
- **147+ languages**: Each has a module in `core/generators/languages/` (e.g., `python.js`, `c.js`)
- **Export pattern**: `module.exports = { generate<Language>CodeBase }` - used by `codeBaseGenerators.js`
- **Special handling**: Python uses frame format in Ruby generator; C/C++ have `.h`/`.hpp` header file creation

### Configuration Precedence
1. VS Code settings (`tsiheader.username`, `tsiheader.email`)
2. Git config (`git config user.name`, `git config user.email`)
3. Environment variables (`TSI_USERNAME`, `TSI_EMAIL`)

Users without config see helpful setup dialogs (not errors) when first using header commands.

## Development Workflows

### Testing
```bash
# Ruby tests (394 tests, 100% coverage)
ruby TEST_Suite/full_test_suite.rb

# Study Mode tests (Node.js)
cd studyMode && npm test
```

Test files validate:
- Header insertion/update for all 147 languages
- Class generation for 13 OOP languages
- Project creation for 7 languages
- Timer state persistence and analytics
- Learn Mode curriculum integrity (394 automated tests)

### Building
```bash
npm run compile  # Runs scripts/compile.rb - copies files, no transpilation
npm run watch    # Monitors file changes for auto-copy
```

No webpack/bundler - extension uses direct file structure. Ruby must be installed (v2.7+) in user PATH.

### Adding Language Support
1. Create `core/generators/languages/newlang.js` with `generate<NewLang>CodeBase()` function
2. Add import and case in `core/generators/codeBaseGenerators.js`
3. Ruby side auto-supports if delimiters exist in `core/lib/tsi_header/delimiters.rb`
4. Test with `ruby TEST_Suite/test_header_insertion.rb`

## Project-Specific Conventions

### TSI Header Format
- **Width**: 79 characters total (including comment delimiters)
- **Logo**: ASCII "TTTTTTTT SSSSSSS II" right-aligned in header
- **Institution**: "Transport and Telecommunication Institute - Riga, Latvia" footer
- **Timestamps**: "Sep 23 2025 11:39" format (no seconds in display)

### File Structure Rules
- Entry point: `core/src/extension.js` (registered in `package.json` as `main`)
- Core API: `core/index.js` exports `TSICore` class with manager interfaces
- Ruby CLI: `core/lib/tsi_header_cli.rb` is the ONLY Ruby file JS should call
- Tree views: `core/src/tsiViewProvider.js` manages Activity Bar panels

### Study Mode Integration
- **Persistence**: Timer state saved to `context.globalState` (survives VS Code restarts)
- **Elapsed Time Tracking**: Uses `elapsedTime` (milliseconds) instead of wall clock timestamps for accurate persistence
- **Monotonic Timing**: `process.hrtime.bigint()` for high-resolution, drift-free timing between ticks
- **User Confirmation**: All timer actions (pause/resume/stop) require user confirmation in dialog before executing
- **Analytics**: Session logs track completion rates, focus time (today/week/month/all-time)
- **Configuration**: All durations configurable in VS Code settings (1-120 minutes)
- **Status Bar**: Positioned on the right side (priority 1000), shows phase emoji (🍅 work, ☕ short break, 🏖️ long break) + countdown

### Learn Mode Integration
- **Tree View Section**: Collapsible "📚 Learn" panel in TSI Header Activity Bar
- **Interactive Platform**: Complete learning system with curriculum, exercises, and progress tracking in `learn/` directory
- **Architecture**: Three core modules - LearnManager (curriculum), ProgressTracker (gamification), ExerciseRunner (testing)
- **Curriculum Structure**: JSON-based curriculum with modules, lessons (Markdown), exercises (JSON), and solutions
- **Progress Persistence**: Uses `context.globalState` to track completed lessons, streaks, and achievements
- **Exercise System**: Automated testing for Ruby/Python/JavaScript with instant feedback and hints
- **Gamification**: Achievement system with streaks (3/7/30 days), completion milestones, and progress statistics
- **Webview Integration**: Lessons displayed in webview panels with interactive buttons for exercises
- **Command Pattern**: `tsiheader.learn<Language>` starts learning session, `tsiheader.runExerciseTests` validates solutions
- **File Generation**: Creates exercise files in workspace `learn_exercises/{language}/` with starter code
- **Testing**: 57 tests in `TEST_Suite/test_learn_module.rb` validate curriculum structure and functionality

### Recent Major Enhancements (October 2025)

#### Systematic Numbering System
All curriculum elements now use hierarchical numbering for easier issue tracking:
- **Modules**: "Module 1: Ruby Basics" through "Module 7: Practical Ruby"
- **Lessons**: "Lesson 1.1: Hello World", "Lesson 1.2: Variables", etc.
- **Exercises**: "Exercise 1.1: Hello World Exercise" matches lesson numbering exactly
- **Implementation**: 8 modules, 34 lessons, 34 exercises all numbered consistently
- **Files Updated**: `curriculum.json`, all 25 exercise JSON files in `learn/curriculum/ruby/exercises/`

#### Testing System Enhancement - Output Capture
ExerciseRunner now supports dual testing modes:
1. **Return Value Tests** (default): Tests what a method returns
   ```json
   {"name": "test_add", "call": "add(2, 3)", "expected": 5}
   ```
2. **Output Tests** (new): Tests printed output using StringIO capture
   ```json
   {"name": "test_hello", "call": "say_hello", "expected": "Hello!", "type": "output"}
   ```

**Technical Implementation**:
```ruby
# In exercise_runner.js -> generateRubyTestCode()
if (test.type === 'output') {
  rubyCode += `
    captured_output = StringIO.new
    original_stdout = $stdout
    $stdout = captured_output
    ${test.call}
    $stdout = original_stdout
    output = captured_output.string
    assert_equal(expected, output.chomp, "#{test.name} failed")
  `;
} else {
  rubyCode += `result = ${test.call}\n`;
  rubyCode += `assert_equal(expected, result, "#{test.name} failed")\n`;
}
```

#### Lesson-Exercise Alignment Fixes
- **Lesson 1.4 (hello_world)**: Fixed to test OUTPUT (puts/print/p) instead of return values
  - Changed all 5 tests to use `"type": "output"`
  - Updated starterCode comments: "Use puts to print" instead of "Return"
  - Aligned with lesson content that teaches `puts`, `print`, and `p` methods
- **Lesson 1.3 (p_method)**: Updated year references from 2024 to 2025
  - Exercise tests now expect `2025` as return value
  - Hints updated to reflect current year
  - StarterCode comment updated to 2025

#### Curriculum Expansion - Module 8: Professional Ruby (COMPLETED)
Added comprehensive professional Ruby curriculum with 5 new lessons covering advanced concepts:

1. **Lesson 8.1: Advanced Enumerables** - `each_with_index`, `reduce`, `select`, `reject`, `map`, `group_by`, `sort_by`, `take`, `drop`, `chunk`, `partition`, `zip`, method chaining patterns
2. **Lesson 8.2: Attribute Methods** - `attr_reader`, `attr_writer`, `attr_accessor` with proper encapsulation, validation, and custom accessors
3. **Lesson 8.3: Class vs Instance Methods** - Factory patterns, utility methods, class variables, singleton patterns, method visibility
4. **Lesson 8.4: Operator Overloading** - Arithmetic (`+`, `-`, `*`, `/`), comparison (`<=>`, `==`), indexing (`[]`, `[]=`), conversion methods (`to_s`, `to_i`, `to_f`)
5. **Lesson 8.5: Module Namespacing** - Code organization, conflict prevention, nested modules, service objects, configuration patterns

**Implementation Details**:
- **Curriculum Structure**: Updated `curriculum.json` from 7 modules/29 lessons to 8 modules/34 lessons
- **Content Creation**: 5 comprehensive lesson files (Markdown) with examples, best practices, and advanced patterns
- **Exercise System**: 5 exercise files with 6 tests each, covering practical applications of concepts
- **Solution Files**: Complete working solutions in JSON format with explanations and key learning points
- **Testing Integration**: Updated test expectations, added required fields (`description`, `tags`), maintained 100% test coverage
- **Quality Assurance**: Markdown linting compliance, JSON validation, automated testing for all new content

**Technical Achievements**:
- **Test Suite**: 394 total tests, 100% pass rate maintained throughout expansion
- **Curriculum Integrity**: All lessons, exercises, and solutions validated through automated testing
- **Professional Coverage**: Advanced enumerable methods, OOP design patterns, operator overloading, architectural organization
- **Learning Outcomes**: Students master real-world Ruby development skills from basic syntax to enterprise-level patterns

#### Curriculum Expansion - Module 7: Practical Ruby (COMPLETED)
Added 3 practical Ruby lessons to bridge theory and real-world application:

1. **Lesson 7.1: Symbols and Hash Keys** - Symbol vs String comparison, memory efficiency, hash key best practices
2. **Lesson 7.2: Range Objects** - Inclusive/exclusive ranges, iteration methods, practical use cases (dates, numbers, custom ranges)
3. **Lesson 7.3: Regular Expressions** - Pattern matching, common regex patterns, capture groups, substitution

**Implementation Details**:
- **Systematic Numbering**: All curriculum elements use hierarchical numbering (Module N: Title, Lesson N.M: Title)
- **Testing Enhancement**: Dual testing modes - return value tests (default) and output tests (StringIO capture)
- **Lesson-Exercise Alignment**: Fixed output-focused lessons to test printed output, data manipulation lessons to test return values
- **User Experience**: Cursor positioning, tab management, modal dialogs, navigation flow improvements

**Curriculum Statistics** (October 2025):
- Total Modules: 8 (Ruby Basics, Control Flow, Collections, Methods/Blocks, OOP, Advanced, Practical, Professional)
- Total Lessons: 34 (was 29)
- Total Exercises: 34 (all automated, 0 manual)
- Total Tests: 394+ automated test cases
- Success Rate: 100% test coverage maintained
- Success Rate: 100% test coverage maintained

#### User Experience Improvements
1. **Cursor Positioning**: After opening exercise, cursor automatically positioned on empty line after "# Your code here" comment
2. **Tab Management**: Fixed `closeAllLearnTabs()` to properly close webview panels and exercise files
3. **Modal Dialogs**: All user interactions now use `{ modal: true }` for better UX
4. **Navigation Flow**: "Next Lesson" button navigates directly to next lesson after completion
5. **Font Consistency**: All code blocks use 13px font, body text uses 14px

#### Exercise Quality Standards
Every exercise now includes:
- **Clear Requirements**: Specific tasks with input/output examples
- **Starter Code**: Pre-structured with comments and empty lines for coding
- **Automated Tests**: 3-5 tests per exercise covering edge cases
- **Progressive Hints**: 3-5 hints per exercise, increasing in specificity
- **Difficulty Tags**: beginner, intermediate, advanced labels
- **Empty Line After Comments**: Ensures cursor lands on writable line

### C/C++ Dual-Language Exercise Clarity (Oct 2025)
- **Explicit Starter Instructions**: When creating or updating any C/C++ exercise (`learn/curriculum/cpp/exercises/*.json`), write starter-code comments that spell out the exact values learners must declare, the order of computations, and the precise output lines (including capitalization and punctuation). Never rely on lesson text alone.
- **Primary Reference**: Use the repository's `Beginning_C++17_Novice_to_Professional_Fifth_Edition_Ivor_Horton.pdf` as the main source when writing or updating C++ lessons, exercises, and solutions. Map concepts, terminology, and examples back to the book to ensure consistency.
- **Boolean Formatting**: For exercises involving `bool`, require students to convert the value to the exact words expected by the tests (`true/false`, `yes/no`, etc.) and mention the conversion explicitly in starter comments.
- **Input Ordering**: If input is required, note the order the values must be read (e.g., "first value, then second value") and include representative sample inputs in the comments when helpful.
- **Output Blueprint**: Provide a mini blueprint in comments showing the required lines (e.g., `//   Sum: <value>`). Ensure that the labels, spacing, and decimal precision match the `expected` strings in tests.
- **Variant Parity**: Apply the same clarity to every variant (e.g., `_c`, `_cpp`). Both variants must include the identical values/formats relevant to their language.
- **Test Alignment**: After editing exercises, update or verify the associated tests so the `expected` output matches the instructions exactly. If tests require formatted numbers, ensure the starter guidance calls out the formatter to use (`%.1f`, `std::setprecision(4)`, etc.).
- **Regression Check**: Always run `ruby TEST_Suite/test_learn_module.rb` after modifying curriculum JSON to confirm the Learn module remains green.
- **Future Modules**: When adding new modules beyond Module 1, follow this template immediately so learners receive consistent guidance across the entire C/C++ track.
- **Lesson Expansion Mandate**: When revising C/C++ lessons, audit them sequentially from Module 1 forward and expand any sections that feel rushed or underspecified. Every theme must be taught in full with definitions, motivation, complete examples in both C and C++, and explicit call-outs of common pitfalls. Add missing explanations, diagrams, tables, or sidebars as needed so the standalone lesson delivers the same depth as the reference text. Exercises after module 1 to be only in C++.
- **Offline Tutorial Standard**: Treat every C/C++ lesson and exercise as a self-contained offline tutorial. Include all background information, build/run steps, troubleshooting tips, diagrams, and practice workflows a learner would need without internet access.
- **Ref.txt Paraphrasing Mandate**: For every new or revised C/C++ lesson or chapter, mine `ref.txt` for subject matter, then craft an original write-up that paraphrases the concepts, adds fresh examples, ASCII/SVG diagrams, and offline-friendly in-editor exercises. Ensure the lesson remains self-contained and respects copyright by avoiding verbatim excerpts.
- **Plain-Text Example Presentation**: When documenting lessons, rewrite code samples as plain text (no fenced code blocks) and rely on typographic emphasis—colors where supported, italics/bold elsewhere—to highlight keywords or syntax. Apply consistently across all chapters.
- **Chapter Text Source & Formatting Reminder**: Every chapter rewrite must draw primary source material from `ref.txt`, paraphrasing and expanding it, and all code snippets must stay in plain text with stylistic emphasis (color/italic/bold) instead of fenced code blocks—apply this consistently to every chapter in the curriculum.

#### Files Modified Summary
**Major Changes**:
- `learn/curriculum/ruby/curriculum.json`: Added Module 7, numbered all modules/lessons
- `learn/curriculum/ruby/exercises/*.json`: Numbered all 25 exercises (Exercise N.M format)
- `learn/curriculum/ruby/exercises/hello_world_exercise.json`: Converted to output tests
- `learn/curriculum/ruby/exercises/p_method_exercise.json`: Updated year 2024→2025
- `learn/lib/exercise_runner.js`: Added StringIO output capture for type: "output" tests
- `learn/lib/learn_manager.js`: Enhanced cursor positioning and tab management
- `learn/curriculum/ruby/lessons/symbols.md`: New lesson
- `learn/curriculum/ruby/lessons/ranges.md`: New lesson (to be created)
- `learn/curriculum/ruby/lessons/regex.md`: New lesson (to be created)

**Impact**:
- All 25 exercises fully automated with passing tests
- Lesson-exercise alignment verified for all 34 lessons
- Output-focused lessons correctly test printed output
- Data manipulation lessons correctly test return values
- Numbering system enables precise issue tracking: "Exercise 2.3 fails" is unambiguous

## Common Tasks

### Modifying Header Format
1. Edit `core/lib/tsi_header/header_generator.rb` (Ruby side for core format)
2. Update `generateFallbackHeader()` in `core/index.js` (JS fallback)
3. Run `ruby TEST_Suite/full_test_suite.rb` to validate all 147 languages

### Adding VS Code Command
1. Register in `package.json` under `contributes.commands`
2. Implement handler in `core/src/extension.js`: `vscode.commands.registerCommand()`
3. Add to `context.subscriptions.push()` for disposal
4. Update `contributes.menus` if adding to context menus

### Project Template Changes
- Build files: `core/generators/project/buildSystemGenerator.js`
- Documentation: `core/generators/project/documentationGenerator.js`
- Language-specific: `core/generators/project/projectcreators/<language>.js`

## Integration Points

### VS Code APIs Used
- `vscode.workspace.fs`: File system operations (project creation)
- `vscode.window.activeTextEditor`: Document editing for header insertion
- `vscode.commands.registerCommand()`: Command registration
- `context.globalState`: Persistent storage (Study Mode)
- `vscode.window.createStatusBarItem()`: Timer display

### External Dependencies
- **Ruby 2.7+**: Required for header generation (not bundled)
- **execSync**: Synchronous shell execution for Ruby CLI calls
- **No npm runtime dependencies**: Only `@types/vscode` for development

### Extension Activation
- `activationEvents: ["*"]` in `package.json` - always active
- Lazy loads Study Mode only when timer commands are invoked
- Ruby CLI spawned on-demand for each header operation

## Quality Assurance

- **100% language coverage**: All 147 languages have passing header insertion tests
- **No code quality enforcement module**: Previously existed, has been removed
- **Manual validation**: Generated code templates follow language-specific conventions
- **Cross-platform**: Tested on Linux, macOS, Windows (Ruby PATH requirements vary)

## Debugging Tips

- Ruby errors appear in JS `catch` blocks - check `execSync` stderr
- Study Mode issues: Check `context.globalState.get('studyModeState')` for corrupted state
- Timer accuracy: Verify `elapsedTime` is persisted correctly and `lastTickTime` is reset on resume
- Header format issues: Verify delimiter lengths in `delimiters.rb` (must sum to 79 chars)
- Language not supported: Ensure `Delimiters.supports_language?(language_id)` returns true
- Learn Mode issues: Check `context.globalState.get('learn_progress_{language}')` for progress data

## Study Mode Timer - Implementation Details

### Timer State Properties
```javascript
// Source of truth - persisted
this.elapsedTime = 0;        // Milliseconds elapsed in current phase
this.currentPhase = 'stopped'; // 'stopped', 'work', 'shortBreak', 'longBreak'
this.currentSession = 0;      // Current session number

// Runtime only - not persisted
this.lastTickTime = null;     // process.hrtime.bigint() for monotonic timing
this.isRunning = false;       // Whether timer is actively ticking

// Logging only - persisted but not used for calculations
this.phaseStartTimestamp = null; // Date.now() for session log entries
```

### Critical Timer Methods
- `updateElapsedTime()`: Calculates tick duration using `lastTickTime`, adds to `elapsedTime`
- `start()`: Sets `elapsedTime = 0`, `lastTickTime = process.hrtime.bigint()`
- `pause()`: Calls `updateElapsedTime()` to save progress, clears `lastTickTime`
- `resume()`: Resets `lastTickTime = process.hrtime.bigint()`, continues from existing `elapsedTime`
- `getRemainingTime()`: Returns `getCurrentDuration() - elapsedTime` (updates elapsed if running)

### User Interaction Pattern
All timer state changes in `studyModeExtension.js` follow this pattern:
```javascript
// ✅ Correct: Show dialog FIRST, change state AFTER confirmation
this.vscode.window.showInformationMessage('Pause Timer?', { modal: true }, 'Pause', 'Cancel')
    .then(selection => {
        if (selection === 'Pause') {
            this.timer.pause(); // Only execute if user confirms
        }
    });

// ❌ Wrong: Change state BEFORE dialog (old pattern, now fixed)
this.timer.pause(); // Don't do this
this.vscode.window.showInformationMessage('Timer Paused', { modal: true }, 'Got it!');
```

## Learn Mode - Implementation Details

### Module Architecture
```
learn/
├── index.js              # Main Learn class - orchestrates learning sessions
├── lib/
│   ├── learn_manager.js    # Curriculum loading, lesson display, exercise creation
│   ├── progress_tracker.js # Streaks, achievements, statistics
│   └── exercise_runner.js  # Test execution for Ruby/Python/JavaScript
└── curriculum/
    └── ruby/              # Ruby curriculum (8 modules, 34 lessons)
        ├── curriculum.json  # Course structure and metadata
        ├── lessons/         # Markdown lesson files
        ├── exercises/       # JSON exercise definitions with tests
        └── solutions/       # Complete solutions with explanations
```

### Curriculum JSON Structure
```json
{
  "language": "Ruby",
  "modules": [
    {
      "id": "basics",
      "title": "Ruby Basics",
      "lessons": [
        {
          "id": "hello_world",
          "title": "Hello World and Output",
          "duration": 30,
          "difficulty": "beginner"
        }
      ]
    }
  ]
}
```

### Exercise JSON Structure
```json
{
  "id": "hello_world_exercise",
  "title": "Hello World Exercise",
  "starterCode": "# Your code here\n",
  "tests": [
    {
      "name": "test_greeting",
      "call": "greet('Alice')",
      "expected": "Hello, Alice!"
    }
  ],
  "hints": ["Hint 1", "Hint 2"]
}
```

### Progress Tracking Model
Stored in `context.globalState` with key `learn_progress_{language}`:
```javascript
{
  completed: [],              // Array of lesson IDs
  exercisesCompleted: [],     // Array of exercise IDs
  streakDays: 0,              // Consecutive study days
  lastStudyDate: "2025-10-01", // ISO date string
  totalTimeMinutes: 0,         // Total study time
  achievements: []            // Array of achievement IDs
}
```

### Achievement System
Achievements auto-unlock based on conditions:
- `first_exercise`: Complete 1 exercise
- `five_exercises`: Complete 5 exercises
- `ten_exercises`: Complete 10 exercises
- `streak_3`: Study 3 consecutive days
- `streak_7`: Study 7 consecutive days
- `streak_30`: Study 30 consecutive days

### Test Execution Flow
1. User opens exercise file in editor
2. Writes code to solve the problem
3. Runs `tsiheader.runExerciseTests` command
4. ExerciseRunner:
   - Extracts code from active editor
   - Creates temporary test files
   - Executes language-specific test runner (Ruby: minitest, Python: pytest, JS: eval)
   - Parses test output
   - Returns result with score and failures
5. Progress updated if all tests pass
6. User sees success/failure dialog with hints

### Webview Lesson Display
Lessons rendered as HTML in webview panels:
- Markdown converted to HTML with syntax highlighting
- Interactive buttons for "Start Exercise" and "Mark Complete"
- VS Code theme-aware styling
- Bidirectional messaging between webview and extension

### Adding a New Language Curriculum
1. Create directory structure: `curriculum/{language}/`
2. Write `curriculum.json` with modules and lessons
3. Create Markdown files in `lessons/`
4. Define exercises with tests in `exercises/`
5. Write solutions with explanations in `solutions/`
6. Add test runner logic in `exercise_runner.js`
7. Create test file in `TEST_Suite/test_learn_{language}.rb`
8. Register command in `package.json` and `extension.js`

### Best Practices for Curriculum Design
- **Progressive Difficulty**: Start with basics, build to advanced
- **Hands-on Learning**: Every lesson must have practice exercise
- **Clear Outcomes**: Define what learners will achieve
- **Real Examples**: Use practical, not toy, problems
- **Multiple Hints**: 3-5 hints per exercise, increasing in specificity
- **Detailed Solutions**: Explain not just what, but why

## Documentation References

- **README.md**: User-facing documentation, setup instructions, feature list
- **package.json**: Complete command/configuration registry
- **TEST_Suite/**: Examples of all 147 language outputs in test fixtures

## Final Reminder

**DO NOT CREATE SUMMARIES AFTER EACH EDIT. DO NOT CREATE SUMMARY FILES. CREATE DOCUMENTATION ONLY UPON EXPLICIT REQUEST.**

---

## Next Enhancement Phase: Multi-Language Curriculum Expansion

### Phase Overview
Following the successful completion of the comprehensive Ruby curriculum (8 modules, 34 lessons, 394 tests), the next major enhancement will focus on expanding the Learn Mode to support additional programming languages, starting with Python and JavaScript.

### Strategic Objectives
1. **Language Diversity**: Expand from Ruby-only to multi-language support (Python, JavaScript, Java, C++)
2. **Curriculum Consistency**: Maintain the same high-quality standards across all languages
3. **Shared Infrastructure**: Leverage existing Learn Mode architecture for rapid language addition
4. **Cross-Language Learning**: Enable students to learn multiple languages with consistent pedagogy

### Implementation Roadmap

#### Phase 1: Python Curriculum (Priority: High)
**Target**: Complete Python curriculum mirroring Ruby's 8-module structure
**Timeline**: 4-6 weeks
**Scope**:
- 8 modules, 34 lessons, 34 exercises
- Topics: Python basics, control flow, data structures, OOP, advanced concepts, practical applications, professional patterns
- Exercise types: Return value tests, output capture tests, file I/O tests
- Integration with existing test runner infrastructure

**Technical Requirements**:
- Extend `exercise_runner.js` to support Python (pytest integration)
- Create `learn/curriculum/python/` directory structure
- Add Python command registration (`tsiheader.learnPython`)
- Update progress tracking for multi-language support

#### Phase 2: JavaScript Curriculum (Priority: High)
**Target**: Complete JavaScript curriculum with Node.js and browser contexts
**Timeline**: 4-6 weeks (parallel with Python)
**Scope**:
- 8 modules, 34 lessons, 34 exercises
- Topics: JS fundamentals, DOM manipulation, Node.js, async programming, modern ES6+, frameworks introduction
- Exercise types: Console output tests, return value tests, file system tests
- Browser-based exercise execution for DOM-related content

**Technical Requirements**:
- Extend test runner for JavaScript (Node.js execution)
- Add browser simulation capabilities for DOM exercises
- Create `learn/curriculum/javascript/` directory structure
- Update UI to handle language selection

#### Phase 3: Infrastructure Enhancements (Priority: Medium)
**Target**: Improve Learn Mode architecture for multi-language support
**Timeline**: 2-3 weeks
**Scope**:
- Multi-language progress tracking (`learn_progress_{language}`)
- Shared curriculum validation system
- Enhanced webview theming and responsiveness
- Performance optimizations for larger curricula

**Technical Requirements**:
- Refactor progress tracker for language-specific state management
- Add curriculum validation scripts
- Improve webview CSS for better cross-language content display
- Implement lazy loading for curriculum content

#### Phase 4: Advanced Learning Features (Priority: Medium)
**Target**: Add sophisticated learning capabilities
**Timeline**: 3-4 weeks
**Scope**:
- Code review exercises with automated feedback
- Pair programming simulation mode
- Interactive coding challenges with time limits
- Progress analytics and learning path recommendations

**Technical Requirements**:
- Extend exercise runner for advanced test types
- Add timer-based challenge modes
- Implement progress analytics in webview
- Create recommendation engine based on learning patterns

### Success Metrics
- **Test Coverage**: Maintain 100% automated test coverage across all languages
- **Curriculum Quality**: Each language curriculum meets the same standards as Ruby (comprehensive, progressive, practical)
- **User Experience**: Seamless language switching, consistent UI/UX across languages
- **Performance**: Test execution under 2 seconds per exercise, webview load times under 1 second
- **Adoption**: Track usage metrics for different language curricula

### Risk Mitigation
- **Scope Creep**: Implement one language at a time, validate each before proceeding
- **Quality Control**: Automated testing pipeline for all curriculum content
- **Architecture Debt**: Refactor core Learn Mode components before adding languages
- **Resource Allocation**: Dedicate specific time blocks for curriculum development vs. infrastructure work

### Dependencies
- **Ruby Infrastructure**: Current Learn Mode architecture (stable, tested)
- **Testing Framework**: Existing 394-test suite provides validation patterns
- **VS Code APIs**: Current extension framework supports multi-language expansion
- **Content Creation**: Systematic approach proven with Ruby curriculum

### Next Steps
1. **Planning Phase**: Create detailed Python curriculum outline (1 week)
2. **Infrastructure Prep**: Refactor Learn Mode for multi-language support (1 week)
3. **Python Development**: Implement Python curriculum (4 weeks)
4. **JavaScript Development**: Implement JavaScript curriculum (4 weeks)
5. **Testing & Validation**: Comprehensive testing across all languages (2 weeks)
6. **Launch & Monitor**: Deploy multi-language Learn Mode, gather user feedback (ongoing)

This roadmap builds on the solid foundation established by the Ruby curriculum expansion, ensuring consistent quality and user experience across multiple programming languages.
