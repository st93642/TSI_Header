# TSI Header - AI Coding Agent Instructions

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
# Ruby tests (319 tests, 100% coverage)
ruby TEST_Suite/full_test_suite.rb

# Study Mode tests (Node.js)
cd studyMode && npm test
```

Test files validate:
- Header insertion/update for all 147 languages
- Class generation for 13 OOP languages
- Project creation for 7 languages
- Timer state persistence and analytics

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

## Documentation References

- **README.md**: User-facing documentation, setup instructions, feature list
- **package.json**: Complete command/configuration registry
- **TEST_Suite/**: Examples of all 147 language outputs in test fixtures
