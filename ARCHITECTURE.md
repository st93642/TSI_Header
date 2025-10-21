# Uni Header Extension - Architecture Documentation

## Overview

The Uni Header extension is a comprehensive VS Code extension that provides multiple educational and productivity features for students and developers. It combines code generation, learning modules, study management, and calendar functionality in a single package.

## Core Architecture

### Main Components

1. **Core Module** (`/core/`) - Header generation and code scaffolding
2. **Learn Module** (`/learn/`) - Interactive learning platform
3. **Calendar Module** (`/calendar/`) - Study calendar and event management
4. **Study Mode** (`/studyMode/`) - Pomodoro timer and study analytics
5. **Top Module** (`/top/`) - The Odin Project curriculum integration

### Extension Entry Point

- **Main File**: `core/src/extension.js` (122,138 characters - very large file)
- **Package**: `package.json` with comprehensive VS Code extension configuration
- **Version**: 7.0.8

## Module Breakdown

### 1. Core Module (`/core/`)

**Purpose**: Code generation, header management, and project scaffolding

**Key Files**:
- `index.js` - Core interface and API definitions
- `src/extension.js` - Main extension logic (very large file)
- `generators/` - Code generation templates for 122+ languages
- `lib/` - Ruby CLI for header generation
- `utils/` - Content analysis utilities

**Features**:
- TSI header generation for 122+ programming languages
- Class and code base generation
- Project scaffolding for C/C++/Java/Python/Ruby/Rust/PHP/HTML
- Custom institution branding support
- Header detection and updates

**Architecture**:
```javascript
class TSICore {
    constructor(extensionPath) {
        this.headerManager = new TSIHeaderManager(extensionPath);
        this.codeGenerator = new TSICodeGenerator(extensionPath);
        this.projectManager = new TSIProjectManager(extensionPath);
        this.utils = new TSIUtils();
    }
}
```

### 2. Learn Module (`/learn/`)

**Purpose**: Interactive learning platform with structured curricula

**Key Files**:
- `index.js` - Main learning interface
- `lib/learn_manager.js` - Curriculum management
- `lib/progress_tracker.js` - Learning progress tracking
- `lib/exercise_runner.js` - Exercise execution and testing
- `curriculum/` - Language-specific curricula

**Supported Languages**:
- C (41 files)
- C++ (79 files)
- C++ DSA (61 files)
- Ruby (112 files)
- Rust (103 files)
- Git (37 files)
- Mathematics (42 files)

**Features**:
- Structured lesson progression
- Interactive exercises with automated testing
- Progress tracking and statistics
- Solution viewing with syntax highlighting
- Offline curriculum caching

**Architecture**:
```javascript
class Learn {
    constructor(context, vscode) {
        this.progressTracker = new ProgressTracker(context);
        this.learnManager = new LearnManager(context, vscode, this.progressTracker);
        this.exerciseRunner = new ExerciseRunner(vscode);
    }
}
```

### 3. Calendar Module (`/calendar/`)

**Purpose**: Study calendar, deadline tracking, and event management

**Key Files**:
- `src/calendarManager.js` - Main calendar logic
- `src/calendarDataManager.js` - Data persistence
- `src/calendarEventManager.js` - Event operations
- `src/calendarWebviewProvider.js` - Webview interface
- `resources/` - Calendar UI assets

**Features**:
- Deadline tracking with priority levels
- Custom event scheduling
- Daily schedule management
- iCalendar import/export
- Email notifications via SMTP
- Full calendar webview

**Architecture**:
```javascript
class CalendarManager {
    constructor(context) {
        this.dataManager = new CalendarDataManager(context);
        this.eventManager = new CalendarEventManager(this.dataManager);
        this.treeDataProvider = new CalendarTreeDataProvider(this.eventManager);
        this.webviewProvider = new CalendarWebviewProvider(/*...*/);
    }
}
```

### 4. Study Mode (`/studyMode/`)

**Purpose**: Pomodoro timer and study session management

**Key Files**:
- `timer.js` - Core timer logic
- `timer.test.js` - Unit tests
- `package.json` - Study mode dependencies

**Features**:
- Configurable Pomodoro timer (work/break cycles)
- Visual countdown with emoji indicators
- Session logging and analytics
- Audio notifications
- Status bar integration

**Architecture**:
```javascript
class StudyModeTimer {
    constructor(vscode, context, config = {}, onStateChange = null) {
        this.workDuration = config.workDuration || 25; // minutes
        this.shortBreakDuration = config.shortBreakDuration || 5;
        this.longBreakDuration = config.longBreakDuration || 15;
        this.sessionsBeforeLongBreak = config.sessionsBeforeLongBreak || 4;
    }
}
```

### 5. Top Module (`/top/`)

**Purpose**: The Odin Project curriculum integration

**Key Files**:
- `curriculum.json` - Complete Odin Project curriculum (2,400+ lines)
- `odin_manager.js` - Odin Project specific management

**Features**:
- Complete Odin Project curriculum mapping
- 800+ estimated hours of content
- Foundations and Full Stack JavaScript paths
- Lesson progression tracking

## Data Flow

### 1. Extension Activation
```
VS Code → extension.js → Module Initialization → Command Registration
```

### 2. Header Generation
```
User Command → TSICore → TSIHeaderManager → Ruby CLI → Generated Header
```

### 3. Learning Flow
```
User Command → Learn Module → Curriculum Loading → Exercise Execution → Progress Update
```

### 4. Calendar Operations
```
User Command → CalendarManager → EventManager → DataManager → Storage
```

### 5. Study Timer
```
User Command → StudyModeTimer → State Management → Status Bar Update → Notifications
```

## Configuration System

The extension uses VS Code's configuration system with the following categories:

### Header Configuration
- `tsiheader.username` - User name for headers
- `tsiheader.email` - User email for headers
- `tsiheader.customHeader.*` - Custom institution branding

### Study Mode Configuration
- `tsiheader.studyMode.workDuration` - Work session duration
- `tsiheader.studyMode.shortBreakDuration` - Short break duration
- `tsiheader.studyMode.longBreakDuration` - Long break duration
- `tsiheader.studyMode.sessionsBeforeLongBreak` - Sessions before long break

### Calendar Configuration
- `tsiheader.calendar.importUrl` - Calendar import URL
- `tsiheader.notifications.*` - Email notification settings

## Command Structure

The extension registers 50+ commands across different categories:

### Header Commands
- `tsiheader.insertHeader` - Insert TSI header
- `tsiheader.updateHeader` - Update existing header
- `tsiheader.removeHeader` - Remove header
- `tsiheader.addClass` - Add class template
- `tsiheader.addCodeBase` - Add code base template

### Project Commands
- `tsiheader.createTSIProject` - Create generic project
- `tsiheader.createCProject` - Create C project
- `tsiheader.createCppProject` - Create C++ project
- `tsiheader.createPythonProject` - Create Python project
- `tsiheader.createJavaProject` - Create Java project
- `tsiheader.createRustProject` - Create Rust project
- `tsiheader.createRubyProject` - Create Ruby project
- `tsiheader.createPhpProject` - Create PHP project
- `tsiheader.createHtmlProject` - Create HTML project

### Learning Commands
- `tsiheader.learnRuby` - Start Ruby learning
- `tsiheader.learnRust` - Start Rust learning
- `tsiheader.learnC` - Start C learning
- `tsiheader.learnCpp` - Start C++ learning
- `tsiheader.learnCppDsa` - Start C++ DSA learning
- `tsiheader.learnGit` - Start Git learning
- `tsiheader.learnOdin` - Start Odin Project learning
- `tsiheader.learnMathematics` - Start Mathematics learning

### Study Mode Commands
- `tsiheader.startStudySession` - Start study session
- `tsiheader.pauseStudyTimer` - Pause/resume timer
- `tsiheader.stopStudySession` - Stop study session
- `tsiheader.viewStudyStats` - View study statistics

### Calendar Commands
- `tsiheader.showCalendar` - Show calendar
- `tsiheader.addCalendarDeadline` - Add deadline
- `tsiheader.addCalendarEvent` - Add custom event
- `tsiheader.addCalendarSchedule` - Add daily schedule
- `tsiheader.exportCalendar` - Export calendar
- `tsiheader.importCalendar` - Import calendar

## View Structure

### Activity Bar Panels
1. **Uni Header Explorer** (`tsi-header-explorer`)
   - Uni Commands view
   - Cached Lessons view

2. **Study Calendar** (`tsi-calendar-container`)
   - Calendar view

### Tree Views
- **tsi-commands** - Main command interface
- **tsi-projects** - Project creation interface
- **tsi-calendar** - Calendar event management

## Dependencies

### Runtime Dependencies
- VS Code API (^1.74.0)
- Ruby (2.7+) for header generation
- Git (optional) for user identity resolution
- Compilers (optional) for Learn Mode exercises

### Development Dependencies
- @types/vscode (^1.74.0)
- @xmldom/xmldom (^0.8.10)

## File Structure

```
/workspace/
├── core/                    # Core functionality
│   ├── generators/         # Code generation templates
│   ├── lib/               # Ruby CLI and utilities
│   ├── src/               # Main extension logic
│   └── utils/             # Content analysis utilities
├── learn/                  # Learning platform
│   ├── curriculum/        # Language curricula
│   ├── lib/              # Learning management
│   └── tests/            # Learning tests
├── calendar/              # Calendar functionality
│   ├── resources/        # Calendar UI assets
│   └── src/             # Calendar logic
├── studyMode/            # Pomodoro timer
├── top/                  # Odin Project integration
├── resources/            # Extension resources
├── scripts/             # Build scripts
├── spec/               # Test specifications
└── TEST_Suite/         # Test suite
```

## Testing

### Test Structure
- **Unit Tests**: Individual module testing
- **Integration Tests**: Cross-module functionality
- **End-to-End Tests**: Complete user workflows

### Test Files
- `studyMode/timer.test.js` - Timer unit tests
- `learn/tests/no_md_tables.js` - Learning module tests
- `spec/unified_test.rb` - Ruby test suite
- `TEST_Suite/` - Comprehensive test suite

## Performance Considerations

### Large Files
- `core/src/extension.js` (122,138 characters) - Consider refactoring
- `top/curriculum.json` (2,400+ lines) - Curriculum data

### Caching
- Offline curriculum caching for Learn Mode
- Calendar data persistence
- Progress tracking storage

### Memory Management
- Proper disposal of event listeners
- Cleanup of webview panels
- Timer interval management

## Security Considerations

### User Data
- Local storage for progress and calendar data
- No external data transmission without user consent
- SMTP credentials stored in VS Code settings

### Code Execution
- Sandboxed exercise execution
- Safe file operations
- Input validation for user inputs

## Future Improvements

### Architecture
1. **Modularization**: Break down large files into smaller modules
2. **Plugin System**: Allow third-party curriculum additions
3. **API Layer**: Create REST API for external integrations
4. **Database**: Consider SQLite for complex data relationships

### Features
1. **Collaboration**: Multi-user study sessions
2. **Analytics**: Advanced learning analytics
3. **Integration**: More external calendar services
4. **Mobile**: Companion mobile app

### Performance
1. **Lazy Loading**: Load curricula on demand
2. **Background Processing**: Move heavy operations to background
3. **Caching**: Implement intelligent caching strategies
4. **Bundle Optimization**: Reduce extension size

## Maintenance

### Regular Tasks
1. **Dependency Updates**: Keep VS Code API and dependencies current
2. **Curriculum Updates**: Update learning materials regularly
3. **Bug Fixes**: Address user-reported issues
4. **Performance Monitoring**: Monitor extension performance

### Code Quality
1. **Linting**: Maintain consistent code style
2. **Testing**: Ensure comprehensive test coverage
3. **Documentation**: Keep documentation current
4. **Refactoring**: Regular code cleanup and optimization