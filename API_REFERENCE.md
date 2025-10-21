# Uni Header Extension - API Reference

## Overview

This document provides a comprehensive API reference for the Uni Header VS Code extension, covering all public interfaces, classes, and methods available for extension development and integration.

## Core API

### TSICore Class

The main interface class that provides access to all extension functionality.

```javascript
const { TSICore } = require('./core/index.js');
const core = new TSICore(extensionPath);
```

#### Constructor
```javascript
constructor(extensionPath)
```
- `extensionPath` (string): Path to the extension directory

#### Methods

##### Header Management
```javascript
async generateHeader(languageId, fileName, env)
```
Generates a TSI header for a file.
- **Parameters**:
  - `languageId` (string): VS Code language ID
  - `fileName` (string): Name of the file
  - `env` (Object): Environment variables for configuration
- **Returns**: `Promise<Object>` - `{success: boolean, header?: string, message?: string}`

```javascript
async updateHeader(languageId, fileName, env)
```
Updates an existing TSI header.
- **Parameters**: Same as `generateHeader`
- **Returns**: `Promise<Object>` - `{success: boolean, message?: string}`

```javascript
async generateTSIHeaderContent(fileName, vscode)
```
Generates TSI header content for a file.
- **Parameters**:
  - `fileName` (string): Name of the file
  - `vscode` (Object): VS Code API object
- **Returns**: `Promise<string>` - Generated header content

##### Code Generation
```javascript
generateClass(languageId, className, fileName, env)
```
Generates a class template.
- **Parameters**:
  - `languageId` (string): VS Code language ID
  - `className` (string): Name of the class
  - `fileName` (string): Name of the file
  - `env` (Object): Environment variables
- **Returns**: `Object` - `{success: boolean, content?: string, files?: Array, message?: string}`

```javascript
generateCodeBase(languageId, fileName)
```
Generates code base structure.
- **Parameters**:
  - `languageId` (string): VS Code language ID
  - `fileName` (string): Name of the file
- **Returns**: `Object` - `{success: boolean, content?: string, message?: string}`

##### Project Management
```javascript
async createProject(language, projectName, workspaceUri, vscode)
```
Creates a TSI project structure.
- **Parameters**:
  - `language` (string): Programming language
  - `projectName` (string): Name of the project
  - `workspaceUri` (Object): VS Code workspace URI
  - `vscode` (Object): VS Code API object
- **Returns**: `Promise<Object>` - `{success: boolean, message?: string}`

##### Utility Methods
```javascript
hasSubstantialContent(content)
```
Checks if file content has substantial content beyond header.
- **Parameters**: `content` (string): File content
- **Returns**: `boolean`

```javascript
findHeaderEndLine(content)
```
Finds the line where header ends.
- **Parameters**: `content` (string): File content
- **Returns**: `number` - Line number where header ends

```javascript
detectLanguageFromExtension(languageId, fileName)
```
Detects language from file extension.
- **Parameters**:
  - `languageId` (string): VS Code language ID
  - `fileName` (string): Name of the file
- **Returns**: `string` - Detected language ID

```javascript
getUserConfig(vscode)
```
Gets user configuration with fallbacks.
- **Parameters**: `vscode` (Object): VS Code API
- **Returns**: `Object` - `{username, email, hasCredentials}`

## Learn Module API

### Learn Class

Interactive learning platform for programming languages.

```javascript
const Learn = require('./learn/index.js');
const learn = new Learn(context, vscode);
```

#### Constructor
```javascript
constructor(context, vscode)
```
- `context` (Object): VS Code extension context
- `vscode` (Object): VS Code API object

#### Methods

##### Learning Management
```javascript
async startLearning(language)
```
Starts a learning session for a specific language.
- **Parameters**: `language` (string): Programming language to learn
- **Returns**: `Promise<void>`

```javascript
async runExercise(language, exercise)
```
Runs an exercise.
- **Parameters**:
  - `language` (string): Programming language
  - `exercise` (Object): Exercise object
- **Returns**: `Promise<Object>` - Exercise results

```javascript
async browseLessons(language)
```
Browses and jumps to any lesson in the curriculum.
- **Parameters**: `language` (string): Programming language
- **Returns**: `Promise<void>`

```javascript
async reviewLessons(language)
```
Reviews completed lessons.
- **Parameters**: `language` (string): Programming language
- **Returns**: `Promise<void>`

##### Progress and Statistics
```javascript
async getStats(language)
```
Gets learning statistics.
- **Parameters**: `language` (string): Programming language
- **Returns**: `Promise<Object>` - Statistics object

##### Solution Management
```javascript
async showSolution(language, exercise)
```
Shows the solution for an exercise.
- **Parameters**:
  - `language` (string): Programming language
  - `exercise` (Object): Exercise object
- **Returns**: `Promise<void>`

```javascript
async showHint(exercise)
```
Shows a hint for an exercise.
- **Parameters**: `exercise` (Object): Exercise object
- **Returns**: `Promise<void>`

## Calendar Module API

### CalendarManager Class

Study calendar and event management.

```javascript
const { CalendarManager } = require('./calendar/src/calendarManager.js');
const calendar = new CalendarManager(context);
```

#### Constructor
```javascript
constructor(context)
```
- `context` (Object): VS Code extension context

#### Methods

##### Calendar Management
```javascript
async initialize()
```
Initializes the calendar module.
- **Returns**: `Promise<void>`

```javascript
async showDeadlineDialog()
```
Shows dialog to add a deadline.
- **Returns**: `Promise<Object|null>` - Deadline object or null

```javascript
async showEventDialog()
```
Shows dialog to add a custom event.
- **Returns**: `Promise<Object|null>` - Event object or null

```javascript
async showScheduleDialog()
```
Shows dialog to add a daily schedule.
- **Returns**: `Promise<Object|null>` - Schedule object or null

##### Import/Export
```javascript
async exportCalendar()
```
Exports calendar data.
- **Returns**: `Promise<void>`

```javascript
async importCalendar()
```
Imports calendar data from file.
- **Returns**: `Promise<void>`

```javascript
async importCalendarFromUrl()
```
Imports calendar data from URL.
- **Returns**: `Promise<void>`

## Study Mode API

### StudyModeTimer Class

Pomodoro timer with work/break phases.

```javascript
const { StudyModeTimer } = require('./studyMode/timer.js');
const timer = new StudyModeTimer(vscode, context, config, onStateChange);
```

#### Constructor
```javascript
constructor(vscode, context, config = {}, onStateChange = null)
```
- `vscode` (Object): VS Code API object
- `context` (Object): VS Code extension context
- `config` (Object): Timer configuration
- `onStateChange` (Function): Callback for state persistence

#### Configuration Object
```javascript
{
    workDuration: 25,           // Work session duration in minutes
    shortBreakDuration: 5,      // Short break duration in minutes
    longBreakDuration: 15,      // Long break duration in minutes
    sessionsBeforeLongBreak: 4  // Sessions before long break
}
```

#### Methods

##### Timer Control
```javascript
start()
```
Starts the timer.

```javascript
pause()
```
Pauses the timer.

```javascript
resume()
```
Resumes the timer.

```javascript
stop()
```
Stops the timer.

```javascript
reset()
```
Resets the timer to initial state.

##### State Management
```javascript
getRemainingTime()
```
Gets remaining time in current phase.
- **Returns**: `number` - Remaining time in milliseconds

```javascript
formatTime(milliseconds)
```
Formats time in MM:SS format.
- **Parameters**: `milliseconds` (number): Time in milliseconds
- **Returns**: `string` - Formatted time string

```javascript
updateConfiguration(config)
```
Updates timer configuration.
- **Parameters**: `config` (Object): New configuration object

##### Session Logging
```javascript
logSession(completed)
```
Logs a session.
- **Parameters**: `completed` (boolean): Whether session was completed

## Configuration API

### VS Code Settings

The extension uses VS Code's configuration system with the following settings:

#### Header Settings
```json
{
    "tsiheader.username": "string",
    "tsiheader.email": "string",
    "tsiheader.customHeader.enableCustomHeader": "boolean",
    "tsiheader.customHeader.institutionName": "string",
    "tsiheader.customHeader.institutionUrl": "string"
}
```

#### Study Mode Settings
```json
{
    "tsiheader.studyMode.enableSounds": "boolean",
    "tsiheader.studyMode.workDuration": "number",
    "tsiheader.studyMode.shortBreakDuration": "number",
    "tsiheader.studyMode.longBreakDuration": "number",
    "tsiheader.studyMode.sessionsBeforeLongBreak": "number"
}
```

#### Calendar Settings
```json
{
    "tsiheader.calendar.importUrl": "string",
    "tsiheader.notifications.enableEmail": "boolean",
    "tsiheader.notifications.emailService": "string",
    "tsiheader.notifications.emailAddress": "string",
    "tsiheader.notifications.advanceNotice": "number",
    "tsiheader.notifications.smtpHost": "string",
    "tsiheader.notifications.smtpPort": "number",
    "tsiheader.notifications.smtpUser": "string",
    "tsiheader.notifications.smtpPassword": "string"
}
```

## Command API

### Available Commands

The extension registers the following commands:

#### Header Commands
- `tsiheader.insertHeader` - Insert TSI header
- `tsiheader.updateHeader` - Update existing header
- `tsiheader.removeHeader` - Remove header
- `tsiheader.addClass` - Add class template
- `tsiheader.addCodeBase` - Add code base template

#### Project Commands
- `tsiheader.createTSIProject` - Create generic project
- `tsiheader.createCProject` - Create C project
- `tsiheader.createCppProject` - Create C++ project
- `tsiheader.createPythonProject` - Create Python project
- `tsiheader.createJavaProject` - Create Java project
- `tsiheader.createRustProject` - Create Rust project
- `tsiheader.createRubyProject` - Create Ruby project
- `tsiheader.createPhpProject` - Create PHP project
- `tsiheader.createHtmlProject` - Create HTML project

#### Learning Commands
- `tsiheader.learnRuby` - Start Ruby learning
- `tsiheader.learnRust` - Start Rust learning
- `tsiheader.learnC` - Start C learning
- `tsiheader.learnCpp` - Start C++ learning
- `tsiheader.learnCppDsa` - Start C++ DSA learning
- `tsiheader.learnGit` - Start Git learning
- `tsiheader.learnOdin` - Start Odin Project learning
- `tsiheader.learnMathematics` - Start Mathematics learning

#### Study Mode Commands
- `tsiheader.startStudySession` - Start study session
- `tsiheader.pauseStudyTimer` - Pause/resume timer
- `tsiheader.stopStudySession` - Stop study session
- `tsiheader.viewStudyStats` - View study statistics
- `tsiheader.configureStudyMode` - Configure study mode
- `tsiheader.resetStudyProgress` - Reset study progress

#### Calendar Commands
- `tsiheader.showCalendar` - Show calendar
- `tsiheader.addCalendarDeadline` - Add deadline
- `tsiheader.addCalendarEvent` - Add custom event
- `tsiheader.addCalendarSchedule` - Add daily schedule
- `tsiheader.exportCalendar` - Export calendar
- `tsiheader.importCalendar` - Import calendar
- `tsiheader.importCalendarFromUrl` - Import calendar from URL
- `tsiheader.testNotification` - Test email notification

## Event API

### Event Listeners

The extension provides several event listeners:

#### Document Events
```javascript
vscode.workspace.onWillSaveTextDocument(callback)
```
Triggered before a document is saved.

```javascript
vscode.workspace.onDidCloseTextDocument(callback)
```
Triggered when a document is closed.

#### Tree View Events
```javascript
vscode.window.registerTreeDataProvider(viewId, provider)
```
Registers a tree data provider for a view.

#### Webview Events
```javascript
panel.webview.onDidReceiveMessage(callback)
```
Handles messages from webview panels.

## Error Handling

### Error Types

The extension defines several error types:

#### Header Generation Errors
- `Ruby CLI not available` - Ruby CLI failed to execute
- `Invalid language ID` - Unsupported language
- `File access denied` - Permission issues

#### Learning Errors
- `Curriculum not found` - Language curriculum missing
- `Exercise execution failed` - Compiler or runtime errors
- `Progress tracking failed` - Storage issues

#### Calendar Errors
- `Import failed` - Invalid calendar data
- `SMTP connection failed` - Email notification issues
- `Storage access denied` - File system permissions

### Error Handling Patterns

```javascript
try {
    const result = await someAsyncOperation();
    if (!result.success) {
        throw new Error(result.message);
    }
    return result;
} catch (error) {
    vscode.window.showErrorMessage(`Operation failed: ${error.message}`);
    throw error;
}
```

## Data Structures

### Exercise Object
```javascript
{
    id: "string",
    title: "string",
    description: "string",
    language: "string",
    lessonId: "string",
    hints: ["string"],
    tests: ["string"],
    solution: "string",
    explanation: "string"
}
```

### Calendar Event Object
```javascript
{
    id: "string",
    title: "string",
    description: "string",
    start: "ISO string",
    end: "ISO string",
    category: "string",
    priority: "string"
}
```

### Study Session Object
```javascript
{
    type: "work|shortBreak|longBreak",
    startTime: "ISO string",
    endTime: "ISO string",
    duration: "number",
    completed: "boolean",
    sessionNumber: "number"
}
```

## Integration Examples

### Basic Header Generation
```javascript
const { TSICore } = require('./core/index.js');
const core = new TSICore(extensionPath);

// Generate header for a C file
const result = await core.generateHeader('c', 'main.c', {
    TSI_USERNAME: 'John Doe',
    TSI_EMAIL: 'john@example.com'
});

if (result.success) {
    console.log('Header generated:', result.header);
} else {
    console.error('Error:', result.message);
}
```

### Learning Module Integration
```javascript
const Learn = require('./learn/index.js');
const learn = new Learn(context, vscode);

// Start learning Ruby
await learn.startLearning('ruby');

// Run an exercise
const exercise = {
    id: 'hello-world',
    title: 'Hello World',
    language: 'ruby',
    tests: ['puts "Hello, World!"']
};

const result = await learn.runExercise('ruby', exercise);
console.log('Exercise result:', result);
```

### Calendar Integration
```javascript
const { CalendarManager } = require('./calendar/src/calendarManager.js');
const calendar = new CalendarManager(context);
await calendar.initialize();

// Add a deadline
const deadline = await calendar.showDeadlineDialog();
if (deadline) {
    await calendar.eventManager.addDeadline(deadline);
}
```

### Study Timer Integration
```javascript
const { StudyModeTimer } = require('./studyMode/timer.js');

const config = {
    workDuration: 25,
    shortBreakDuration: 5,
    longBreakDuration: 15,
    sessionsBeforeLongBreak: 4
};

const timer = new StudyModeTimer(vscode, context, config, () => {
    console.log('Timer state changed');
});

// Start the timer
timer.start();
```

## Best Practices

### Error Handling
1. Always wrap async operations in try-catch blocks
2. Provide meaningful error messages to users
3. Log errors for debugging purposes
4. Gracefully handle missing dependencies

### Performance
1. Use lazy loading for large datasets
2. Implement proper cleanup in dispose methods
3. Cache frequently accessed data
4. Avoid blocking the main thread

### User Experience
1. Provide clear feedback for long-running operations
2. Use progress indicators for multi-step processes
3. Validate user input before processing
4. Maintain consistent UI patterns

### Security
1. Validate all user inputs
2. Sanitize data before storage
3. Use secure communication for external APIs
4. Follow VS Code security guidelines