# Uni Header Extension - User Guide

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Core Features](#core-features)
4. [Learning Platform](#learning-platform)
5. [Study Mode](#study-mode)
6. [Calendar System](#calendar-system)
7. [Configuration](#configuration)
8. [Troubleshooting](#troubleshooting)
9. [Tips and Tricks](#tips-and-tricks)

## Installation

### Prerequisites

- **Visual Studio Code**: Version 1.74.0 or later
- **Ruby**: Version 2.7 or later (for header generation)
- **Git** (optional): For automatic user identity resolution

### Install from VS Code Marketplace

1. Open VS Code
2. Go to Extensions view (Ctrl+Shift+X)
3. Search for "Uni Header"
4. Click Install on the "University Header, Learn, Pomodoro & Calendar" extension
5. Reload VS Code when prompted

### Manual Installation

1. Download the `.vsix` file from the releases page
2. Open VS Code
3. Go to Extensions view
4. Click the "..." menu and select "Install from VSIX..."
5. Select the downloaded file

## Quick Start

### 1. Configure Your Identity

Before using the extension, set up your user information:

1. Open VS Code Settings (Ctrl+,)
2. Search for "tsiheader"
3. Set your username and email:
   - `tsiheader.username`: Your name
   - `tsiheader.email`: Your email address

### 2. Insert Your First Header

1. Open any code file
2. Right-click in the editor
3. Select "Insert Header" from the context menu
4. A TSI header will be automatically generated

### 3. Start Learning

1. Open the Activity Bar (Ctrl+Shift+E)
2. Click on the "Uni Header" icon
3. Use the "Uni Commands" panel to access learning modules
4. Choose a programming language to start learning

## Core Features

### Header Generation

#### Supported Languages

The extension supports 122+ programming languages including:

- C/C++
- Java
- Python
- Ruby
- Rust
- PHP
- HTML/CSS/JavaScript
- And many more...

#### Inserting Headers

**Method 1: Context Menu**

1. Right-click in any code file
2. Select "Insert Header"

**Method 2: Command Palette**

1. Press Ctrl+Shift+P
2. Type "Insert Header"
3. Press Enter

**Method 3: Activity Bar**

1. Click the "Uni Header" icon in the Activity Bar
2. Click "Insert Header" in the Uni Commands panel

#### Updating Headers

- Right-click in a file with an existing header
- Select "Update Header" to refresh the header with current information

#### Removing Headers

- Right-click in a file with an existing header
- Select "Remove Header" to delete the header

### Code Generation

#### Adding Classes

1. Right-click in a code file
2. Select "Add class"
3. Enter the class name when prompted
4. A class template will be generated

#### Adding Code Bases

1. Right-click in a code file
2. Select "Add code base"
3. A basic code structure will be generated

### Project Creation

#### Creating New Projects

1. Right-click in the Explorer panel
2. Select "Create Uni Project"
3. Choose the programming language
4. Enter the project name
5. A complete project structure will be created

#### Supported Project Types

- **C Projects**: Complete C project with Makefile
- **C++ Projects**: C++ project with CMake support
- **Python Projects**: Python project with virtual environment
- **Java Projects**: Maven-based Java project
- **Rust Projects**: Cargo-based Rust project
- **Ruby Projects**: Gem-based Ruby project
- **PHP Projects**: Composer-based PHP project
- **HTML Projects**: Static website project

## Learning Platform

### Available Curricula

#### Programming Languages

- **C**: 41 lessons covering C fundamentals
- **C++**: 79 lessons including advanced features
- **C++ DSA**: 61 lessons on data structures and algorithms
- **Ruby**: 112 lessons from basics to advanced topics
- **Rust**: 103 lessons including web development
- **Git**: 37 lessons on version control

#### Additional Subjects

- **Mathematics**: 42 workbooks on higher mathematics
- **The Odin Project**: Complete full-stack web development curriculum

### Starting a Learning Session

#### Method 1: Command Palette

1. Press Ctrl+Shift+P
2. Type "Learn [Language]"
3. Press Enter

#### Method 2: Activity Bar

1. Click the "Uni Header" icon
2. Use the "Uni Commands" panel
3. Click on your desired language

### Learning Interface

#### Lesson Structure

Each lesson includes:

- **Theory**: Explanatory content
- **Exercises**: Hands-on practice
- **Solutions**: Complete solutions with explanations
- **Progress Tracking**: Automatic progress saving

#### Exercise Types

- **Automated Tests**: Code is automatically tested
- **Manual Exercises**: Self-paced learning activities
- **Interactive Examples**: Step-by-step tutorials

#### Progress Tracking

- **Completion Status**: Track completed lessons and exercises
- **Statistics**: View learning progress and streaks
- **Resume**: Continue where you left off

### Running Exercises

#### Automated Exercises

1. Complete the exercise code
2. Right-click in the file
3. Select "Run Exercise Tests"
4. View test results and feedback

#### Manual Exercises

1. Complete the exercise
2. Click "Mark Complete" when done
3. Progress will be automatically saved

### Viewing Solutions

1. After completing an exercise, click "View Solution"
2. A webview will open with:
   - Complete solution code
   - Detailed explanations
   - Syntax highlighting
   - Navigation options

## Study Mode

### Pomodoro Timer

#### Starting a Study Session

1. Press Ctrl+Shift+P
2. Type "Start Study Session"
3. Press Enter
4. The timer will start in the status bar

#### Timer Controls

- **Pause/Resume**: Click the timer in the status bar or use Ctrl+Shift+P → "Pause/Resume Study Timer"
- **Stop**: Use Ctrl+Shift+P → "Stop Study Session"

#### Timer Phases

- **Work Session**: 25 minutes (configurable)
- **Short Break**: 5 minutes (configurable)
- **Long Break**: 15 minutes (configurable)
- **Sessions Before Long Break**: 4 sessions (configurable)

#### Visual Indicators

- **🍅**: Work session countdown
- **☕**: Break session countdown
- **❄️**: Paused state

### Study Statistics

#### Viewing Statistics

1. Press Ctrl+Shift+P
2. Type "View Study Statistics"
3. Press Enter

#### Statistics Include

- **Total Study Time**: Hours and minutes studied
- **Sessions Completed**: Number of completed work sessions
- **Current Streak**: Consecutive days of study
- **Weekly/Monthly Progress**: Detailed breakdowns

### Configuration

#### Timer Settings

1. Open VS Code Settings (Ctrl+,)
2. Search for "tsiheader.studyMode"
3. Adjust the following settings:
   - `workDuration`: Work session duration (1-120 minutes)
   - `shortBreakDuration`: Short break duration (1-60 minutes)
   - `longBreakDuration`: Long break duration (1-120 minutes)
   - `sessionsBeforeLongBreak`: Sessions before long break (1-10)
   - `enableSounds`: Enable audio notifications

## Calendar System

### Accessing the Calendar

1. Click the "Study Calendar" icon in the Activity Bar
2. Use the calendar panel to manage events

### Event Types

#### Deadlines

Track assignment due dates with priority levels.

**Adding a Deadline:**

1. Click "➕ Add Deadline" in the calendar panel
2. Enter deadline details:
   - Title
   - Description (optional)
   - Due date (YYYY-MM-DD format)
   - Priority level (Low/Medium/High)

#### Custom Events

Schedule study sessions, meetings, or personal events.

**Adding a Custom Event:**

1. Click "➕ Add Event" in the calendar panel
2. Enter event details:
   - Title
   - Description (optional)
   - Date (YYYY-MM-DD format)
   - Category (Study/Work/Personal/Meeting/Other)

#### Daily Schedules

Set recurring time blocks for consistent study routines.

**Adding a Daily Schedule:**

1. Click "➕ Add Schedule" in the calendar panel
2. Enter schedule details:
   - Title
   - Start time (HH:MM format)
   - End time (HH:MM format)
   - Days of week
   - Category

### Calendar Views

#### Tree View

- **Upcoming Deadlines**: Next 7 days
- **Today's Schedule**: Today's scheduled activities
- **This Week's Events**: Events for the next 7 days

#### Full Calendar

1. Click "📅 Open Full Calendar"
2. View monthly calendar with all events
3. Click on events to view details
4. Add, edit, or delete events

### Import/Export

#### Importing Calendar Data

**From File:**

1. Click "📥 Import from File"
2. Select a JSON or iCalendar (.ics) file
3. Data will be merged with existing calendar

**From URL:**

1. Configure import URL in settings
2. Click "🌐 Import from URL"
3. Data will be fetched and merged

#### Exporting Calendar Data

1. Click "📤 Export Calendar"
2. Choose save location
3. Calendar data will be exported as JSON

### Email Notifications

#### Setting Up Email Notifications

1. Open VS Code Settings (Ctrl+,)
2. Search for "tsiheader.notifications"
3. Configure SMTP settings:
   - `enableEmail`: Enable email notifications
   - `emailService`: Set to "smtp"
   - `smtpHost`: SMTP server hostname
   - `smtpPort`: SMTP server port (usually 587)
   - `smtpUser`: Your email username
   - `smtpPassword`: Your email password
   - `emailAddress`: Recipient email address
   - `advanceNotice`: Hours in advance to send notifications

#### Testing Notifications

1. Press Ctrl+Shift+P
2. Type "Test Email Notification"
3. Press Enter
4. Check your email for the test message

## Configuration

### Header Configuration

#### Basic Settings

- `tsiheader.username`: Your name for headers
- `tsiheader.email`: Your email for headers

#### Custom Institution Headers

Enable custom branding for your institution:

1. Set `tsiheader.customHeader.enableCustomHeader` to `true`
2. Configure:
   - `tsiheader.customHeader.institutionName`: Your institution name
   - `tsiheader.customHeader.institutionUrl`: Your institution website

### Study Mode Configuration

- `tsiheader.studyMode.enableSounds`: Enable audio notifications
- `tsiheader.studyMode.workDuration`: Work session duration (minutes)
- `tsiheader.studyMode.shortBreakDuration`: Short break duration (minutes)
- `tsiheader.studyMode.longBreakDuration`: Long break duration (minutes)
- `tsiheader.studyMode.sessionsBeforeLongBreak`: Sessions before long break

### Calendar Configuration

- `tsiheader.calendar.importUrl`: URL for importing calendar data
- `tsiheader.notifications.*`: Email notification settings

## Troubleshooting

### Common Issues

#### Extension Not Activating

**Problem**: Extension doesn't start or commands aren't available.

**Solutions**:

1. Ensure VS Code version is 1.74.0 or later
2. Reload the window (Ctrl+Shift+P → "Developer: Reload Window")
3. Check the Output panel for error messages
4. Restart VS Code completely

#### Headers Showing "Unknown" Username

**Problem**: Headers display "unknown" instead of your name.

**Solutions**:

1. Set `tsiheader.username` in VS Code settings
2. Update existing headers (right-click → "Update Header")
3. Check Git configuration: `git config --global user.name`

#### Headers Not Inserting

**Problem**: Nothing happens when trying to insert headers.

**Solutions**:

1. Check user settings (`tsiheader.username`, `tsiheader.email`)
2. Verify Ruby is installed and accessible
3. Check file permissions
4. Try with a different file type

#### Learning Module Issues

**Problem**: Learning features not working properly.

**Solutions**:

1. Ensure compilers are installed for the language you're learning
2. Check file permissions in the workspace
3. Clear curriculum cache (Ctrl+Shift+P → "Clear [Language] Cache")
4. Restart VS Code

#### Calendar Import/Export Issues

**Problem**: Calendar data not importing or exporting correctly.

**Solutions**:

1. Check file format (JSON or iCalendar)
2. Verify file permissions
3. Check network connectivity for URL imports
4. Validate JSON format

#### SMTP Connection Errors

**Problem**: Email notifications not working.

**Solutions**:

1. Verify SMTP server settings
2. Use port 587 for STARTTLS or 465 for direct TLS
3. Check firewall settings
4. Verify email credentials
5. Test with "Test Email Notification" command

#### C/C++ Compilation Errors

**Problem**: Learn Mode shows "g++ not found" error.

**Solutions**:

1. **Install MSYS2 (Recommended)**:
   - Download from [msys2.org](https://www.msys2.org/)
   - Install and run: `pacman -Syu`
   - Install toolchain: `pacman -S mingw-w64-x86_64-toolchain`
   - Add `C:\msys64\mingw64\bin` to PATH

2. **Alternative - MinGW-w64**:
   - Download from [winlibs.com](https://winlibs.com/)
   - Extract to `C:\MinGW`
   - Add `C:\MinGW\bin` to PATH

3. **Verify Installation**:
   - Open terminal: `g++ --version`
   - Restart VS Code
   - Re-run the exercise

### Getting Help

#### Check Logs

1. Open Output panel (View → Output)
2. Select "Uni Header" from the dropdown
3. Look for error messages

#### Report Issues

1. Check [GitHub Issues](https://github.com/st93642/TSI_Header/issues)
2. Search for existing issues
3. Create a new issue with:
   - VS Code version
   - Extension version
   - Steps to reproduce
   - Error messages
   - System information

#### Community Support

- GitHub Discussions
- Stack Overflow (tag: vscode-extensions)
- VS Code Discord

## Tips and Tricks

### Productivity Tips

#### Keyboard Shortcuts

- **Ctrl+Shift+P**: Open Command Palette
- **Ctrl+,**: Open Settings
- **F5**: Launch extension in debug mode
- **Ctrl+Shift+E**: Open Explorer

#### Efficient Workflow

1. **Set up once**: Configure your identity and preferences
2. **Use templates**: Create project templates for common setups
3. **Track progress**: Use the learning platform to track your studies
4. **Stay organized**: Use the calendar for deadline management
5. **Stay focused**: Use Study Mode for productive work sessions

#### Customization

1. **Custom headers**: Set up institution branding
2. **Timer settings**: Adjust Pomodoro timer to your preferences
3. **Calendar categories**: Organize events with meaningful categories
4. **Learning paths**: Choose curricula that match your goals

### Advanced Features

#### Batch Operations

- Use the Activity Bar panels for quick access to common commands
- Right-click in Explorer to create projects
- Use Command Palette for advanced operations

#### Integration

- Import existing calendar data
- Export your progress and calendar
- Use with other VS Code extensions

#### Automation

- Set up daily schedules for consistent study routines
- Use email notifications for deadline reminders
- Track learning progress automatically

### Best Practices

#### Learning

1. **Start with basics**: Complete foundational lessons first
2. **Practice regularly**: Use the Study Mode for consistent practice
3. **Review solutions**: Learn from provided solutions
4. **Track progress**: Monitor your learning statistics

#### Project Management

1. **Use templates**: Start with provided project templates
2. **Keep headers updated**: Use the update header feature
3. **Organize files**: Use proper project structure
4. **Version control**: Use Git with the extension

#### Study Sessions

1. **Use Pomodoro technique**: Work in focused 25-minute sessions
2. **Take breaks**: Use the built-in break reminders
3. **Track time**: Monitor your study statistics
4. **Stay consistent**: Use daily schedules for routine

### Troubleshooting Tips

#### Before Reporting Issues

1. **Check prerequisites**: Ensure all required software is installed
2. **Update everything**: Keep VS Code and the extension updated
3. **Restart VS Code**: Many issues are resolved with a restart
4. **Check logs**: Look at the Output panel for error messages
5. **Try different files**: Test with different file types

#### When Reporting Issues

1. **Be specific**: Describe exactly what you were doing
2. **Include details**: Version numbers, error messages, system info
3. **Provide steps**: How to reproduce the issue
4. **Check existing issues**: Search for similar problems first

This user guide should help you get the most out of the Uni Header extension. For additional help, refer to the API documentation or contact the development team.
