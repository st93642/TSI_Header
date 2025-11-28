let vscode;
try {
    vscode = require('vscode');
} catch (error) {
    // In test environment, use global mock
    vscode = global.vscode || {};
}

// In test environment, use dynamic global.vscode to allow test overrides
const getVSCode = () => process.env.NODE_ENV === 'test' ? global.vscode || vscode : vscode;
const path = require('path');
const { BaseManager } = require('../../core/src/baseManager');
const { CalendarDataManager } = require('./calendarDataManager');
const { CalendarEventManager } = require('./calendarEventManager');
const { CalendarWebviewProvider } = require('./calendarWebviewProvider');

class CalendarTreeDataProvider {
    constructor(eventManager) {
        this.eventManager = eventManager;
        this._onDidChangeTreeData = new vscode.EventEmitter();
        this.onDidChangeTreeData = this._onDidChangeTreeData.event;
    }

    refresh() {
        this._onDidChangeTreeData.fire();
    }

    getTreeItem(element) {
        return element;
    }

    async getChildren(element) {
        if (!element) {
            // Root level - show main categories and add buttons
            return [
                new CalendarTreeItem('➕ Add Deadline', vscode.TreeItemCollapsibleState.None, 'add-deadline'),
                new CalendarTreeItem('➕ Add Event', vscode.TreeItemCollapsibleState.None, 'add-event'),
                new CalendarTreeItem('➕ Add Schedule', vscode.TreeItemCollapsibleState.None, 'add-schedule'),
                new CalendarTreeItem('', vscode.TreeItemCollapsibleState.None, 'separator'), // Empty separator
                new CalendarTreeItem('📅 Open Full Calendar', vscode.TreeItemCollapsibleState.None, 'open-calendar'),
                new CalendarTreeItem('', vscode.TreeItemCollapsibleState.None, 'separator'), // Empty separator
                new CalendarTreeItem('Upcoming Deadlines', vscode.TreeItemCollapsibleState.Expanded, 'deadlines'),
                new CalendarTreeItem('Today\'s Schedule', vscode.TreeItemCollapsibleState.Expanded, 'today'),
                new CalendarTreeItem('This Week\'s Events', vscode.TreeItemCollapsibleState.Expanded, 'week'),
                new CalendarTreeItem('', vscode.TreeItemCollapsibleState.None, 'separator'), // Empty separator
                new CalendarTreeItem('📥 Import from File', vscode.TreeItemCollapsibleState.None, 'import-calendar'),
                new CalendarTreeItem('🌐 Import from URL', vscode.TreeItemCollapsibleState.None, 'import-calendar-url')
            ];
        }

        // Handle special items
        if (element.contextValue === 'open-calendar') {
            // This shouldn't have children, but just in case
            return [];
        }
        if (element.contextValue === 'separator') {
            return [];
        }
        if (element.contextValue === 'add-deadline') {
            return [];
        }
        if (element.contextValue === 'add-event') {
            return [];
        }
        if (element.contextValue === 'add-schedule') {
            return [];
        }
        if (element.contextValue === 'import-calendar') {
            return [];
        }
        if (element.contextValue === 'import-calendar-url') {
            return [];
        }

        // Child items
        switch (element.contextValue) {
            case 'deadlines':
                return await this.getUpcomingDeadlines();
            case 'today':
                return await this.getTodaysSchedule();
            case 'week':
                return await this.getThisWeeksEvents();
            default:
                return [];
        }
    }

    async getUpcomingDeadlines() {
        const deadlines = await this.eventManager.getUpcomingDeadlines(7); // Next 7 days
        return deadlines.map(deadline => {
            const formattedDate = this.formatDate(deadline.dueDate);
            const item = new CalendarTreeItem(
                `${deadline.title} (${formattedDate})`,
                vscode.TreeItemCollapsibleState.None,
                'deadline'
            );
            const truncatedDesc = this.truncateDescription(deadline.description || deadline.title);
            item.tooltip = truncatedDesc;
            item.iconPath = this.getPriorityIcon(deadline.priority);
            item.command = {
                command: 'tsiheader.showCalendar',
                title: 'Show Calendar',
                arguments: []
            };
            return item;
        });
    }

    async getTodaysSchedule() {
        const today = new Date().toISOString().split('T')[0];
        const schedules = await this.eventManager.getSchedulesForDate(today);
        return schedules.map(schedule => {
            const item = new CalendarTreeItem(
                `${schedule.startTime}-${schedule.endTime}: ${schedule.title}`,
                vscode.TreeItemCollapsibleState.None,
                'schedule'
            );
            item.tooltip = `${schedule.category} - ${schedule.title}`;
            item.iconPath = this.getCategoryIcon(schedule.category);
            return item;
        });
    }

    async getThisWeeksEvents() {
        const today = new Date();
        const weekEnd = new Date(today);
        weekEnd.setDate(today.getDate() + 7);

        const events = await this.eventManager.getEventsInRange(today.toISOString().split('T')[0], weekEnd.toISOString().split('T')[0]);
        return events.map(event => {
            const eventDate = event.start.split('T')[0]; // Extract date from start datetime
            const formattedDate = this.formatDate(eventDate);
            const item = new CalendarTreeItem(
                `${formattedDate}: ${event.title}`,
                vscode.TreeItemCollapsibleState.None,
                'event'
            );
            const truncatedDesc = this.truncateDescription(event.extendedProps.data.description || event.title);
            item.tooltip = truncatedDesc;
            item.iconPath = this.getCategoryIcon(event.extendedProps.data.category);
            item.command = {
                command: 'tsiheader.showCalendar',
                title: 'Show Calendar',
                arguments: []
            };
            return item;
        });
    }

    formatDate(dateString) {
        const date = new Date(dateString);
        const monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        const month = monthNames[date.getMonth()];
        const day = date.getDate().toString().padStart(2, '0');
        const year = date.getFullYear();
        return `${month}.${day}.${year}`;
    }

    formatDate(dateString) {
        const date = new Date(dateString);
        const monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        const month = monthNames[date.getMonth()];
        const day = date.getDate().toString().padStart(2, '0');
        const year = date.getFullYear();
        return `${month}.${day}.${year}`;
    }

    getPriorityIcon(priority) {
        switch (priority) {
            case 'high': return new vscode.ThemeIcon('warning');
            case 'medium': return new vscode.ThemeIcon('info');
            case 'low': return new vscode.ThemeIcon('check');
            default: return new vscode.ThemeIcon('calendar');
        }
    }

    getCategoryIcon(category) {
        switch (category?.toLowerCase()) {
            case 'study': return new vscode.ThemeIcon('mortar-board');
            case 'work': return new vscode.ThemeIcon('tools');
            case 'exercise': return new vscode.ThemeIcon('pulse');
            case 'meeting': return new vscode.ThemeIcon('organization');
            default: return new vscode.ThemeIcon('calendar');
        }
    }

    truncateDescription(description, maxLength = 100) {
        if (!description || description.length <= maxLength) {
            return description;
        }
        return description.substring(0, maxLength - 3) + '... (Click to view full details in calendar)';
    }
}

class CalendarTreeItem extends (vscode.TreeItem || class {}) {
    constructor(label, collapsibleState, contextValue) {
        if (vscode.TreeItem) {
            super(label, collapsibleState);
        }
        this.contextValue = contextValue;
        
        // Add command for open calendar item
        if (contextValue === 'open-calendar') {
            this.command = {
                command: 'tsiheader.showCalendar',
                title: 'Open Full Calendar',
                arguments: []
            };
            this.iconPath = new vscode.ThemeIcon('calendar');
            this.tooltip = 'Click to open the full calendar view';
        }

        // Add commands for add buttons
        if (contextValue === 'add-deadline') {
            this.command = {
                command: 'tsiheader.addCalendarDeadline',
                title: 'Add Deadline',
                arguments: []
            };
            this.iconPath = new vscode.ThemeIcon('clock');
            this.tooltip = 'Add a new deadline';
        }

        if (contextValue === 'add-event') {
            this.command = {
                command: 'tsiheader.addCalendarEvent',
                title: 'Add Event',
                arguments: []
            };
            this.iconPath = new vscode.ThemeIcon('add');
            this.tooltip = 'Add a new custom event';
        }

        if (contextValue === 'add-schedule') {
            this.command = {
                command: 'tsiheader.addCalendarSchedule',
                title: 'Add Schedule',
                arguments: []
            };
            this.iconPath = new vscode.ThemeIcon('history');
            this.tooltip = 'Add a new daily schedule';
        }

        if (contextValue === 'import-calendar') {
            this.command = {
                command: 'tsiheader.importCalendar',
                title: 'Import from File',
                arguments: []
            };
            this.iconPath = new vscode.ThemeIcon('file-code');
            this.tooltip = 'Import calendar data from a JSON file';
        }

        if (contextValue === 'import-calendar-url') {
            this.command = {
                command: 'tsiheader.importCalendarFromUrl',
                title: 'Import from URL',
                arguments: []
            };
            this.iconPath = new vscode.ThemeIcon('cloud-download');
            this.tooltip = 'Import calendar data from configured URL';
        }
    }
}

class CalendarManager extends BaseManager {
    constructor(context) {
        super(context);
        this.treeDataProvider = null;
        this.webviewProvider = null;
        this.dataManager = new CalendarDataManager(context);
        this.eventManager = new CalendarEventManager(this.dataManager);
    }

    /**
     * Phase 1: Register views (tree provider and webview provider)
     */
    registerViews(context) {
        if (this._initialized.views) {
            console.warn('CalendarManager: registerViews called multiple times, skipping');
            return;
        }
        this._initialized.views = true;

        try {
            this.treeDataProvider = new CalendarTreeDataProvider(this.eventManager);
            const treeDisposable = getVSCode().window.registerTreeDataProvider('tsi-calendar', this.treeDataProvider);
            this._addDisposable(treeDisposable);
            context.subscriptions.push(treeDisposable);
            console.log('CalendarManager: Tree provider registered successfully');
        } catch (error) {
            console.error('CalendarManager: Failed to register tree provider:', error);
            throw error;
        }
    }

    /**
     * Phase 2: Register commands
     */
    registerCommands(context) {
        if (this._initialized.commands) {
            console.warn('CalendarManager: registerCommands called multiple times, skipping');
            return;
        }
        this._initialized.commands = true;

        try {
            if (!this.webviewProvider) {
                this.webviewProvider = new CalendarWebviewProvider(
                    context.extensionUri, 
                    this.eventManager, 
                    this.treeDataProvider, 
                    context
                );
            }

            this._registerAllCommands(context);
            console.log('CalendarManager: Commands registered successfully');
        } catch (error) {
            console.error('CalendarManager: Failed to register commands:', error);
            throw error;
        }
    }

    /**
     * Phase 3: Setup listeners
     */
    setupListeners(context) {
        if (this._initialized.listeners) {
            console.warn('CalendarManager: setupListeners called multiple times, skipping');
            return;
        }
        this._initialized.listeners = true;
        console.log('CalendarManager: Listeners setup complete (no listeners needed)');
    }

    /**
     * Register all calendar commands
     */
    _registerAllCommands(context) {
        // Show calendar command - opens full calendar webview
        const showCalendarCmd = getVSCode().commands.registerCommand('tsiheader.showCalendar', async () => {
            await this.webviewProvider.createCalendarPanel();
        });

        // Add deadline command
        const addDeadlineCmd = getVSCode().commands.registerCommand('tsiheader.addCalendarDeadline', async () => {
            const deadline = await this.showDeadlineDialog();
            if (deadline) {
                await this.eventManager.addDeadline(deadline);
                this.treeDataProvider.refresh();
            }
        });

        // Add custom event command
        const addEventCmd = getVSCode().commands.registerCommand('tsiheader.addCalendarEvent', async () => {
            const event = await this.showEventDialog();
            if (event) {
                await this.eventManager.addCustomEvent(event);
                this.treeDataProvider.refresh();
            }
        });

        // Add daily schedule command
        const addScheduleCmd = getVSCode().commands.registerCommand('tsiheader.addCalendarSchedule', async () => {
            const schedule = await this.showScheduleDialog();
            if (schedule) {
                await this.eventManager.addDailySchedule(schedule);
                this.treeDataProvider.refresh();
            }
        });

        // Export calendar command
        const exportCmd = getVSCode().commands.registerCommand('tsiheader.exportCalendar', async () => {
            await this.exportCalendar();
        });

        // Import calendar command
        const importCmd = getVSCode().commands.registerCommand('tsiheader.importCalendar', async () => {
            await this.importCalendar();
            this.treeDataProvider.refresh();
        });

        // Import calendar from URL command
        const importFromUrlCmd = getVSCode().commands.registerCommand('tsiheader.importCalendarFromUrl', async () => {
            await this.importCalendarFromUrl();
            this.treeDataProvider.refresh();
        });

        // Track all commands for disposal
        const commands = [
            showCalendarCmd,
            addDeadlineCmd,
            addEventCmd,
            addScheduleCmd,
            exportCmd,
            importCmd,
            importFromUrlCmd
        ];

        commands.forEach(cmd => {
            this._addDisposable(cmd);
            context.subscriptions.push(cmd);
        });
    }

    /**
     * Show dialog to add a deadline
     */
    async showDeadlineDialog() {
        const title = await vscode.window.showInputBox({
            prompt: 'Enter deadline title',
            placeHolder: 'e.g., Complete Rust Chapter 5'
        });

        if (!title) return null;

        const description = await vscode.window.showInputBox({
            prompt: 'Enter deadline description (optional)',
            placeHolder: 'Additional details...'
        });

        const date = await vscode.window.showInputBox({
            prompt: 'Enter due date (YYYY-MM-DD)',
            placeHolder: '2025-12-31',
            validateInput: (value) => {
                const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
                if (!dateRegex.test(value)) {
                    return 'Please enter date in YYYY-MM-DD format';
                }
                return null;
            }
        });

        if (!date) return null;

        const priority = await vscode.window.showQuickPick(['Low', 'Medium', 'High'], {
            placeHolder: 'Select priority level'
        });

        if (!priority) return null;

        return {
            id: Date.now().toString(),
            title,
            description: description || '',
            dueDate: date,
            priority: priority.toLowerCase(),
            completed: false,
            createdAt: new Date().toISOString()
        };
    }

    /**
     * Show dialog to add a custom event
     */
    async showEventDialog() {
        const title = await vscode.window.showInputBox({
            prompt: 'Enter event title',
            placeHolder: 'e.g., Project Review Meeting'
        });

        if (!title) return null;

        const description = await vscode.window.showInputBox({
            prompt: 'Enter event description (optional)',
            placeHolder: 'Additional details...'
        });

        const date = await vscode.window.showInputBox({
            prompt: 'Enter event date (YYYY-MM-DD)',
            placeHolder: '2025-12-31',
            validateInput: (value) => {
                const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
                if (!dateRegex.test(value)) {
                    return 'Please enter date in YYYY-MM-DD format';
                }
                return null;
            }
        });

        if (!date) return null;

        const category = await vscode.window.showQuickPick(
            ['Study', 'Work', 'Personal', 'Meeting', 'Other'],
            { placeHolder: 'Select category' }
        );

        return {
            id: Date.now().toString(),
            title,
            description: description || '',
            date,
            category: category || 'Other',
            createdAt: new Date().toISOString()
        };
    }

    /**
     * Show dialog to add a daily schedule
     */
    async showScheduleDialog() {
        const title = await vscode.window.showInputBox({
            prompt: 'Enter schedule title',
            placeHolder: 'e.g., Morning Study Session'
        });

        if (!title) return null;

        const startTime = await vscode.window.showInputBox({
            prompt: 'Enter start time (HH:MM)',
            placeHolder: '09:00',
            validateInput: (value) => {
                const timeRegex = /^\d{2}:\d{2}$/;
                if (!timeRegex.test(value)) {
                    return 'Please enter time in HH:MM format';
                }
                return null;
            }
        });

        if (!startTime) return null;

        const endTime = await vscode.window.showInputBox({
            prompt: 'Enter end time (HH:MM)',
            placeHolder: '11:00',
            validateInput: (value) => {
                const timeRegex = /^\d{2}:\d{2}$/;
                if (!timeRegex.test(value)) {
                    return 'Please enter time in HH:MM format';
                }
                return null;
            }
        });

        if (!endTime) return null;

        const days = await vscode.window.showQuickPick(
            [
                { label: 'Monday-Friday', value: [1, 2, 3, 4, 5] },
                { label: 'Monday-Sunday', value: [0, 1, 2, 3, 4, 5, 6] },
                { label: 'Weekends', value: [0, 6] },
                { label: 'Custom', value: 'custom' }
            ],
            { placeHolder: 'Select days of week' }
        );

        let daysOfWeek = [];
        if (days && days.value !== 'custom') {
            daysOfWeek = days.value;
        } else if (days && days.value === 'custom') {
            // For now, default to weekdays
            daysOfWeek = [1, 2, 3, 4, 5];
        }

        const category = await vscode.window.showQuickPick(
            ['Study', 'Work', 'Exercise', 'Personal', 'Other'],
            { placeHolder: 'Select category' }
        );

        return {
            id: Date.now().toString(),
            title,
            startTime,
            endTime,
            daysOfWeek,
            category: category || 'Other',
            createdAt: new Date().toISOString()
        };
    }

    /**
     * Export calendar data
     */
    async exportCalendar() {
        try {
            const data = await this.dataManager.exportData();
            const jsonString = JSON.stringify(data, null, 2);

            const uri = await vscode.window.showSaveDialog({
                defaultUri: vscode.Uri.file('study-calendar-export.json'),
                filters: {
                    'JSON files': ['json'],
                    'All files': ['*']
                }
            });

            if (uri) {
                await vscode.workspace.fs.writeFile(uri, Buffer.from(jsonString, 'utf8'));
                vscode.window.showInformationMessage('Calendar exported successfully!');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to export calendar: ${error.message}`);
        }
    }

    /**
     * Import calendar data from file
     */
    async importCalendar() {
        try {
            // Show file picker
            const uri = await vscode.window.showOpenDialog({
                canSelectFiles: true,
                canSelectFolders: false,
                canSelectMany: false,
                defaultUri: vscode.workspace.workspaceFolders?.[0]?.uri,
                filters: {
                    'Calendar files': ['json', 'ics'],
                    'JSON files': ['json'],
                    'iCalendar files': ['ics'],
                    'All files': ['*']
                },
                openLabel: 'Import Calendar'
            });

            if (!uri || uri.length === 0) {
                return; // User cancelled
            }

            const fileUri = uri[0];

                        // Show progress
            await vscode.window.withProgress({
                location: vscode.ProgressLocation.Notification,
                title: 'Merging calendar data from file...',
                cancellable: false
            }, async (progress) => {
                progress.report({ increment: 0, message: 'Reading file...' });

                // Read file content
                const fileContent = await vscode.workspace.fs.readFile(fileUri);
                const content = Buffer.from(fileContent).toString('utf8');

                progress.report({ increment: 30, message: 'Parsing data...' });

                let data;
                if (content.trim().startsWith('BEGIN:VCALENDAR')) {
                    // Parse iCalendar format
                    data = this.parseICalendar(content);
                } else {
                    // Parse JSON format
                    try {
                        data = JSON.parse(content);
                    } catch (error) {
                        throw new Error('Invalid JSON format in calendar file');
                    }
                }

                progress.report({ increment: 70, message: 'Merging with existing calendar...' });

                // Import the data
                await this.dataManager.importData(data);

                progress.report({ increment: 100, message: 'Complete!' });
            });

            vscode.window.showInformationMessage('Calendar data merged successfully! New events added and duplicates updated.');

        } catch (error) {
            vscode.window.showErrorMessage(`Failed to import calendar from file: ${error.message}`);
        }
    }
    async importCalendarFromUrl() {
        try {
            // Get the import URL from configuration
            const config = vscode.workspace.getConfiguration('tsiheader');
            const importUrl = config.get('calendar.importUrl');

            if (!importUrl) {
                const setUrl = await vscode.window.showInformationMessage(
                    'No calendar import URL configured. Would you like to set one?',
                    'Set URL', 'Cancel'
                );

                if (setUrl === 'Set URL') {
                    const url = await vscode.window.showInputBox({
                        prompt: 'Enter calendar import URL',
                        placeHolder: 'https://example.com/calendar/export.php?...',
                    });

                    if (url) {
                        await config.update('calendar.importUrl', url, vscode.ConfigurationTarget.Global);
                        vscode.window.showInformationMessage('Calendar import URL saved. You can now import calendar data.');
                    }
                }
                return;
            }

            // Show progress
            await vscode.window.withProgress({
                location: vscode.ProgressLocation.Notification,
                title: 'Merging calendar data from URL...',
                cancellable: false
            }, async (progress) => {
                progress.report({ increment: 0, message: 'Fetching data...' });

                try {
                    const response = await this.fetchUrl(importUrl);
                    progress.report({ increment: 50, message: 'Parsing data...' });

                    let data;
                    if (response.trim().startsWith('BEGIN:VCALENDAR')) {
                        // Parse iCalendar format
                        data = this.parseICalendar(response);
                    } else {
                        // Parse JSON format
                        data = JSON.parse(response);
                    }

                    progress.report({ increment: 75, message: 'Merging with existing calendar...' });

                    await this.dataManager.importData(data);
                    progress.report({ increment: 100, message: 'Complete!' });

                } catch (error) {
                    throw new Error(`Failed to import calendar: ${error.message}`);
                }
            });

            vscode.window.showInformationMessage('Calendar data merged successfully from URL! New events added and duplicates updated.');

        } catch (error) {
            vscode.window.showErrorMessage(`Failed to import calendar from URL: ${error.message}`);
        }
    }

    /**
     * Parse iCalendar format and convert to calendar data structure
     */
    parseICalendar(icsData) {
        const events = [];
        // First, unfold folded lines (lines that start with a space are continuations)
        // Handle both CRLF and LF line endings, and various folding patterns
        const unfoldedData = icsData.replace(/\r?\n[ \t]/g, '');
        const lines = unfoldedData.split('\n');
        let currentEvent = null;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i].trim();

            if (line === 'BEGIN:VEVENT') {
                currentEvent = {};
            } else if (line === 'END:VEVENT') {
                if (currentEvent) {
                    events.push(currentEvent);
                    currentEvent = null;
                }
            } else if (currentEvent && line.includes(':')) {
                const [key, ...valueParts] = line.split(':');
                const value = valueParts.join(':');

                switch (key) {
                    case 'SUMMARY':
                        currentEvent.title = value;
                        break;
                    case 'DESCRIPTION':
                        currentEvent.description = value.replace(/\\n/g, '\n').replace(/\\,/g, ',');
                        break;
                    case 'DTSTART':
                        const startInfo = this.parseICalendarDateTime(value);
                        currentEvent.startDate = startInfo.date;
                        currentEvent.startTime = startInfo.time;
                        break;
                    case 'DTEND':
                        const endInfo = this.parseICalendarDateTime(value);
                        currentEvent.endDate = endInfo.date;
                        currentEvent.endTime = endInfo.time;
                        break;
                    case 'DUE':
                        currentEvent.dueDate = this.parseICalendarDate(value);
                        break;
                    case 'UID':
                        currentEvent.id = value;
                        break;
                }
            }
        }

        // Convert to calendar data format
        const calendarData = {
            deadlines: [],
            customEvents: [],
            dailySchedules: [],
            version: '1.0.0'
        };

        events.forEach(event => {
            if (event.dueDate) {
                // This is a deadline
                calendarData.deadlines.push({
                    id: event.id || Date.now().toString(),
                    title: event.title || 'Imported Deadline',
                    description: event.description || '',
                    dueDate: event.dueDate,
                    priority: 'medium',
                    completed: false,
                    createdAt: new Date().toISOString()
                });
            } else if (event.startDate) {
                // This is an event
                const customEvent = {
                    id: event.id || Date.now().toString(),
                    title: event.title || 'Imported Event',
                    description: event.description || '',
                    date: event.startDate,
                    category: 'Other',
                    createdAt: new Date().toISOString()
                };

                // Add time information if available
                if (event.startTime) {
                    customEvent.time = event.startTime;
                }
                if (event.endTime) {
                    customEvent.endTime = event.endTime;
                }

                calendarData.customEvents.push(customEvent);
            }
        });

        return calendarData;
    }

    /**
     * Parse iCalendar date/time format - display raw calendar times
     */
    parseICalendarDateTime(dateTimeString) {
        // Parse iCalendar date/time and display exactly as in remote calendar
        // Handle timezone information properly

        let dateTime = dateTimeString;
        let isUTC = false;

        // Handle timezone indicators
        if (dateTimeString.includes(';TZID=')) {
            // Extract just the date/time part after TZID parameter
            const tzidMatch = dateTimeString.match(/^[^:]*;TZID=[^:]+:(.*)$/);
            if (tzidMatch) {
                dateTime = tzidMatch[1];
                // Times with TZID are in the specified timezone - treat as local time
            }
        }

        // Handle UTC indicator (Z)
        if (dateTime.endsWith('Z')) {
            dateTime = dateTime.slice(0, -1);
            isUTC = true;
        }

        // Parse the date/time components
        let match;
        if (dateTime.includes('T')) {
            // DateTime format: YYYYMMDDTHHMMSS
            match = dateTime.match(/^(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})$/);
            if (match) {
                const [, year, month, day, hour, minute, second] = match;

                // If it's marked as UTC, convert to local time for display
                let displayHour = parseInt(hour);
                let displayMinute = parseInt(minute);

                if (isUTC) {
                    // Convert UTC to local time
                    const utcDate = new Date(`${year}-${month}-${day}T${hour}:${minute}:${second}Z`);
                    displayHour = utcDate.getHours();
                    displayMinute = utcDate.getMinutes();
                }

                const time = `${displayHour.toString().padStart(2, '0')}:${displayMinute.toString().padStart(2, '0')}`;
                return {
                    date: `${year}-${month}-${day}`,
                    time: time
                };
            }
        } else {
            // Date format: YYYYMMDD
            match = dateTime.match(/^(\d{4})(\d{2})(\d{2})$/);
            if (match) {
                const [, year, month, day] = match;
                return {
                    date: `${year}-${month}-${day}`,
                    time: null
                };
            }
        }

        return { date: dateTimeString, time: null }; // Return as-is if parsing fails
    }

    /**
     * Parse iCalendar date format (legacy method for DUE dates)
     */
    parseICalendarDate(dateString) {
        const result = this.parseICalendarDateTime(dateString);
        return result.date;
    }
    async fetchUrl(url) {
        return new Promise((resolve, reject) => {
            const https = require('https');
            const urlObj = new URL(url);

            const options = {
                hostname: urlObj.hostname,
                path: urlObj.pathname + urlObj.search,
                method: 'GET',
                headers: {
                    'User-Agent': 'TSI-Header-VSCode-Extension/6.2.0'
                }
            };

            const req = https.request(options, (res) => {
                let data = '';

                res.on('data', (chunk) => {
                    data += chunk;
                });

                res.on('end', () => {
                    if (res.statusCode >= 200 && res.statusCode < 300) {
                        resolve(data);
                    } else {
                        reject(new Error(`HTTP ${res.statusCode}: ${res.statusText}`));
                    }
                });
            });

            req.on('error', (error) => {
                reject(error);
            });

            req.setTimeout(30000, () => {
                req.destroy();
                reject(new Error('Request timeout'));
            });

            req.end();
        });
    }
}

module.exports = { CalendarManager };