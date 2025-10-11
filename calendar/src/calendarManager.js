const vscode = require('vscode');
const path = require('path');
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
                new CalendarTreeItem('This Week\'s Events', vscode.TreeItemCollapsibleState.Expanded, 'week')
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
            const item = new CalendarTreeItem(
                `${deadline.title} (${deadline.dueDate})`,
                vscode.TreeItemCollapsibleState.None,
                'deadline'
            );
            item.tooltip = deadline.description || deadline.title;
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
            const item = new CalendarTreeItem(
                `${eventDate}: ${event.title}`,
                vscode.TreeItemCollapsibleState.None,
                'event'
            );
            item.tooltip = event.extendedProps.data.description || event.title;
            item.iconPath = this.getCategoryIcon(event.extendedProps.data.category);
            item.command = {
                command: 'tsiheader.showCalendar',
                title: 'Show Calendar',
                arguments: []
            };
            return item;
        });
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
}

class CalendarTreeItem extends vscode.TreeItem {
    constructor(label, collapsibleState, contextValue) {
        super(label, collapsibleState);
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
    }
}

class CalendarManager {
    constructor(context) {
        this.context = context;
        this.treeDataProvider = null;
        this.webviewProvider = null;
        this.dataManager = new CalendarDataManager(context);
        this.eventManager = new CalendarEventManager(this.dataManager);
    }

    /**
     * Initialize the calendar module
     */
    async initialize() {
        // Register tree data provider for the calendar view
        this.treeDataProvider = new CalendarTreeDataProvider(this.eventManager);

        this.context.subscriptions.push(
            vscode.window.registerTreeDataProvider('tsi-calendar', this.treeDataProvider)
        );

        // Register webview provider for the full calendar modal
        this.webviewProvider = new CalendarWebviewProvider(this.context.extensionUri, this.eventManager, this.treeDataProvider);

        // Register commands
        this.registerCommands();

        // Sample data initialization removed - users should add their own events
    }

    /**
     * Register calendar commands
     */
    registerCommands() {
        // Show calendar command - opens full calendar webview
        const showCalendarCmd = vscode.commands.registerCommand('tsiheader.showCalendar', async () => {
            await this.webviewProvider.createCalendarPanel();
        });

        // Add deadline command
        const addDeadlineCmd = vscode.commands.registerCommand('tsiheader.addCalendarDeadline', async () => {
            const deadline = await this.showDeadlineDialog();
            if (deadline) {
                await this.eventManager.addDeadline(deadline);
                this.treeDataProvider.refresh();
            }
        });

        // Add custom event command
        const addEventCmd = vscode.commands.registerCommand('tsiheader.addCalendarEvent', async () => {
            const event = await this.showEventDialog();
            if (event) {
                await this.eventManager.addCustomEvent(event);
                this.treeDataProvider.refresh();
            }
        });

        // Add daily schedule command
        const addScheduleCmd = vscode.commands.registerCommand('tsiheader.addCalendarSchedule', async () => {
            const schedule = await this.showScheduleDialog();
            if (schedule) {
                await this.eventManager.addDailySchedule(schedule);
                this.treeDataProvider.refresh();
            }
        });

        // Export calendar command
        const exportCmd = vscode.commands.registerCommand('tsiheader.exportCalendar', async () => {
            await this.exportCalendar();
        });

        // Import calendar command
        const importCmd = vscode.commands.registerCommand('tsiheader.importCalendar', async () => {
            await this.importCalendar();
            this.treeDataProvider.refresh();
        });

        // Add to subscriptions
        this.context.subscriptions.push(
            showCalendarCmd,
            addDeadlineCmd,
            addEventCmd,
            addScheduleCmd,
            exportCmd,
            importCmd
        );
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
     * Import calendar data
     */
    async importCalendar() {
        try {
            const uris = await vscode.window.showOpenDialog({
                canSelectFiles: true,
                canSelectFolders: false,
                canSelectMany: false,
                filters: {
                    'JSON files': ['json'],
                    'All files': ['*']
                }
            });

            if (uris && uris[0]) {
                const content = await vscode.workspace.fs.readFile(uris[0]);
                const data = JSON.parse(content.toString());

                await this.dataManager.importData(data);
                this.treeDataProvider.refresh();
                vscode.window.showInformationMessage('Calendar imported successfully!');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to import calendar: ${error.message}`);
        }
    }
}

module.exports = { CalendarManager };