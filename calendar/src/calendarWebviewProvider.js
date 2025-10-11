const vscode = require('vscode');
const path = require('path');

/**
 * Calendar Webview Provider
 * Provides the calendar interface as a VS Code webview
 */

class CalendarWebviewProvider {
    constructor(extensionUri, eventManager, treeDataProvider) {
        this.extensionUri = extensionUri;
        this.eventManager = eventManager;
        this.treeDataProvider = treeDataProvider;
        this._currentPanel = null;
    }

    /**
     * Create and show a modal calendar webview panel
     */
    async createCalendarPanel() {
        const panel = vscode.window.createWebviewPanel(
            'calendarWebview',
            'Study Calendar',
            vscode.ViewColumn.One,
            {
                enableScripts: true,
                enableForms: true,
                localResourceRoots: [
                    vscode.Uri.joinPath(this.extensionUri, 'calendar', 'resources')
                ]
            }
        );

        this._currentPanel = panel;
        panel.webview.html = this._getHtmlForWebview(panel.webview);

        // Handle messages from the webview
        panel.webview.onDidReceiveMessage(
            async (message) => {
                await this._handleMessage(message);
            },
            undefined,
            [] // No subscriptions needed for modal panels
        );

        // Load initial data
        this.refresh();

        return panel;
    }

    /**
     * Get the HTML content for the webview (public method for modal panels)
     */
    getHtmlForWebview(webview) {
        return this._getHtmlForWebview(webview);
    }

    /**
     * Get the HTML content for the webview
     */
    _getHtmlForWebview(webview) {
        const nonce = getNonce();

        // Get resource URIs
        const styleUri = webview.asWebviewUri(
            vscode.Uri.joinPath(this.extensionUri, 'calendar', 'resources', 'calendar.css')
        );
        const scriptUri = webview.asWebviewUri(
            vscode.Uri.joinPath(this.extensionUri, 'calendar', 'resources', 'calendar.js')
        );

        return `<!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src ${webview.cspSource} 'unsafe-inline' https:; script-src 'nonce-${nonce}' https:; img-src https:; font-src https:;">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>Study Calendar</title>
            
            <!-- FullCalendar CSS -->
            <link href="https://cdn.jsdelivr.net/npm/fullcalendar@6.1.10/index.global.min.css" rel="stylesheet">
            <link href="${styleUri}" rel="stylesheet">
        </head>
        <body>
            <div class="calendar-container">
                <div class="calendar-header">
                    <h2>📅 Study Calendar</h2>
                    <div class="calendar-controls">
                        <button id="add-deadline-btn" class="btn btn-secondary">➕ Deadline</button>
                        <button id="add-event-btn" class="btn btn-secondary">➕ Event</button>
                        <button id="add-schedule-btn" class="btn btn-secondary">➕ Schedule</button>
                        <button id="today-btn" class="btn btn-primary">Today</button>
                        <button id="refresh-btn" class="btn btn-secondary">Refresh</button>
                    </div>
                </div>

                <div class="calendar-legend">
                    <div class="legend-item">
                        <span class="legend-color" style="background-color: #e74c3c;"></span>
                        <span>High Priority</span>
                    </div>
                    <div class="legend-item">
                        <span class="legend-color" style="background-color: #f39c12;"></span>
                        <span>Medium Priority</span>
                    </div>
                    <div class="legend-item">
                        <span class="legend-color" style="background-color: #27ae60;"></span>
                        <span>Low Priority</span>
                    </div>
                    <div class="legend-item">
                        <span class="legend-color" style="background-color: #3498db;"></span>
                        <span>Study Events</span>
                    </div>
                    <div class="legend-item">
                        <span class="legend-color" style="background-color: #2980b9;"></span>
                        <span>Study Schedule</span>
                    </div>
                </div>

                <div id="calendar"></div>

                <div class="upcoming-section">
                    <h3>📋 Upcoming Deadlines</h3>
                    <div id="upcoming-deadlines"></div>
                </div>

                <div class="today-section">
                    <h3>⏰ Today's Schedule</h3>
                    <div id="today-schedule"></div>
                </div>
            </div>

            <!-- FullCalendar JS -->
            <script src="https://cdn.jsdelivr.net/npm/fullcalendar@6.1.10/index.global.min.js"></script>
            <script nonce="${nonce}" src="${scriptUri}"></script>
        </body>
        </html>`;
    }

    /**
     * Handle messages from the webview
     */
    async _handleMessage(message) {
        console.log('Calendar webview received message:', message);

        switch (message.type) {
            case 'getEvents':
                await this._sendEvents(message.start, message.end);
                break;

            case 'getUpcomingDeadlines':
                await this._sendUpcomingDeadlines();
                break;

            case 'getTodaySchedule':
                await this._sendTodaySchedule();
                break;

            case 'deleteEvent':
                await this._deleteEvent(message.eventId, message.eventType);
                break;

            case 'toggleDeadline':
                await this._toggleDeadline(message.deadlineId);
                break;

            case 'addDeadline':
                await vscode.commands.executeCommand('tsiheader.addCalendarDeadline');
                break;

            case 'addEvent':
                await vscode.commands.executeCommand('tsiheader.addCalendarEvent');
                break;

            case 'addEventAtTime':
                await this._addEventAtTime(message.eventData);
                break;

            default:
                console.log('Unknown message type:', message.type);
        }
    }

    /**
     * Send calendar events to webview
     */
    async _sendEvents(startDate, endDate) {
        try {
            console.log('Sending events for date range:', startDate, 'to', endDate);
            const events = await this.eventManager.getEventsInRange(startDate, endDate);
            console.log('Retrieved events:', events);
            this._postMessage({
                type: 'eventsLoaded',
                events: events
            });
        } catch (error) {
            console.error('Error loading events:', error);
            this._postMessage({
                type: 'error',
                message: 'Failed to load calendar events'
            });
        }
    }

    /**
     * Send upcoming deadlines to webview
     */
    async _sendUpcomingDeadlines() {
        try {
            console.log('Sending upcoming deadlines');
            const deadlines = await this.eventManager.getUpcomingDeadlines();
            console.log('Retrieved deadlines:', deadlines);
            this._postMessage({
                type: 'upcomingDeadlinesLoaded',
                deadlines: deadlines
            });
        } catch (error) {
            console.error('Error loading upcoming deadlines:', error);
        }
    }

    /**
     * Send today's schedule to webview
     */
    async _sendTodaySchedule() {
        try {
            console.log('Sending today schedule');
            const today = new Date().toISOString().split('T')[0];
            const schedule = await this.eventManager.getSchedulesForDate(today);
            console.log('Retrieved schedule:', schedule);
            this._postMessage({
                type: 'todayScheduleLoaded',
                schedule: schedule
            });
        } catch (error) {
            console.error('Error loading today schedule:', error);
        }
    }

    /**
     * Delete an event
     */
    async _deleteEvent(eventId, eventType) {
        try {
            switch (eventType) {
                case 'deadline':
                    await this.eventManager.deleteDeadline(eventId);
                    break;
                case 'customEvent':
                    await this.eventManager.deleteCustomEvent(eventId);
                    break;
                case 'dailySchedule':
                    await this.eventManager.deleteDailySchedule(eventId);
                    break;
            }

            this._postMessage({
                type: 'eventDeleted',
                eventId: eventId,
                eventType: eventType
            });

            // Refresh the calendar
            this.refresh();

            // Refresh tree view
            if (this.treeDataProvider) {
                this.treeDataProvider.refresh();
            }

        } catch (error) {
            console.error('Error deleting event:', error);
            this._postMessage({
                type: 'error',
                message: 'Failed to delete event'
            });
        }
    }

    /**
     * Add event at specific time
     */
    async _addEventAtTime(eventData) {
        try {
            // Convert the event data to the format expected by the event manager
            const event = {
                id: eventData.id,
                title: eventData.title,
                description: eventData.description,
                date: eventData.date,
                time: eventData.time,
                endTime: eventData.endTime, // Include end time if provided
                category: eventData.category,
                createdAt: eventData.createdAt
            };

            await this.eventManager.addCustomEvent(event);

            this._postMessage({
                type: 'eventAdded',
                message: 'Event added successfully'
            });

            // Refresh the calendar
            this.refresh();

            // Refresh tree view
            if (this.treeDataProvider) {
                this.treeDataProvider.refresh();
            }

        } catch (error) {
            console.error('Error adding event at time:', error);
            this._postMessage({
                type: 'error',
                message: 'Failed to add event'
            });
        }
    }
    async _toggleDeadline(deadlineId) {
        try {
            await this.eventManager.toggleDeadlineCompletion(deadlineId);

            this._postMessage({
                type: 'deadlineToggled',
                deadlineId: deadlineId
            });

            // Refresh data
            await this._sendUpcomingDeadlines();

            // Refresh the calendar
            this.refresh();

            // Refresh tree view
            if (this.treeDataProvider) {
                this.treeDataProvider.refresh();
            }

        } catch (error) {
            console.error('Error toggling deadline:', error);
            this._postMessage({
                type: 'error',
                message: 'Failed to toggle deadline'
            });
        }
    }

    /**
     * Refresh the calendar data
     */
    async refresh() {
        if (this._currentPanel) {
            // Trigger refresh in webview
            this._postMessage({ type: 'refresh' });
        }
    }

    /**
     * Post a message to the webview
     */
    _postMessage(message) {
        if (this._currentPanel) {
            this._currentPanel.webview.postMessage(message);
        }
    }
}

/**
 * Generate a nonce for CSP
 */
function getNonce() {
    let text = '';
    const possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    for (let i = 0; i < 32; i++) {
        text += possible.charAt(Math.floor(Math.random() * possible.length));
    }
    return text;
}

module.exports = { CalendarWebviewProvider };