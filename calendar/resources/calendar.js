/**
 * Study Calendar JavaScript
 * Handles calendar UI and communication with VS Code extension
 */

(function() {
    'use strict';

    // VS Code API
    const vscode = acquireVsCodeApi();

    // Calendar instance
    let calendar;

    // Initialize when DOM is loaded
    document.addEventListener('DOMContentLoaded', function() {
        initializeCalendar();
        setupEventListeners();
        loadInitialData();
    });

    /**
     * Initialize FullCalendar
     */
    function initializeCalendar() {
        const calendarEl = document.getElementById('calendar');

        calendar = new FullCalendar.Calendar(calendarEl, {
            initialView: 'dayGridMonth',
            headerToolbar: {
                left: 'prev,next today',
                center: 'title',
                right: 'dayGridMonth,timeGridWeek,timeGridDay'
            },
            height: 'auto',
            events: [],
            eventClick: handleEventClick,
            dateClick: handleDateClick,
            datesSet: handleDatesSet,
            dayMaxEvents: 3,
            moreLinkClick: 'popover',
            nowIndicator: true,
            highlightDates: [new Date()], // Highlight today
            dayCellDidMount: function(info) {
                // Additional styling for today
                if (info.date.toDateString() === new Date().toDateString()) {
                    info.el.style.backgroundColor = 'rgba(52, 152, 219, 0.1)';
                    info.el.style.border = '2px solid #3498db';
                    info.el.style.borderRadius = '4px';
                }
            }
        });

        calendar.render();
    }

    /**
     * Setup event listeners
     */
    function setupEventListeners() {
        // Add buttons
        document.getElementById('add-deadline-btn').addEventListener('click', function() {
            vscode.postMessage({ type: 'addDeadline' });
        });

        document.getElementById('add-event-btn').addEventListener('click', function() {
            vscode.postMessage({ type: 'addEvent' });
        });

        document.getElementById('add-schedule-btn').addEventListener('click', function() {
            vscode.postMessage({ type: 'addSchedule' });
        });

        // Today button
        document.getElementById('today-btn').addEventListener('click', function() {
            calendar.today();
        });

        // Refresh button
        document.getElementById('refresh-btn').addEventListener('click', function() {
            loadInitialData();
        });
    }

    /**
     * Load initial calendar data
     */
    function loadInitialData() {
        // Check if calendar is initialized
        if (!calendar || !calendar.view) {
            console.log('Calendar not yet initialized, retrying...');
            setTimeout(loadInitialData, 100);
            return;
        }

        // Load events for current view
        const view = calendar.view;
        loadEvents(view.activeStart, view.activeEnd);

        // Load upcoming deadlines
        loadUpcomingDeadlines();

        // Load today's schedule
        loadTodaySchedule();
    }

    /**
     * Load calendar events for date range
     */
    function loadEvents(startDate, endDate) {
        vscode.postMessage({
            type: 'getEvents',
            start: startDate.toISOString().split('T')[0],
            end: endDate.toISOString().split('T')[0]
        });
    }

    /**
     * Load upcoming deadlines
     */
    function loadUpcomingDeadlines() {
        vscode.postMessage({
            type: 'getUpcomingDeadlines'
        });
    }

    /**
     * Load today's schedule
     */
    function loadTodaySchedule() {
        vscode.postMessage({
            type: 'getTodaySchedule'
        });
    }

    /**
     * Handle calendar event click
     */
    function handleEventClick(info) {
        const event = info.event;
        const extendedProps = event.extendedProps;

        // Show event details modal
        showEventDetailsModal(event, extendedProps);
    }

    /**
     * Handle calendar date click
     */
    function handleDateClick(info) {
        // Check if we're in day view and it's a time slot click
        if (calendar.view.type === 'timeGridDay' && info.date) {
            // This is a time slot click in day view - add event at this time
            showAddEventAtTimeModal(info.date);
        } else {
            // Regular date click - navigate to day view
            calendar.changeView('timeGridDay', info.dateStr);
        }
    }

    /**
     * Handle calendar dates change
     */
    function handleDatesSet(dateInfo) {
        loadEvents(dateInfo.start, dateInfo.end);
    }

    /**
     * Show add event modal with pre-filled date/time
     */
    function showAddEventAtTimeModal(dateTime) {
        const dateStr = dateTime.toISOString().split('T')[0];
        const timeStr = dateTime.toTimeString().substring(0, 5); // HH:MM format

        const modal = createModal('Add Event', `
            <div class="add-event-form">
                <div class="form-group">
                    <label for="event-title">Title:</label>
                    <input type="text" id="event-title" placeholder="Enter event title" required>
                </div>
                <div class="form-group">
                    <label for="event-description">Description (optional):</label>
                    <textarea id="event-description" placeholder="Enter event description"></textarea>
                </div>
                <div class="form-group">
                    <label for="event-date">Date:</label>
                    <input type="date" id="event-date" value="${dateStr}" required>
                </div>
                <div class="form-group">
                    <label for="event-time">Start Time:</label>
                    <input type="time" id="event-time" value="${timeStr}" required>
                </div>
                <div class="form-group">
                    <label for="event-end-time">End Time (optional):</label>
                    <input type="time" id="event-end-time" placeholder="Leave empty for 1 hour">
                </div>
                <div class="form-group">
                    <label for="event-category">Category:</label>
                    <select id="event-category">
                        <option value="Study">Study</option>
                        <option value="Work">Work</option>
                        <option value="Personal">Personal</option>
                        <option value="Meeting">Meeting</option>
                        <option value="Other">Other</option>
                    </select>
                </div>
            </div>
            <div class="modal-actions">
                <button id="add-event-submit" class="btn btn-primary">Add Event</button>
                <button id="add-event-cancel" class="btn btn-secondary">Cancel</button>
            </div>
        `);

        // Setup form event listeners
        document.getElementById('add-event-submit').addEventListener('click', () => {
            const title = document.getElementById('event-title').value.trim();
            const description = document.getElementById('event-description').value.trim();
            const date = document.getElementById('event-date').value;
            const time = document.getElementById('event-time').value;
            const endTime = document.getElementById('event-end-time').value;
            const category = document.getElementById('event-category').value;

            if (!title || !date || !time) {
                showNotification('Please fill in all required fields', 'error');
                return;
            }

            // Create event object
            const eventData = {
                id: Date.now().toString(),
                title,
                description,
                date,
                time,
                category,
                createdAt: new Date().toISOString()
            };

            // Add end time if provided
            if (endTime) {
                eventData.endTime = endTime;
            }

            // Send to extension
            vscode.postMessage({
                type: 'addEventAtTime',
                eventData: eventData
            });

            modal.remove();
        });

        document.getElementById('add-event-cancel').addEventListener('click', () => {
            modal.remove();
        });
    }
    function showEventDetailsModal(event, extendedProps) {
        const modal = createModal('Event Details', `
            <div class="event-details">
                <h3>${event.title}</h3>
                <p><strong>Type:</strong> ${getEventTypeLabel(extendedProps.type)}</p>
                <p><strong>Date:</strong> ${formatDate(event.start)}</p>
                ${event.end ? `<p><strong>End:</strong> ${formatDate(event.end)}</p>` : ''}
                ${extendedProps.data.description ? `<p><strong>Description:</strong> ${extendedProps.data.description}</p>` : ''}
                ${extendedProps.type === 'deadline' ? `<p><strong>Priority:</strong> ${extendedProps.data.priority}</p>` : ''}
                ${extendedProps.type === 'deadline' ? `<p><strong>Completed:</strong> ${extendedProps.data.completed ? 'Yes' : 'No'}</p>` : ''}
                ${extendedProps.type === 'customEvent' ? `<p><strong>Category:</strong> ${extendedProps.data.category}</p>` : ''}
                ${extendedProps.type === 'dailySchedule' ? `<p><strong>Category:</strong> ${extendedProps.data.category}</p>` : ''}
            </div>
            <div class="modal-actions">
                ${extendedProps.type === 'deadline' ?
                    `<button id="toggle-deadline" class="btn btn-primary">
                        ${extendedProps.data.completed ? 'Mark Incomplete' : 'Mark Complete'}
                    </button>` : ''}
                <button id="delete-event" class="btn btn-secondary delete-btn">Delete</button>
                <button id="close-modal" class="btn btn-secondary">Close</button>
            </div>
        `);

        // Setup modal event listeners
        document.getElementById('close-modal').addEventListener('click', () => {
            modal.remove();
        });

        if (extendedProps.type === 'deadline') {
            document.getElementById('toggle-deadline').addEventListener('click', () => {
                vscode.postMessage({
                    type: 'toggleDeadline',
                    deadlineId: extendedProps.data.id
                });
                modal.remove(); // Close modal immediately since user gets notification
            });
        }

        document.getElementById('delete-event').addEventListener('click', () => {
            // Show custom confirmation instead of using confirm()
            showDeleteConfirmation(extendedProps.data.id, extendedProps.type);
            modal.remove();
        });
    }

    /**
     * Create a modal dialog
     */
    function createModal(title, content) {
        const modal = document.createElement('div');
        modal.className = 'modal-overlay';
        modal.innerHTML = `
            <div class="modal">
                <div class="modal-header">
                    <h2>${title}</h2>
                    <button class="modal-close">&times;</button>
                </div>
                <div class="modal-body">
                    ${content}
                </div>
            </div>
        `;

        // Add modal styles
        const style = document.createElement('style');
        style.textContent = `
            .modal-overlay {
                position: fixed;
                top: 0;
                left: 0;
                width: 100%;
                height: 100%;
                background-color: rgba(0, 0, 0, 0.5);
                display: flex;
                justify-content: center;
                align-items: center;
                z-index: 1000;
            }
            .modal {
                background-color: var(--vscode-editor-background);
                border: 1px solid var(--vscode-panel-border);
                border-radius: 6px;
                max-width: 500px;
                width: 90%;
                max-height: 80vh;
                overflow-y: auto;
            }
            .modal-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 15px 20px;
                border-bottom: 1px solid var(--vscode-panel-border);
            }
            .modal-header h2 {
                margin: 0;
                color: var(--vscode-foreground);
            }
            .modal-close {
                background: none;
                border: none;
                font-size: 1.5em;
                cursor: pointer;
                color: var(--vscode-foreground);
            }
            .modal-body {
                padding: 20px;
            }
            .add-event-form .form-group {
                margin-bottom: 15px;
            }
            .add-event-form label {
                display: block;
                margin-bottom: 5px;
                color: var(--vscode-foreground);
                font-weight: 500;
            }
            .add-event-form input,
            .add-event-form textarea,
            .add-event-form select {
                width: 100%;
                padding: 8px 12px;
                border: 1px solid var(--vscode-input-border);
                border-radius: 3px;
                background-color: var(--vscode-input-background);
                color: var(--vscode-input-foreground);
                font-family: var(--vscode-font-family);
                font-size: var(--vscode-font-size);
                box-sizing: border-box;
            }
            .add-event-form input:focus,
            .add-event-form textarea:focus,
            .add-event-form select:focus {
                outline: none;
                border-color: var(--vscode-focusBorder);
            }
            .add-event-form textarea {
                resize: vertical;
                min-height: 60px;
            }
            .event-details h3 {
                margin-top: 0;
                color: var(--vscode-foreground);
            }
            .event-details p {
                margin: 8px 0;
                color: var(--vscode-descriptionForeground);
            }
            .modal-actions {
                display: flex;
                gap: 10px;
                justify-content: flex-end;
                margin-top: 20px;
            }
        `;
        document.head.appendChild(style);

        // Close modal when clicking overlay or close button
        modal.addEventListener('click', (e) => {
            if (e.target === modal || e.target.classList.contains('modal-close')) {
                modal.remove();
                style.remove();
            }
        });

        document.body.appendChild(modal);
        return modal;
    }

    /**
     * Handle messages from extension
     */
    window.addEventListener('message', event => {
        const message = event.data;
        console.log('Calendar webview received message from extension:', message);

        switch (message.type) {
            case 'eventsLoaded':
                console.log('Loading events into calendar:', message.events);
                calendar.removeAllEvents();
                calendar.addEventSource(message.events);
                break;

            case 'upcomingDeadlinesLoaded':
                console.log('Loading upcoming deadlines:', message.deadlines);
                renderUpcomingDeadlines(message.deadlines);
                break;

            case 'todayScheduleLoaded':
                console.log('Loading today schedule:', message.schedule);
                renderTodaySchedule(message.schedule);
                break;

            case 'eventDeleted':
                // Refresh calendar
                loadInitialData();
                showNotification('Event deleted successfully', 'success');
                break;

            case 'eventAdded':
                showNotification(message.message || 'Event added successfully', 'success');
                loadInitialData(); // Refresh calendar data
                break;

            case 'refresh':
                console.log('Refreshing calendar data');
                loadInitialData();
                break;

            case 'error':
                showNotification(message.message, 'error');
                break;
        }
    });

    /**
     * Render upcoming deadlines
     */
    function renderUpcomingDeadlines(deadlines) {
        const container = document.getElementById('upcoming-deadlines');

        if (!deadlines || deadlines.length === 0) {
            container.innerHTML = `
                <div class="empty-state">
                    <p>No upcoming deadlines</p>
                </div>
            `;
            return;
        }

        container.innerHTML = deadlines.map(deadline => `
            <div class="deadline-item ${deadline.completed ? 'completed' : ''}">
                <div class="deadline-info">
                    <div class="deadline-title">${deadline.title}</div>
                    <div class="deadline-meta">
                        Due: ${formatDate(new Date(deadline.dueDate))}
                        ${deadline.description ? `• ${deadline.description}` : ''}
                    </div>
                </div>
                <div class="deadline-actions">
                    <span class="deadline-priority priority-${deadline.priority}">
                        ${deadline.priority}
                    </span>
                    <input type="checkbox"
                           class="checkbox"
                           ${deadline.completed ? 'checked' : ''}
                           data-deadline-id="${deadline.id}">
                    <button class="delete-btn" data-event-id="${deadline.id}" data-event-type="deadline">×</button>
                </div>
            </div>
        `).join('');

        // Add event listeners for checkboxes and delete buttons
        container.querySelectorAll('.checkbox').forEach(checkbox => {
            checkbox.addEventListener('change', function() {
                const deadlineId = this.getAttribute('data-deadline-id');
                toggleDeadline(deadlineId);
            });
        });

        container.querySelectorAll('.delete-btn').forEach(button => {
            button.addEventListener('click', function() {
                const eventId = this.getAttribute('data-event-id');
                const eventType = this.getAttribute('data-event-type');
                deleteEvent(eventId, eventType);
            });
        });
    }

    /**
     * Render today's schedule
     */
    function renderTodaySchedule(schedule) {
        const container = document.getElementById('today-schedule');

        if (!schedule || schedule.length === 0) {
            container.innerHTML = `
                <div class="empty-state">
                    <p>No scheduled activities for today</p>
                </div>
            `;
            return;
        }

        container.innerHTML = schedule.map(item => `
            <div class="schedule-item">
                <div class="schedule-info">
                    <div class="schedule-title">${item.title}</div>
                    <div class="schedule-meta">
                        ${item.startTime} - ${item.endTime}
                        ${item.category ? `• ${item.category}` : ''}
                    </div>
                </div>
                <div class="schedule-actions">
                    <button class="delete-btn" data-event-id="${item.id}" data-event-type="dailySchedule">×</button>
                </div>
            </div>
        `).join('');

        // Add event listeners for delete buttons
        container.querySelectorAll('.delete-btn').forEach(button => {
            button.addEventListener('click', function() {
                const eventId = this.getAttribute('data-event-id');
                const eventType = this.getAttribute('data-event-type');
                deleteEvent(eventId, eventType);
            });
        });
    }

    /**
     * Toggle deadline completion
     */
    function toggleDeadline(deadlineId) {
        vscode.postMessage({
            type: 'toggleDeadline',
            deadlineId: deadlineId
        });
    }

    /**
     * Delete event
     */
    function deleteEvent(eventId, eventType) {
        // Create custom confirmation dialog instead of using confirm()
        showDeleteConfirmation(eventId, eventType);
    }

    /**
     * Show notification messages
     */
    function showNotification(message, type = 'info') {
        // Create a notification element
        const notification = document.createElement('div');
        notification.className = `notification ${type}`;
        notification.style.cssText = `
            position: fixed;
            top: 20px;
            right: 20px;
            background: ${type === 'success' ? '#4CAF50' : type === 'error' ? '#f44336' : '#2196F3'};
            color: white;
            padding: 12px 16px;
            border-radius: 4px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.3);
            z-index: 10000;
            font-family: var(--vscode-font-family);
            font-size: var(--vscode-font-size);
            max-width: 300px;
            word-wrap: break-word;
        `;
        notification.textContent = message;
        document.body.appendChild(notification);

        // Auto-remove after 3 seconds
        setTimeout(() => {
            if (notification.parentNode) {
                notification.remove();
            }
        }, 3000);
    }

    /**
     * Show custom delete confirmation dialog
     */
    function showDeleteConfirmation(eventId, eventType) {
        const modal = document.createElement('div');
        modal.className = 'delete-confirmation-modal';
        modal.innerHTML = `
            <div class="delete-modal-content">
                <h3>Delete Event</h3>
                <p>Are you sure you want to delete this event? This action cannot be undone.</p>
                <div class="delete-modal-buttons">
                    <button class="btn btn-secondary cancel-btn">Cancel</button>
                    <button class="btn btn-danger confirm-btn">Delete</button>
                </div>
            </div>
        `;

        // Add modal styles
        const style = document.createElement('style');
        style.textContent = `
            .delete-confirmation-modal {
                position: fixed;
                top: 0;
                left: 0;
                width: 100%;
                height: 100%;
                background-color: rgba(0, 0, 0, 0.5);
                display: flex;
                justify-content: center;
                align-items: center;
                z-index: 10000;
            }
            .delete-modal-content {
                background-color: var(--vscode-editor-background);
                border: 1px solid var(--vscode-panel-border);
                border-radius: 6px;
                padding: 20px;
                max-width: 400px;
                width: 90%;
                color: var(--vscode-foreground);
            }
            .delete-modal-content h3 {
                margin: 0 0 15px 0;
                color: var(--vscode-foreground);
            }
            .delete-modal-content p {
                margin: 0 0 20px 0;
                color: var(--vscode-descriptionForeground);
            }
            .delete-modal-buttons {
                display: flex;
                gap: 10px;
                justify-content: flex-end;
            }
            .confirm-btn {
                background-color: var(--vscode-errorForeground);
                color: white;
            }
            .confirm-btn:hover {
                background-color: var(--vscode-errorForeground);
                opacity: 0.8;
            }
        `;
        document.head.appendChild(style);

        // Handle button clicks
        modal.querySelector('.cancel-btn').addEventListener('click', () => {
            modal.remove();
            style.remove();
        });

        modal.querySelector('.confirm-btn').addEventListener('click', () => {
            vscode.postMessage({
                type: 'deleteEvent',
                eventId: eventId,
                eventType: eventType
            });
            modal.remove();
            style.remove();
        });

        // Close on background click
        modal.addEventListener('click', (e) => {
            if (e.target === modal) {
                modal.remove();
                style.remove();
            }
        });

        document.body.appendChild(modal);
    }

    /**
     * Get event type label
     */
    function getEventTypeLabel(eventType) {
        const labels = {
            deadline: 'Deadline',
            customEvent: 'Custom Event',
            dailySchedule: 'Daily Schedule'
        };
        return labels[eventType] || 'Event';
    }

    /**
     * Format date for display
     */
    function formatDate(date) {
        if (!date) return '';

        const d = new Date(date);
        return d.toLocaleDateString() + ' ' + d.toLocaleTimeString([], {hour: '2-digit', minute:'2-digit'});
    }

})();