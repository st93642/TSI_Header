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
            timeZone: 'local', // Explicitly use local timezone
            firstDay: 1, // Start week on Monday (0 = Sunday, 1 = Monday)
            slotDuration: '00:30:00', // 30-minute time slots
            slotLabelInterval: '01:00:00', // Show labels every hour
            slotLabelFormat: {
                hour: '2-digit',
                minute: '2-digit',
                hour12: false // Force 24-hour format for time slots
            },
            eventTimeFormat: {
                hour: '2-digit',
                minute: '2-digit',
                hour12: false // Force 24-hour format
            },
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

        // Format date for display
        const d = new Date(dateTime);
        const monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        const displayDate = `${monthNames[d.getMonth()]}.${d.getDate().toString().padStart(2, '0')}.${d.getFullYear()}`;

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
                    <input type="text" id="event-date" value="${displayDate}" pattern="(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\.\\d{1,2}\\.\\d{4}" placeholder="Mmm.dd.yyyy" required>
                </div>
                <div class="form-group">
                    <label for="event-time">Start Time:</label>
                    <input type="text" id="event-time" value="${timeStr}" pattern="([01]?[0-9]|2[0-3]):[0-5][0-9]" placeholder="HH:MM (24-hour)" maxlength="5" required>
                    <small style="color: var(--vscode-descriptionForeground); font-size: 0.9em;">Format: HH:MM (24-hour, e.g., 14:30)</small>
                </div>
                <div class="form-group">
                    <label for="event-end-time">End Time (optional):</label>
                    <input type="text" id="event-end-time" pattern="([01]?[0-9]|2[0-3]):[0-5][0-9]" placeholder="HH:MM (24-hour)" maxlength="5">
                    <small style="color: var(--vscode-descriptionForeground); font-size: 0.9em;">Format: HH:MM (24-hour, e.g., 16:00)</small>
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
            let date = document.getElementById('event-date').value;
            const time = document.getElementById('event-time').value;
            const endTime = document.getElementById('event-end-time').value;
            const category = document.getElementById('event-category').value;

            if (!title || !date || !time) {
                showNotification('Please fill in all required fields', 'error');
                return;
            }

            // Validate time format
            if (!isValid24HourTime(time)) {
                showNotification('Please enter time in HH:MM format (24-hour)', 'error');
                return;
            }

            if (endTime && !isValid24HourTime(endTime)) {
                showNotification('Please enter end time in HH:MM format (24-hour)', 'error');
                return;
            }

            // Convert formatted date (Mmm.dd.yyyy) back to YYYY-MM-DD
            date = convertFormattedDateToISO(date);

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

        // Force 24-hour format for time inputs
        setTimeout(() => {
            const timeInputs = modal.querySelectorAll('input[type="text"][pattern*=":"]');
            timeInputs.forEach(input => {
                // Set input mode to numeric for better mobile experience
                input.setAttribute('inputmode', 'numeric');

                // Add input formatting
                const formatTimeInput = (value) => {
                    // Remove any non-numeric characters except colon
                    let cleanValue = value.replace(/[^0-9:]/g, '');

                    // Auto-format as user types
                    if (cleanValue.length >= 2 && cleanValue.charAt(2) !== ':') {
                        cleanValue = cleanValue.substring(0, 2) + ':' + cleanValue.substring(2);
                    }

                    // Limit to HH:MM format
                    if (cleanValue.length > 5) {
                        cleanValue = cleanValue.substring(0, 5);
                    }

                    return cleanValue;
                };

                // Apply formatting on input
                input.addEventListener('input', (e) => {
                    const formatted = formatTimeInput(e.target.value);
                    if (formatted !== e.target.value) {
                        e.target.value = formatted;
                    }
                });

                // Validate on blur
                input.addEventListener('blur', (e) => {
                    if (e.target.value && !isValid24HourTime(e.target.value)) {
                        e.target.style.borderColor = 'var(--vscode-errorForeground)';
                        showNotification('Invalid time format. Use HH:MM (24-hour)', 'error');
                    } else {
                        e.target.style.borderColor = 'var(--vscode-input-border)';
                    }
                });
            });
        }, 100);
    }
    function showEventDetailsModal(event, extendedProps) {
        const modal = createModal('Event Details', `
            <div class="event-details">
                <h3>${event.title}</h3>
                <p><strong>Type:</strong> ${getEventTypeLabel(extendedProps.type)}</p>
                <p><strong>Date:</strong> ${formatDate(event.start)}</p>
                ${event.end ? `<p><strong>End:</strong> ${formatDate(event.end)}</p>` : ''}
                ${extendedProps.data.description ? `
                    <div class="description-section">
                        <strong>Description:</strong>
                        <div class="description-content">${makeLinksClickable(extendedProps.data.description.replace(/\n/g, '<br>'))}</div>
                    </div>
                ` : ''}
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
                max-width: 700px;
                width: 90%;
                max-height: 90vh;
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
            .add-event-form input[type="time"] {
                /* Force 24-hour format display */
                font-variant-numeric: tabular-nums;
            }
            /* Comprehensive AM/PM hiding for all browsers */
            .add-event-form input[type="time"]::-webkit-datetime-edit-ampm-field,
            .add-event-form input[type="time"]::-webkit-datetime-edit-meridiem-field,
            .add-event-form input[type="time"]::-webkit-datetime-edit-text,
            .add-event-form input[type="time"]::-moz-datetime-edit-ampm-field,
            .add-event-form input[type="time"]::-ms-datetime-edit-ampm-field {
                display: none !important;
                visibility: hidden !important;
                opacity: 0 !important;
                width: 0 !important;
                height: 0 !important;
                margin: 0 !important;
                padding: 0 !important;
                border: none !important;
                background: transparent !important;
                position: absolute !important;
                clip: rect(0, 0, 0, 0) !important;
                overflow: hidden !important;
            }
            /* Force time input to show only hours and minutes */
            .add-event-form input[type="time"]::-webkit-datetime-edit-fields-wrapper {
                display: flex;
                align-items: center;
            }
            .add-event-form input[type="time"]::-webkit-datetime-edit-hour-field,
            .add-event-form input[type="time"]::-webkit-datetime-edit-minute-field {
                padding: 0 2px;
                width: 20px;
                text-align: center;
                font-variant-numeric: tabular-nums;
                border: none;
                background: transparent;
            }
            .add-event-form input[type="time"]::-webkit-datetime-edit-hour-field {
                border-right: 1px solid #ccc;
            }
            /* Firefox specific */
            .add-event-form input[type="time"] {
                -moz-appearance: textfield;
            }
            /* Ensure the picker indicator doesn't interfere */
            .add-event-form input[type="time"]::-webkit-calendar-picker-indicator {
                opacity: 0.8;
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
            .description-section {
                margin: 15px 0;
            }
            .description-section strong {
                display: block;
                margin-bottom: 8px;
                color: var(--vscode-foreground);
            }
            .description-content {
                background-color: var(--vscode-textBlockQuote-background);
                border: 1px solid var(--vscode-textBlockQuote-border);
                border-radius: 3px;
                padding: 12px;
                color: var(--vscode-descriptionForeground);
                max-height: 300px;
                overflow-y: auto;
                white-space: pre-wrap;
                word-wrap: break-word;
                font-family: var(--vscode-font-family);
                font-size: var(--vscode-font-size);
                line-height: 1.4;
            }
            .description-link {
                color: var(--vscode-textLink-foreground);
                text-decoration: underline;
                cursor: pointer;
            }
            .description-link:hover {
                color: var(--vscode-textLink-activeForeground);
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
                        ${deadline.description ? `• ${makeLinksClickable(deadline.description.length > 50 ? deadline.description.substring(0, 50) + '...' : deadline.description)}` : ''}
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
     * Convert URLs in text to clickable links
     */
    function makeLinksClickable(text) {
        if (!text) return text;

        try {
            // Regex to match URLs (http, https, ftp, etc.)
            const urlRegex = /(\b(https?|ftp|file):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;]*[-A-Z0-9+&@#\/%=~_|])/ig;

            // Replace URLs with clickable links
            return text.replace(urlRegex, function(url) {
                return `<a href="${url}" target="_blank" rel="noopener noreferrer" class="description-link">${url}</a>`;
            });
        } catch (error) {
            console.error('Error making links clickable:', error);
            return text; // Return original text if there's an error
        }
    }

    /**
     * Validate 24-hour time format (HH:MM)
     */
    function isValid24HourTime(timeString) {
        if (!timeString || typeof timeString !== 'string') return false;

        const timeRegex = /^([01]?[0-9]|2[0-3]):[0-5][0-9]$/;
        return timeRegex.test(timeString);
    }

    /**
     * Convert formatted date (Mmm.dd.yyyy) to ISO format (YYYY-MM-DD)
     */
    function convertFormattedDateToISO(formattedDate) {
        const monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        const parts = formattedDate.split('.');
        if (parts.length !== 3) return formattedDate;

        const monthName = parts[0];
        const day = parseInt(parts[1]);
        const year = parseInt(parts[2]);

        const monthIndex = monthNames.indexOf(monthName);
        if (monthIndex === -1) return formattedDate;

        const date = new Date(year, monthIndex, day);
        return date.toISOString().split('T')[0];
    }

    /**
     * Format date for display
     */
    function formatDate(date) {
        if (!date) return '';

        const d = new Date(date);
        const monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        const month = monthNames[d.getMonth()];
        const day = d.getDate().toString().padStart(2, '0');
        const year = d.getFullYear();
        // Force 24-hour format - use local time since times are now treated as local
        const hours = d.getHours().toString().padStart(2, '0');
        const minutes = d.getMinutes().toString().padStart(2, '0');
        return `${month}.${day}.${year} ${hours}:${minutes}`;
    }

})();