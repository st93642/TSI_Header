/**
 * Calendar Event Manager
 * Handles CRUD operations for calendar events
 */

class CalendarEventManager {
    constructor(dataManager) {
        this.dataManager = dataManager;
    }

    /**
     * Get all events for calendar display
     */
    async getAllEvents() {
        const [deadlines, customEvents, dailySchedules] = await Promise.all([
            this.dataManager.getDeadlines(),
            this.dataManager.getCustomEvents(),
            this.dataManager.getDailySchedules()
        ]);

        return {
            deadlines,
            customEvents,
            dailySchedules
        };
    }

    /**
     * Add a deadline
     */
    async addDeadline(deadline) {
        // Validate deadline data
        this.validateDeadline(deadline);
        await this.dataManager.addDeadline(deadline);
    }

    /**
     * Update a deadline
     */
    async updateDeadline(deadlineId, updates) {
        // Validate updates
        if (updates.priority && !['low', 'medium', 'high'].includes(updates.priority)) {
            throw new Error('Invalid priority level');
        }

        if (updates.dueDate && !this.isValidDate(updates.dueDate)) {
            throw new Error('Invalid due date format');
        }

        await this.dataManager.updateDeadline(deadlineId, updates);
    }

    /**
     * Delete a deadline
     */
    async deleteDeadline(deadlineId) {
        await this.dataManager.deleteDeadline(deadlineId);
    }

    /**
     * Toggle deadline completion status
     */
    async toggleDeadlineCompletion(deadlineId) {
        const deadlines = await this.dataManager.getDeadlines();
        const deadline = deadlines.find(d => d.id === deadlineId);

        if (deadline) {
            await this.updateDeadline(deadlineId, {
                completed: !deadline.completed
            });
        }
    }

    /**
     * Add a custom event
     */
    async addCustomEvent(event) {
        // Validate event data
        this.validateCustomEvent(event);
        await this.dataManager.addCustomEvent(event);
    }

    /**
     * Update a custom event
     */
    async updateCustomEvent(eventId, updates) {
        // Validate updates
        if (updates.date && !this.isValidDate(updates.date)) {
            throw new Error('Invalid event date format');
        }

        await this.dataManager.updateCustomEvent(eventId, updates);
    }

    /**
     * Delete a custom event
     */
    async deleteCustomEvent(eventId) {
        await this.dataManager.deleteCustomEvent(eventId);
    }

    /**
     * Add a daily schedule
     */
    async addDailySchedule(schedule) {
        // Validate schedule data
        this.validateDailySchedule(schedule);
        await this.dataManager.addDailySchedule(schedule);
    }

    /**
     * Update a daily schedule
     */
    async updateDailySchedule(scheduleId, updates) {
        // Validate updates
        if (updates.daysOfWeek && !Array.isArray(updates.daysOfWeek)) {
            throw new Error('Days of week must be an array');
        }

        if (updates.startTime && !this.isValidTime(updates.startTime)) {
            throw new Error('Invalid start time format');
        }

        if (updates.endTime && !this.isValidTime(updates.endTime)) {
            throw new Error('Invalid end time format');
        }

        await this.dataManager.updateDailySchedule(scheduleId, updates);
    }

    /**
     * Delete a daily schedule
     */
    async deleteDailySchedule(scheduleId) {
        await this.dataManager.deleteDailySchedule(scheduleId);
    }

    /**
     * Get events for a specific date range
     */
    async getEventsInRange(startDate, endDate) {
        const allEvents = await this.getAllEvents();
        const events = [];

        // Add deadlines in range
        allEvents.deadlines.forEach(deadline => {
            if (this.isDateInRange(deadline.dueDate, startDate, endDate) && !deadline.completed) {
                events.push({
                    id: `deadline-${deadline.id}`,
                    title: `📅 ${deadline.title}`,
                    start: deadline.dueDate,
                    backgroundColor: this.getPriorityColor(deadline.priority),
                    borderColor: this.getPriorityColor(deadline.priority),
                    textColor: '#ffffff',
                    extendedProps: {
                        type: 'deadline',
                        data: deadline
                    }
                });
            }
        });

        // Add custom events in range
        allEvents.customEvents.forEach(event => {
            if (this.isDateInRange(event.date, startDate, endDate)) {
                let eventConfig = {
                    id: `event-${event.id}`,
                    title: `📝 ${event.title}`,
                    backgroundColor: this.getCategoryColor(event.category),
                    borderColor: this.getCategoryColor(event.category),
                    textColor: '#ffffff',
                    extendedProps: {
                        type: 'customEvent',
                        data: event
                    }
                };

                // Check if this is a timed event (has time field)
                if (event.time) {
                    // Timed event - create with specific start time (local timezone)
                    // Avoid exact 00:00:00 and 23:59:00 to prevent midnight spanning issues
                    let timeStr;
                    if (event.time === '00:00') {
                        timeStr = '00:00:01';
                    } else if (event.time === '23:59') {
                        timeStr = '23:58:00';
                    } else {
                        timeStr = `${event.time}:00`;
                    }
                    const startDateTime = `${event.date}T${timeStr}`; // No Z suffix - treat as local time
                    eventConfig.start = startDateTime;
                    eventConfig.allDay = false; // Explicitly not all-day

                    // If it has an end time, use it; otherwise create a default 1-hour duration
                    if (event.endTime) {
                        const endDateTime = `${event.date}T${event.endTime}:00`; // No Z suffix - treat as local time
                        eventConfig.end = endDateTime;
                    } else {
                        // Create a default 1-hour duration for timed events without explicit end time
                        const [hours, minutes] = event.time.split(':').map(Number);
                        const endHour = hours + 1;
                        const endTimeStr = endHour < 24 ? `${endHour.toString().padStart(2, '0')}:${minutes.toString().padStart(2, '0')}:00` : '23:59:00';
                        const endDateTime = `${event.date}T${endTimeStr}`; // No Z suffix - treat as local time
                        eventConfig.end = endDateTime;
                    }
                } else {
                    // All-day event
                    eventConfig.start = event.date;
                    eventConfig.allDay = true;
                }

                events.push(eventConfig);
            }
        });

        // Add recurring schedules
        const start = new Date(startDate);
        const end = new Date(endDate);

        allEvents.dailySchedules.forEach(schedule => {
            for (let date = new Date(start); date <= end; date.setDate(date.getDate() + 1)) {
                const dayOfWeek = date.getDay(); // 0 = Sunday, 1 = Monday, etc.

                if (schedule.daysOfWeek.includes(dayOfWeek)) {
                    const eventDate = date.toISOString().split('T')[0];

                    events.push({
                        id: `schedule-${schedule.id}-${eventDate}`,
                        title: `⏰ ${schedule.title}`,
                        start: `${eventDate}T${schedule.startTime}:00`, // No Z suffix - treat as local time
                        end: `${eventDate}T${schedule.endTime}:00`, // No Z suffix - treat as local time
                        backgroundColor: this.getScheduleColor(schedule.category),
                        borderColor: this.getScheduleColor(schedule.category),
                        textColor: '#ffffff',
                        extendedProps: {
                            type: 'dailySchedule',
                            data: schedule
                        }
                    });
                }
            }
        });

        return events;
    }

    /**
     * Get upcoming deadlines
     */
    async getUpcomingDeadlines(days = 7) {
        const deadlines = await this.dataManager.getDeadlines();
        const now = new Date();
        const futureDate = new Date();
        futureDate.setDate(now.getDate() + days);

        return deadlines
            .filter(deadline => {
                const dueDate = new Date(deadline.dueDate);
                return dueDate >= now && dueDate <= futureDate;
            })
            .sort((a, b) => new Date(a.dueDate) - new Date(b.dueDate));
    }

    /**
     * Get schedules for a specific date
     */
    async getSchedulesForDate(dateString) {
        const schedules = await this.dataManager.getDailySchedules();
        const date = new Date(dateString);
        const dayOfWeek = date.getDay(); // 0 = Sunday, 1 = Monday, etc.

        return schedules
            .filter(schedule => schedule.daysOfWeek.includes(dayOfWeek))
            .sort((a, b) => a.startTime.localeCompare(b.startTime));
    }

    // Validation methods

    validateDeadline(deadline) {
        if (!deadline.title || typeof deadline.title !== 'string') {
            throw new Error('Deadline title is required');
        }

        if (!deadline.dueDate || !this.isValidDate(deadline.dueDate)) {
            throw new Error('Valid due date is required');
        }

        if (!['low', 'medium', 'high'].includes(deadline.priority)) {
            throw new Error('Valid priority level is required');
        }
    }

    validateCustomEvent(event) {
        if (!event.title || typeof event.title !== 'string') {
            throw new Error('Event title is required');
        }

        if (!event.date || !this.isValidDate(event.date)) {
            throw new Error('Valid event date is required');
        }

        // Time is optional, but if provided, must be valid
        if (event.time && !this.isValidTime(event.time)) {
            throw new Error('Invalid time format');
        }

        // End time is optional, but if provided, must be valid
        if (event.endTime && !this.isValidTime(event.endTime)) {
            throw new Error('Invalid end time format');
        }
    }

    validateDailySchedule(schedule) {
        if (!schedule.title || typeof schedule.title !== 'string') {
            throw new Error('Schedule title is required');
        }

        if (!schedule.startTime || !this.isValidTime(schedule.startTime)) {
            throw new Error('Valid start time is required');
        }

        if (!schedule.endTime || !this.isValidTime(schedule.endTime)) {
            throw new Error('Valid end time is required');
        }

        if (!Array.isArray(schedule.daysOfWeek)) {
            throw new Error('Days of week must be an array');
        }
    }

    // Utility methods

    isValidDate(dateString) {
        const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
        if (!dateRegex.test(dateString)) return false;

        const date = new Date(dateString);
        return date instanceof Date && !isNaN(date);
    }

    isValidTime(timeString) {
        const timeRegex = /^\d{2}:\d{2}$/;
        return timeRegex.test(timeString);
    }

    isDateInRange(dateString, startDate, endDate) {
        const date = new Date(dateString);
        const start = new Date(startDate);
        const end = new Date(endDate);

        return date >= start && date <= end;
    }

    getPriorityColor(priority) {
        switch (priority) {
            case 'high': return '#e74c3c'; // Red
            case 'medium': return '#f39c12'; // Orange
            case 'low': return '#27ae60'; // Green
            default: return '#95a5a6'; // Gray
        }
    }

    getCategoryColor(category) {
        const colors = {
            'Study': '#3498db', // Blue
            'Work': '#9b59b6', // Purple
            'Personal': '#1abc9c', // Teal
            'Meeting': '#e67e22', // Orange
            'Other': '#95a5a6' // Gray
        };
        return colors[category] || colors['Other'];
    }

    getScheduleColor(category) {
        const colors = {
            'Study': '#2980b9', // Dark Blue
            'Work': '#8e44ad', // Dark Purple
            'Exercise': '#16a085', // Dark Teal
            'Personal': '#d35400', // Dark Orange
            'Other': '#7f8c8d' // Dark Gray
        };
        return colors[category] || colors['Other'];
    }
}

module.exports = { CalendarEventManager };