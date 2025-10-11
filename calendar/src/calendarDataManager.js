/**
 * Calendar Data Manager
 * Handles persistence of calendar data using VS Code global state
 */

class CalendarDataManager {
    constructor(context) {
        this.context = context;
        this.STORAGE_KEY = 'tsi.calendar.data';
    }

    /**
     * Get all calendar data
     */
    async getData() {
        const data = this.context.globalState.get(this.STORAGE_KEY, {
            deadlines: [],
            customEvents: [],
            dailySchedules: [],
            version: '1.0.0'
        });

        // Migrate data if needed
        return this.migrateData(data);
    }

    /**
     * Save calendar data
     */
    async saveData(data) {
        await this.context.globalState.update(this.STORAGE_KEY, data);
    }

    /**
     * Get deadlines
     */
    async getDeadlines() {
        const data = await this.getData();
        return data.deadlines || [];
    }

    /**
     * Add a deadline
     */
    async addDeadline(deadline) {
        const data = await this.getData();
        data.deadlines = data.deadlines || [];
        data.deadlines.push(deadline);
        await this.saveData(data);
    }

    /**
     * Update a deadline
     */
    async updateDeadline(deadlineId, updates) {
        const data = await this.getData();
        const index = data.deadlines.findIndex(d => d.id === deadlineId);
        if (index !== -1) {
            data.deadlines[index] = { ...data.deadlines[index], ...updates };
            await this.saveData(data);
        }
    }

    /**
     * Delete a deadline
     */
    async deleteDeadline(deadlineId) {
        const data = await this.getData();
        data.deadlines = data.deadlines.filter(d => d.id !== deadlineId);
        await this.saveData(data);
    }

    /**
     * Get custom events
     */
    async getCustomEvents() {
        const data = await this.getData();
        return data.customEvents || [];
    }

    /**
     * Add a custom event
     */
    async addCustomEvent(event) {
        const data = await this.getData();
        data.customEvents = data.customEvents || [];
        data.customEvents.push(event);
        await this.saveData(data);
    }

    /**
     * Update a custom event
     */
    async updateCustomEvent(eventId, updates) {
        const data = await this.getData();
        const index = data.customEvents.findIndex(e => e.id === eventId);
        if (index !== -1) {
            data.customEvents[index] = { ...data.customEvents[index], ...updates };
            await this.saveData(data);
        }
    }

    /**
     * Delete a custom event
     */
    async deleteCustomEvent(eventId) {
        const data = await this.getData();
        data.customEvents = data.customEvents.filter(e => e.id !== eventId);
        await this.saveData(data);
    }

    /**
     * Get daily schedules
     */
    async getDailySchedules() {
        const data = await this.getData();
        return data.dailySchedules || [];
    }

    /**
     * Add a daily schedule
     */
    async addDailySchedule(schedule) {
        const data = await this.getData();
        data.dailySchedules = data.dailySchedules || [];
        data.dailySchedules.push(schedule);
        await this.saveData(data);
    }

    /**
     * Update a daily schedule
     */
    async updateDailySchedule(scheduleId, updates) {
        const data = await this.getData();
        const index = data.dailySchedules.findIndex(s => s.id === scheduleId);
        if (index !== -1) {
            data.dailySchedules[index] = { ...data.dailySchedules[index], ...updates };
            await this.saveData(data);
        }
    }

    /**
     * Delete a daily schedule
     */
    async deleteDailySchedule(scheduleId) {
        const data = await this.getData();
        data.dailySchedules = data.dailySchedules.filter(s => s.id !== scheduleId);
        await this.saveData(data);
    }

    /**
     * Export all data
     */
    async exportData() {
        return await this.getData();
    }

    /**
     * Import data (with validation)
     */
    async importData(importData) {
        // Validate import data structure
        if (!importData || typeof importData !== 'object') {
            throw new Error('Invalid import data format');
        }

        // Basic validation
        const requiredFields = ['deadlines', 'customEvents', 'dailySchedules'];
        for (const field of requiredFields) {
            if (!Array.isArray(importData[field])) {
                importData[field] = [];
            }
        }

        // Add version if missing
        if (!importData.version) {
            importData.version = '1.0.0';
        }

        // Validate and clean data
        importData.deadlines = this.validateDeadlines(importData.deadlines);
        importData.customEvents = this.validateCustomEvents(importData.customEvents);
        importData.dailySchedules = this.validateDailySchedules(importData.dailySchedules);

        await this.saveData(importData);
    }

    /**
     * Validate deadlines array
     */
    validateDeadlines(deadlines) {
        if (!Array.isArray(deadlines)) return [];

        return deadlines.filter(deadline => {
            return deadline &&
                   typeof deadline === 'object' &&
                   deadline.id &&
                   deadline.title &&
                   deadline.dueDate &&
                   ['low', 'medium', 'high'].includes(deadline.priority);
        });
    }

    /**
     * Validate custom events array
     */
    validateCustomEvents(events) {
        if (!Array.isArray(events)) return [];

        return events.filter(event => {
            return event &&
                   typeof event === 'object' &&
                   event.id &&
                   event.title &&
                   event.date &&
                   (!event.time || typeof event.time === 'string') &&
                   (!event.endTime || typeof event.endTime === 'string');
        });
    }

    /**
     * Validate daily schedules array
     */
    validateDailySchedules(schedules) {
        if (!Array.isArray(schedules)) return [];

        return schedules.filter(schedule => {
            return schedule &&
                   typeof schedule === 'object' &&
                   schedule.id &&
                   schedule.title &&
                   schedule.startTime &&
                   schedule.endTime &&
                   Array.isArray(schedule.daysOfWeek);
        });
    }

    /**
     * Migrate data structure if needed
     */
    migrateData(data) {
        // Add version if missing
        if (!data.version) {
            data.version = '1.0.0';
        }

        // Ensure arrays exist
        if (!Array.isArray(data.deadlines)) data.deadlines = [];
        if (!Array.isArray(data.customEvents)) data.customEvents = [];
        if (!Array.isArray(data.dailySchedules)) data.dailySchedules = [];

        // Future migrations can be added here based on version

        return data;
    }

    /**
     * Export calendar data for backup
     */
    async exportData() {
        return await this.getData();
    }

    /**
     * Import calendar data from backup
     */
    async importData(data) {
        // Validate data structure
        if (!data || typeof data !== 'object') {
            throw new Error('Invalid calendar data format');
        }

        // Ensure required fields exist
        data.deadlines = data.deadlines || [];
        data.customEvents = data.customEvents || [];
        data.dailySchedules = data.dailySchedules || [];
        data.version = data.version || '1.0.0';

        // Validate and clean data
        data.deadlines = data.deadlines.filter(item => item && item.id && item.title);
        data.customEvents = data.customEvents.filter(item => item && item.id && item.title);
        data.dailySchedules = data.dailySchedules.filter(item => item && item.id && item.title);

        await this.saveData(data);
    }

    /**
     * Clear all data (for testing/reset)
     */
    async clearAllData() {
        await this.context.globalState.update(this.STORAGE_KEY, null);
    }
}

module.exports = { CalendarDataManager };