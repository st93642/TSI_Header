/**
 * TSI Header Calendar Module
 * Main entry point for calendar functionality
 */

const { CalendarManager } = require('./calendarManager');
const { CalendarDataManager } = require('./calendarDataManager');
const { CalendarEventManager } = require('./calendarEventManager');
const { CalendarWebviewProvider } = require('./calendarWebviewProvider');

module.exports = {
    CalendarManager,
    CalendarDataManager,
    CalendarEventManager,
    CalendarWebviewProvider
};