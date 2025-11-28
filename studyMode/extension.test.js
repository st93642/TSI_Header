/**
 * Study Mode Extension - Test Suite
 * Tests for persistence, analytics, and reset functionality
 * Using Node.js built-in test runner
 */

const test = require('node:test');
const assert = require('node:assert');
const { StudyModeExtension } = require('../core/src/studyModeExtension');
const { createVSCodeMock } = require('../test/utils/vscodeMock');
const { createMockExtensionContext } = require('../test/utils/globalStateMock');

const defaultStudyModeConfiguration = {
    workDuration: 25,
    shortBreakDuration: 5,
    longBreakDuration: 15,
    sessionsBeforeLongBreak: 4
};

const createStudyModeVSCode = () => createVSCodeMock({
    configuration: {
        'tsiheader.studyMode': { ...defaultStudyModeConfiguration }
    }
});

test('StudyModeExtension - State Persistence', async (t) => {
    let extension;
    let mockContext;

    t.beforeEach(() => {
        const mockVSCode = createStudyModeVSCode();
        mockContext = createMockExtensionContext();
        extension = new StudyModeExtension(mockVSCode, mockContext);
    });

    t.afterEach(() => {
        if (extension) {
            extension.dispose();
        }
    });

    await t.test('should load persisted state on activation', () => {
        // Mock persisted state
        const persistedState = {
            currentPhase: 'work',
            currentSession: 1,
            isRunning: true,
            startTime: Date.now() - 10000, // 10 seconds ago as timestamp
            remainingTime: 25 * 60 * 1000 - 10000,
            sessionLog: [{
                type: 'work',
                startTime: new Date().toISOString(),
                endTime: new Date().toISOString(),
                duration: 1000,
                completed: true,
                sessionNumber: 0
            }]
        };

        mockContext.globalState.get = (key) => {
            if (key === 'studyMode.state') return persistedState;
            return undefined;
        };

        extension.activate();

        // Check that state was restored (but timer should not auto-start after restart)
        assert.equal(extension.timer.currentPhase, 'work');
        assert.equal(extension.timer.currentSession, 1);
        assert.equal(extension.timer.isRunning, false); // Should be false after restart
        assert.equal(extension.timer.sessionLog.length, 1);
        // Should have calculated remaining time based on elapsed time
        assert(extension.timer.remainingTime > 0);
    });

    await t.test('should save state on deactivation', () => {
        let savedState = null;
        mockContext.globalState.update = (key, value) => {
            if (key === 'studyMode.state') {
                savedState = value;
            }
            return Promise.resolve();
        };

        extension.activate();
        extension.timer.start();
        extension.dispose();

        // Check that state was saved
        assert(savedState);
        assert.equal(savedState.currentPhase, 'work');
        assert.equal(savedState.isRunning, true);
        assert(savedState.phaseStartTimestamp); // Changed from startTime
        assert(savedState.elapsedTime >= 0); // Check elapsed time instead
    });

    await t.test('should handle missing persisted state gracefully', () => {
        mockContext.globalState.get = () => undefined;

        extension.activate();

        // Should initialize with default state
        assert.equal(extension.timer.currentPhase, 'stopped');
        assert.equal(extension.timer.currentSession, 0);
        assert.equal(extension.timer.isRunning, false);
        assert.equal(extension.timer.sessionLog.length, 0);
    });
});

test('StudyModeExtension - Productivity Analytics', async (t) => {
    let extension;
    let mockContext;

    t.beforeEach(() => {
        const mockVSCode = createStudyModeVSCode();
        mockContext = createMockExtensionContext();
        extension = new StudyModeExtension(mockVSCode, mockContext);
        extension.activate();
    });

    t.afterEach(() => {
        if (extension) {
            extension.dispose();
        }
    });

    await t.test('should calculate productivity analytics for different time periods', () => {
        const now = new Date();
        const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
        const yesterday = new Date(today);
        yesterday.setDate(yesterday.getDate() - 1);

        // Add sample sessions
        extension.timer.sessionLog = [
            // Today's sessions
            {
                type: 'work',
                startTime: new Date(today.getTime() + 1000).toISOString(),
                endTime: new Date(today.getTime() + 25 * 60 * 1000 + 1000).toISOString(),
                duration: 25 * 60 * 1000,
                completed: true,
                sessionNumber: 0
            },
            {
                type: 'work',
                startTime: new Date(today.getTime() + 30 * 60 * 1000).toISOString(),
                endTime: new Date(today.getTime() + 55 * 60 * 1000).toISOString(),
                duration: 25 * 60 * 1000,
                completed: true,
                sessionNumber: 1
            },
            // Yesterday's session
            {
                type: 'work',
                startTime: yesterday.toISOString(),
                endTime: new Date(yesterday.getTime() + 25 * 60 * 1000).toISOString(),
                duration: 25 * 60 * 1000,
                completed: true,
                sessionNumber: 0
            }
        ];

        const analytics = extension.getProductivityAnalytics();

        // Check today's analytics
        assert.equal(analytics.today.totalSessions, 2);
        assert.equal(analytics.today.completedSessions, 2);
        assert.equal(analytics.today.totalFocusTime, 50 * 60 * 1000); // 50 minutes
        assert.equal(analytics.today.completionRate, 100);

        // Check all-time analytics
        assert.equal(analytics.allTime.totalSessions, 3);
        assert.equal(analytics.allTime.completedSessions, 3);
        assert.equal(analytics.allTime.totalFocusTime, 75 * 60 * 1000); // 75 minutes
    });

    await t.test('should handle empty session logs', () => {
        extension.timer.sessionLog = [];

        const analytics = extension.getProductivityAnalytics();

        // All metrics should be zero
        assert.equal(analytics.today.totalSessions, 0);
        assert.equal(analytics.today.completedSessions, 0);
        assert.equal(analytics.today.totalFocusTime, 0);
        assert.equal(analytics.today.completionRate, 0);
    });
});

test('StudyModeExtension - Reset Functionality', async (t) => {
    let extension;
    let mockContext;

    t.beforeEach(() => {
        const mockVSCode = createStudyModeVSCode();
        mockContext = createMockExtensionContext();
        extension = new StudyModeExtension(mockVSCode, mockContext);
        extension.activate();

        // Add some test data
        extension.timer.sessionLog = [
            { type: 'work', startTime: new Date().toISOString(), duration: 1000, completed: true, sessionNumber: 0 }
        ];
        extension.timer.currentSession = 2;
        extension.timer.start();
    });

    t.afterEach(() => {
        if (extension) {
            extension.dispose();
        }
    });

    await t.test('should perform full reset', () => {
        extension.performFullReset();

        // Check that everything is reset
        assert.equal(extension.timer.currentPhase, 'stopped');
        assert.equal(extension.timer.currentSession, 0);
        assert.equal(extension.timer.isRunning, false);
        assert.equal(extension.timer.sessionLog.length, 0);
        assert.equal(extension.timer.startTime, null);
        assert.equal(extension.timer.pausedTime, null);
        assert.equal(extension.timer.remainingTime, 0);
    });

    await t.test('should reset today progress only', () => {
        const now = new Date();
        const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
        const yesterday = new Date(today);
        yesterday.setDate(yesterday.getDate() - 1);

        // Stop the timer first to avoid logging incomplete session
        extension.timer.stop();

        // Clear existing sessions and add yesterday's session
        extension.timer.sessionLog = [{
            type: 'work',
            startTime: yesterday.toISOString(),
            duration: 2000,
            completed: true,
            sessionNumber: 0
        }];

        // Add today's session
        extension.timer.sessionLog.push({
            type: 'work',
            startTime: now.toISOString(),
            duration: 1000,
            completed: true,
            sessionNumber: 1
        });

        // Should have 2 sessions initially
        assert.equal(extension.timer.sessionLog.length, 2);

        extension.resetTodayProgress();

        // Should keep yesterday's session but remove today's
        assert.equal(extension.timer.sessionLog.length, 1);
        assert.equal(extension.timer.sessionLog[0].duration, 2000);
    });

    await t.test('should format duration correctly', () => {
        // Test various duration formats
        assert.equal(extension.formatDuration(0), '0m');
        assert.equal(extension.formatDuration(30 * 1000), '0m'); // Less than 1 minute
        assert.equal(extension.formatDuration(90 * 1000), '2m'); // 1.5 minutes rounds up
        assert.equal(extension.formatDuration(60 * 60 * 1000), '1h 0m'); // 1 hour
        assert.equal(extension.formatDuration(90 * 60 * 1000), '1h 30m'); // 1.5 hours
    });

    await t.test('should show study stats without errors', () => {
        // Add some test sessions
        extension.timer.sessionLog = [
            {
                startTime: new Date().toISOString(),
                duration: 25 * 60 * 1000, // 25 minutes
                type: 'work',
                completed: true,
                sessionNumber: 1
            },
            {
                startTime: new Date().toISOString(),
                duration: 5 * 60 * 1000, // 5 minutes
                type: 'shortBreak',
                completed: true,
                sessionNumber: 1
            }
        ];

        // Mock vscode.window.showInformationMessage to capture the call
        let shownMessage = null;
        const mockVSCode = createStudyModeVSCode();
        mockVSCode.window.showInformationMessage = (message, options, ...buttons) => {
            shownMessage = message;
            return Promise.resolve('Got it!');
        };

        // Replace the vscode instance in the extension
        extension.vscode = mockVSCode;

        // Call showStudyStats
        extension.showStudyStats();

        // Verify that a message was shown and it contains expected content
        assert.ok(shownMessage);
        assert.ok(shownMessage.includes('Study Mode Statistics'));
        assert.ok(shownMessage.includes('Focus Time'));
        assert.ok(shownMessage.includes('Sessions Completed'));
    });
});