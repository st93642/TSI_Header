/**
 * Study Mode Timer - Test Suite
 * Test-Driven Development for Pomodoro Timer Implementation
 * Using Node.js built-in test runner
 */

const test = require('node:test');
const assert = require('node:assert');
const { StudyModeTimer } = require('./timer');

// Mock VS Code API for testing
const mockVSCode = {
    window: {
        showInformationMessage: () => {},
        showWarningMessage: () => {},
        setStatusBarMessage: () => {},
        createStatusBarItem: (alignment, priority) => {
            return new mockVSCode.StatusBarItem();
        }
    },
    StatusBarAlignment: { Right: 1, Left: 2 },
    StatusBarItem: class {
        constructor() {
            this.text = '';
            this.tooltip = '';
            this.command = '';
            this.show = () => {};
            this.hide = () => {};
            this.dispose = () => {};
        }
    }
};

test('StudyModeTimer - Initialization', async (t) => {
    let timer;

    t.afterEach(() => {
        if (timer) {
            timer.dispose();
        }
    });

    await t.test('should initialize with default configuration', () => {
        const mockContext = {
            subscriptions: [],
            extensionPath: '/test/path'
        };
        timer = new StudyModeTimer(mockVSCode, mockContext);

        assert.equal(timer.workDuration, 25 * 60 * 1000); // 25 minutes in ms
        assert.equal(timer.shortBreakDuration, 5 * 60 * 1000); // 5 minutes in ms
        assert.equal(timer.longBreakDuration, 15 * 60 * 1000); // 15 minutes in ms
        assert.equal(timer.sessionsBeforeLongBreak, 4);
        assert.equal(timer.currentPhase, 'stopped');
        assert.equal(timer.currentSession, 0);
        assert.equal(timer.isRunning, false);
    });

    await t.test('should create status bar item', () => {
        const mockContext = {
            subscriptions: [],
            extensionPath: '/test/path'
        };
        timer = new StudyModeTimer(mockVSCode, mockContext);

        assert(timer.statusBarItem);
        assert.equal(typeof timer.statusBarItem.text, 'string');
    });
});

test('StudyModeTimer - Timer Controls', async (t) => {
    let timer;
    let mockContext;

    t.beforeEach(() => {
        mockContext = {
            subscriptions: [],
            extensionPath: '/test/path'
        };
        timer = new StudyModeTimer(mockVSCode, mockContext);
    });

    t.afterEach(() => {
        if (timer) {
            timer.dispose();
        }
    });

    await t.test('should start timer and transition to work phase', () => {
        timer.start();
        assert.equal(timer.currentPhase, 'work');
        assert.equal(timer.isRunning, true);
        assert(timer.startTime);
    });

    await t.test('should pause running timer', () => {
        timer.start();
        timer.pause();
        assert.equal(timer.isRunning, false);
        assert(timer.pausedTime);
    });

    await t.test('should resume paused timer', () => {
        timer.start();
        timer.pause();
        timer.resume();
        assert.equal(timer.isRunning, true);
        assert(!timer.pausedTime);
    });

    await t.test('should stop timer and reset state', () => {
        timer.start();
        timer.stop();
        assert.equal(timer.currentPhase, 'stopped');
        assert.equal(timer.isRunning, false);
        assert.equal(timer.currentSession, 0);
    });
});

test('StudyModeTimer - Time Calculations', async (t) => {
    let timer;
    let mockContext;

    t.beforeEach(() => {
        mockContext = {
            subscriptions: [],
            extensionPath: '/test/path'
        };
        timer = new StudyModeTimer(mockVSCode, mockContext);
    });

    t.afterEach(() => {
        if (timer) {
            timer.dispose();
        }
    });

    await t.test('should format time as MM:SS', () => {
        assert.equal(timer.formatTime(65000), '01:05');
        assert.equal(timer.formatTime(59000), '00:59');
        assert.equal(timer.formatTime(0), '00:00');
    });
});

test('StudyModeTimer - Status Bar Updates', async (t) => {
    let timer;
    let mockContext;

    t.beforeEach(() => {
        mockContext = {
            subscriptions: [],
            extensionPath: '/test/path'
        };
        timer = new StudyModeTimer(mockVSCode, mockContext);
    });

    t.afterEach(() => {
        if (timer) {
            timer.dispose();
        }
    });

    await t.test('should update status bar with work phase', () => {
        timer.start();
        timer.updateStatusBar();

        assert(timer.statusBarItem.text.includes('🍅'));
        assert(timer.statusBarItem.tooltip.includes('Work Session'));
    });

    await t.test('should update status bar with break phase', () => {
        timer.currentPhase = 'shortBreak';
        timer.updateStatusBar();

        assert(timer.statusBarItem.text.includes('☕'));
        assert(timer.statusBarItem.tooltip.includes('Short Break'));
    });

    await t.test('should update status bar when paused', () => {
        timer.start();
        timer.pause();
        timer.updateStatusBar();

        assert(timer.statusBarItem.text.includes('⏸️'));
    });
});

test('StudyModeTimer - Configuration', async (t) => {
    let timer;

    t.afterEach(() => {
        if (timer) {
            timer.dispose();
        }
    });

    await t.test('should accept custom configuration', () => {
        const mockContext = {
            subscriptions: [],
            extensionPath: '/test/path'
        };
        const customConfig = {
            workDuration: 30,
            shortBreakDuration: 10,
            longBreakDuration: 20,
            sessionsBeforeLongBreak: 3
        };

        timer = new StudyModeTimer(mockVSCode, mockContext, customConfig);

        assert.equal(timer.workDuration, 30 * 60 * 1000);
        assert.equal(timer.shortBreakDuration, 10 * 60 * 1000);
        assert.equal(timer.longBreakDuration, 20 * 60 * 1000);
        assert.equal(timer.sessionsBeforeLongBreak, 3);
    });
});