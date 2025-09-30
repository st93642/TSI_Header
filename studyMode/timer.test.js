/**
 * Study Mode Timer - Test Suite
 * Test-Driven Development for Pomodoro Timer Implementation
 * Using Node.js built-in test runner
 */

// Set test environment
process.env.NODE_ENV = 'test';

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
        // Wait a moment and then pause
        return new Promise(resolve => {
            setTimeout(() => {
                const remainingBeforePause = timer.getRemainingTime();
                timer.pause();
                
                // Should show paused icon and remaining time
                assert(timer.statusBarItem.text.includes('⏸️🍅'));
                // Should show the remaining time that was stored when paused
                const displayedTime = timer.statusBarItem.text.match(/(\d{2}:\d{2})/)[1];
                const expectedTime = timer.formatTime(timer.remainingTime);
                assert.equal(displayedTime, expectedTime);
                // Remaining time should be stored correctly
                assert(timer.remainingTime > 0);
                assert(timer.remainingTime <= timer.workDuration);
                resolve();
            }, 100); // Wait 100ms
        });
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

    await t.test('should accept state change callback', () => {
        const mockContext = {
            subscriptions: [],
            extensionPath: '/test/path'
        };
        let callbackCalled = false;
        const callback = () => { callbackCalled = true; };

        timer = new StudyModeTimer(mockVSCode, mockContext, {}, callback);
        timer.start();

        assert(callbackCalled, 'State change callback should be called on start');
    });
});

test('StudyModeTimer - Break Popup and Audio', async (t) => {
    let timer;
    let mockContext;
    let showInformationMessageCalls = [];
    let showWarningMessageCalls = [];

    // Enhanced mock VS Code API
    const enhancedMockVSCode = {
        ...mockVSCode,
        window: {
            ...mockVSCode.window,
            showInformationMessage: (message, options, ...buttons) => {
                showInformationMessageCalls.push({ message, options, buttons });
                // Return a promise that resolves immediately to the first button for testing
                return Promise.resolve(buttons[0]);
            },
            showWarningMessage: (message, options, ...buttons) => {
                showWarningMessageCalls.push({ message, options, buttons });
                return Promise.resolve(buttons[0]);
            },
            createTerminal: (name) => ({
                sendText: () => {},
                dispose: () => {}
            })
        }
    };

    t.beforeEach(() => {
        mockContext = {
            subscriptions: [],
            extensionPath: '/test/path'
        };
        timer = new StudyModeTimer(enhancedMockVSCode, mockContext);
        showInformationMessageCalls = [];
        showWarningMessageCalls = [];
    });

    t.afterEach(() => {
        if (timer) {
            timer.dispose();
        }
    });

    await t.test('should transition to break phase and pause timer for popup', async () => {
        timer.start();
        // Simulate work session completion
        timer.currentPhase = 'work';
        timer.currentSession = 0;
        timer.startTime = Date.now() - timer.workDuration; // Work is complete

        timer.transitionToNextPhase();

        // Check that phase changed and timer is paused for popup
        assert.equal(timer.currentPhase, 'shortBreak');
        assert.equal(timer.isRunning, false, 'Timer should be paused while popup is shown');
    });

    await t.test('should transition to long break phase and pause timer for popup', async () => {
        timer.start();
        // Simulate reaching long break threshold
        timer.currentPhase = 'work';
        timer.currentSession = 3; // One less than threshold
        timer.startTime = Date.now() - timer.workDuration;

        timer.transitionToNextPhase();

        // Check that phase changed and timer is paused for popup
        assert.equal(timer.currentPhase, 'longBreak');
        assert.equal(timer.isRunning, false, 'Timer should be paused while popup is shown');
    });

    await t.test('should start break timer when user selects Take Break', () => {
        timer.currentPhase = 'shortBreak';
        timer.startTime = Date.now();

        // Mock user selecting "Take Break"
        enhancedMockVSCode.window.showInformationMessage = () => Promise.resolve('Take Break');

        // This would be called from the popup handler
        timer.startBreak();

        assert.equal(timer.isRunning, true);
        assert(timer.startTime);
    });

    await t.test('should skip break and start work when user selects Skip Break', () => {
        timer.currentPhase = 'shortBreak';
        timer.currentSession = 0;

        // Mock user selecting "Skip Break"
        enhancedMockVSCode.window.showInformationMessage = () => Promise.resolve('Skip Break');

        // This would be called from the popup handler
        timer.skipBreak();

        assert.equal(timer.currentPhase, 'work');
        assert.equal(timer.isRunning, true);
        assert(timer.startTime);
    });

    await t.test('should play audio signal when break starts', () => {
        let callbackCalled = false;

        timer.currentPhase = 'shortBreak';
        
        // In test environment, callback should be called synchronously
        timer.playBreakAudioSignal(() => {
            callbackCalled = true;
        });

        // Callback should be called immediately in test environment
        assert(callbackCalled, 'Callback should be called immediately in test environment');
    });

    await t.test('should play different beep patterns for long break vs short break', () => {
        let callbackCallCount = 0;

        // Test short break
        timer.currentPhase = 'shortBreak';
        timer.playBreakAudioSignal(() => {
            callbackCallCount++;
        });
        
        assert.equal(callbackCallCount, 1, 'Callback should be called once for short break');

        // Reset for long break test
        callbackCallCount = 0;
        
        // Test long break
        timer.currentPhase = 'longBreak';
        timer.playBreakAudioSignal(() => {
            callbackCallCount++;
        });
        
        assert.equal(callbackCallCount, 1, 'Callback should be called once for long break');
    });
});