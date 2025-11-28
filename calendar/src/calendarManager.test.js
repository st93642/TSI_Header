/**
 * Calendar Manager - Test Suite
 * Tests for standardized lifecycle pattern
 * Using Node.js built-in test runner
 */

process.env.NODE_ENV = 'test';

const test = require('node:test');
const assert = require('node:assert');
const { createVSCodeMock } = require('../../test/utils/vscodeMock');
const { createMockExtensionContext } = require('../../test/utils/globalStateMock');

// Mock vscode before requiring CalendarManager
global.vscode = createVSCodeMock({
    TreeItem: class TreeItem {},
    TreeItemCollapsibleState: {
        None: 0,
        Collapsed: 1,
        Expanded: 2
    },
    EventEmitter: class EventEmitter {
        constructor() {
            this.event = () => {};
        }
        fire() {}
    },
    ThemeIcon: class ThemeIcon {
        constructor(id) {
            this.id = id;
        }
    },
    Uri: {
        file: (path) => ({ fsPath: path })
    }
});

const { CalendarManager } = require('./calendarManager');

const createMockContext = () => createMockExtensionContext({
    extensionUri: { fsPath: '/mock/path' }
});

test('CalendarManager - Lifecycle Pattern', async (t) => {
    await t.test('should implement BaseManager lifecycle methods', async () => {
        const mockContext = createMockContext();
        const manager = new CalendarManager(mockContext);

        assert.equal(typeof manager.registerViews, 'function');
        assert.equal(typeof manager.registerCommands, 'function');
        assert.equal(typeof manager.setupListeners, 'function');
        assert.equal(typeof manager.dispose, 'function');
    });

    await t.test('should initialize views phase correctly', async () => {

        const mockContext = createMockContext();
        const manager = new CalendarManager(mockContext);

        manager.registerViews(mockContext);

        assert(manager.treeDataProvider !== null, 'Tree provider should be initialized');
        assert.equal(mockContext.subscriptions.length, 1, 'Should add tree provider to subscriptions');
        
        const status = manager.getInitializationStatus();
        assert.equal(status.views, true);
        assert.equal(status.commands, false);
        assert.equal(status.listeners, false);
    });

    await t.test('should initialize commands phase correctly', async () => {

        const mockContext = createMockContext();
        const manager = new CalendarManager(mockContext);

        manager.registerViews(mockContext);
        manager.registerCommands(mockContext);

        assert(manager.webviewProvider !== null, 'Webview provider should be initialized');
        assert(mockContext.subscriptions.length > 1, 'Should add commands to subscriptions');
        
        const status = manager.getInitializationStatus();
        assert.equal(status.views, true);
        assert.equal(status.commands, true);
        assert.equal(status.listeners, false);
    });

    await t.test('should initialize listeners phase correctly', async () => {

        const mockContext = createMockContext();
        const manager = new CalendarManager(mockContext);

        manager.registerViews(mockContext);
        manager.registerCommands(mockContext);
        manager.setupListeners(mockContext);
        
        const status = manager.getInitializationStatus();
        assert.equal(status.views, true);
        assert.equal(status.commands, true);
        assert.equal(status.listeners, true);
        assert.equal(manager.isFullyInitialized(), true);
    });

    await t.test('should prevent duplicate view registration', async () => {

        const mockContext = createMockContext();
        const manager = new CalendarManager(mockContext);

        manager.registerViews(mockContext);
        const firstCount = mockContext.subscriptions.length;

        manager.registerViews(mockContext);
        const secondCount = mockContext.subscriptions.length;

        assert.equal(firstCount, secondCount, 'Should not register views twice');
    });

    await t.test('should prevent duplicate command registration', async () => {

        const mockContext = createMockContext();
        const manager = new CalendarManager(mockContext);

        manager.registerViews(mockContext);
        manager.registerCommands(mockContext);
        const firstCount = mockContext.subscriptions.length;

        manager.registerCommands(mockContext);
        const secondCount = mockContext.subscriptions.length;

        assert.equal(firstCount, secondCount, 'Should not register commands twice');
    });

    await t.test('should prevent duplicate listener setup', async () => {

        const mockContext = createMockContext();
        const manager = new CalendarManager(mockContext);

        manager.registerViews(mockContext);
        manager.registerCommands(mockContext);
        manager.setupListeners(mockContext);
        const firstCount = mockContext.subscriptions.length;

        manager.setupListeners(mockContext);
        const secondCount = mockContext.subscriptions.length;

        assert.equal(firstCount, secondCount, 'Should not setup listeners twice');
    });

    await t.test('should dispose all resources', async () => {

        const mockContext = createMockContext();
        const manager = new CalendarManager(mockContext);

        manager.registerViews(mockContext);
        manager.registerCommands(mockContext);
        manager.setupListeners(mockContext);

        assert.equal(manager.isFullyInitialized(), true);

        manager.dispose();

        const status = manager.getInitializationStatus();
        assert.equal(status.views, false);
        assert.equal(status.commands, false);
        assert.equal(status.listeners, false);
        assert.equal(manager.isFullyInitialized(), false);
    });

    await t.test('should handle errors in view registration gracefully', async () => {
        // Save the original
        const originalVSCode = global.vscode;
        
        // Override with broken version
        global.vscode = {
            ...originalVSCode,
            window: {
                ...originalVSCode.window,
                registerTreeDataProvider: () => {
                    throw new Error('Registration failed');
                }
            }
        };

        const mockContext = createMockContext();
        
        // Need to create a new CalendarManager instance after overriding vscode
        // But since the module is already loaded, the vscode reference is cached
        // So this test needs to check that the error is caught and rethrown
        const manager = new CalendarManager(mockContext);
        
        try {
            manager.registerViews(mockContext);
            assert.fail('Should have thrown an error');
        } catch (error) {
            assert.match(error.message, /Registration failed/);
        } finally {
            // Restore original
            global.vscode = originalVSCode;
        }
    });
});

test('CalendarManager - Integration', async (t) => {
    await t.test('should have all required calendar commands', async () => {

        const mockContext = createMockContext();
        const manager = new CalendarManager(mockContext);

        const registeredCommands = [];
        const originalRegisterCommand = global.vscode.commands.registerCommand;
        global.vscode.commands.registerCommand = (name) => {
            registeredCommands.push(name);
            return { dispose: () => {} };
        };

        try {
            manager.registerViews(mockContext);
            manager.registerCommands(mockContext);
        } finally {
            global.vscode.commands.registerCommand = originalRegisterCommand;
        }

        const expectedCommands = [
            'tsiheader.showCalendar',
            'tsiheader.addCalendarDeadline',
            'tsiheader.addCalendarEvent',
            'tsiheader.addCalendarSchedule',
            'tsiheader.exportCalendar',
            'tsiheader.importCalendar',
            'tsiheader.importCalendarFromUrl'
        ];

        expectedCommands.forEach(cmd => {
            assert(registeredCommands.includes(cmd), `Command ${cmd} should be registered`);
        });
    });

    await t.test('should properly chain lifecycle phases', async () => {

        const mockContext = createMockContext();
        const manager = new CalendarManager(mockContext);

        const phases = [];
        
        manager.registerViews(mockContext);
        phases.push(manager.getInitializationStatus());
        
        manager.registerCommands(mockContext);
        phases.push(manager.getInitializationStatus());
        
        manager.setupListeners(mockContext);
        phases.push(manager.getInitializationStatus());

        assert.deepEqual(phases[0], { views: true, commands: false, listeners: false });
        assert.deepEqual(phases[1], { views: true, commands: true, listeners: false });
        assert.deepEqual(phases[2], { views: true, commands: true, listeners: true });
    });
});
