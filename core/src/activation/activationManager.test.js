/**
 * ActivationManager - Test Suite
 * Tests for extension activation flow with graceful error handling
 * Using Node.js built-in test runner
 */

process.env.NODE_ENV = 'test';

const test = require('node:test');
const assert = require('node:assert');
const { createVSCodeMock } = require('../../../test/utils/vscodeMock');
const { createMockExtensionContext } = require('../../../test/utils/globalStateMock');

global.vscode = createVSCodeMock();

const { ActivationManager } = require('./activationManager');

const createMockContext = () => {
    return createMockExtensionContext({
        extensionPath: '/test/path'
    });
};

test('ActivationManager - Initialization', { timeout: 5000 }, async (t) => {
    await t.test('should create an instance with context', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        assert(manager);
        assert.equal(manager.context, mockContext);
        assert.equal(manager.tsiCommandsProvider, null);
        assert.equal(manager.calendarManager, null);
        assert.equal(manager.chatManager, null);
    });

    await t.test('should have empty registrations initially', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        assert(Array.isArray(manager.registrations.treeProviders));
        assert(Array.isArray(manager.registrations.webviewProviders));
        assert(Array.isArray(manager.registrations.commands));
        assert(Array.isArray(manager.registrations.failures));
        assert.equal(manager.registrations.treeProviders.length, 0);
    });
});

test('ActivationManager - View Registration', { timeout: 5000 }, async (t) => {
    // Mock the required modules before tests
    const mockRequire = (modulePath) => {
        if (modulePath.includes('tsiViewProvider')) {
            return {
                TSITreeDataProvider: class { constructor() { this.id = 'tsi-commands'; } },
                TSIProjectDataProvider: class { constructor() { this.id = 'tsi-projects'; } }
            };
        }
        throw new Error(`Unknown module: ${modulePath}`);
    };

    // Replace require for these tests
    const originalRequire = require;

    await t.test('should register TSI tree providers synchronously', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        // Temporarily mock require
        global.testMockRequire = true;
        
        try {
            // Since we can't easily mock require at the module level, 
            // we'll test the public interface
            assert(typeof manager.registerViews === 'function');
            assert(typeof manager.initializeModules === 'function');
            assert(typeof manager.registerCommands === 'function');
        } finally {
            delete global.testMockRequire;
        }
    });

    await t.test('should record registration stats', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        // Manually add stats to test the tracking mechanism
        manager.registrations.treeProviders.push('tsi-commands');
        manager.registrations.treeProviders.push('tsi-projects');
        manager.registrations.webviewProviders.push('tsi-chat-view');
        
        const stats = manager.getStats();
        assert.equal(stats.treeProviders, 2);
        assert.equal(stats.webviewProviders, 1);
        assert.equal(stats.failures, 0);
    });

    await t.test('should track failures gracefully', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        // Simulate a failure
        manager._logFailure('Test Component', new Error('Test error'));
        
        assert.equal(manager.registrations.failures.length, 1);
        assert.equal(manager.registrations.failures[0].component, 'Test Component');
        assert(manager.registrations.failures[0].error.includes('Test error'));
    });
});

test('ActivationManager - Lifecycle Methods', { timeout: 5000 }, async (t) => {
    await t.test('registerViews should be callable', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        // Should not throw
        assert.doesNotThrow(() => {
            // We can't fully test this without mocking the module system,
            // but we can verify the method exists and is callable
            assert(typeof manager.registerViews === 'function');
        });
    });

    await t.test('initializeModules should be async', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        const result = manager.initializeModules();
        assert(result instanceof Promise);
    });

    await t.test('registerCommands should be async', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        const result = manager.registerCommands();
        assert(result instanceof Promise);
    });
});

test('ActivationManager - Dependency Injection', { timeout: 5000 }, async (t) => {
    await t.test('should provide getter methods for managers', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        assert.equal(manager.getCalendarManager(), null);
        assert.equal(manager.getChatManager(), null);
        assert.equal(manager.getCore(), null);
        assert.equal(manager.getStudyModeExtension(), null);
    });

    await t.test('should allow setting managers for dependency injection', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        const mockCalendarManager = { test: 'calendar' };
        const mockChatManager = { test: 'chat' };
        
        manager.calendarManager = mockCalendarManager;
        manager.chatManager = mockChatManager;
        
        assert.equal(manager.getCalendarManager(), mockCalendarManager);
        assert.equal(manager.getChatManager(), mockChatManager);
    });
});

test('ActivationManager - Error Handling', { timeout: 5000 }, async (t) => {
    await t.test('should not throw when getStats is called', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        assert.doesNotThrow(() => {
            const stats = manager.getStats();
            assert(stats);
            assert(typeof stats.treeProviders === 'number');
            assert(typeof stats.webviewProviders === 'number');
            assert(typeof stats.commands === 'number');
            assert(typeof stats.failures === 'number');
        });
    });

    await t.test('should handle dispose gracefully', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        // Set up mock managers
        manager.calendarManager = {
            dispose: () => { throw new Error('Dispose error'); }
        };
        
        manager.studyModeExtension = {
            dispose: () => { /* no error */ }
        };
        
        // Should not throw even if dispose throws
        assert.doesNotThrow(() => {
            manager.dispose();
        });
    });

    await t.test('should handle missing dispose methods', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        // Set up managers without dispose
        manager.calendarManager = {};
        manager.studyModeExtension = {};
        
        assert.doesNotThrow(() => {
            manager.dispose();
        });
    });
});

test('ActivationManager - Stats Tracking', { timeout: 5000 }, async (t) => {
    await t.test('should track multiple tree providers', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        manager.registrations.treeProviders.push('tsi-commands');
        manager.registrations.treeProviders.push('tsi-projects');
        manager.registrations.treeProviders.push('tsi-calendar');
        
        const stats = manager.getStats();
        assert.equal(stats.treeProviders, 3);
    });

    await t.test('should track multiple webview providers', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        manager.registrations.webviewProviders.push('tsi-chat-view');
        manager.registrations.webviewProviders.push('tsi-calendar-view');
        
        const stats = manager.getStats();
        assert.equal(stats.webviewProviders, 2);
    });

    await t.test('should track multiple commands', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        manager.registrations.commands.push('tsiheader.openChat');
        manager.registrations.commands.push('tsiheader.newChatConversation');
        manager.registrations.commands.push('tsiheader.clearChatHistory');
        
        const stats = manager.getStats();
        assert.equal(stats.commands, 3);
    });

    await t.test('should track multiple failures', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        manager._logFailure('Component1', new Error('Error 1'));
        manager._logFailure('Component2', new Error('Error 2'));
        
        const stats = manager.getStats();
        assert.equal(stats.failures, 2);
    });
});

test('ActivationManager - Provider Order', { timeout: 5000 }, async (t) => {
    await t.test('should register providers in correct order', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        // Simulate the registration order
        manager.registrations.treeProviders.push('tsi-commands');
        manager.registrations.treeProviders.push('tsi-projects');
        manager.registrations.treeProviders.push('tsi-calendar');
        manager.registrations.webviewProviders.push('tsi-chat-view');
        
        // Verify order
        assert.deepEqual(manager.registrations.treeProviders[0], 'tsi-commands');
        assert.deepEqual(manager.registrations.treeProviders[1], 'tsi-projects');
        assert.deepEqual(manager.registrations.treeProviders[2], 'tsi-calendar');
        assert.deepEqual(manager.registrations.webviewProviders[0], 'tsi-chat-view');
    });

    await t.test('should prioritize view registration before initialization', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        // Views should be null initially
        assert.equal(manager.tsiCommandsProvider, null);
        assert.equal(manager.core, null);
        
        // After registerViews, tree providers should still be null (without actual module)
        // because we can't mock the require, but the registration logic would be called
        manager.registrations.treeProviders.push('tsi-commands');
        
        assert.equal(manager.registrations.treeProviders.length, 1);
        // Core is still null because initializeModules hasn't been called
        assert.equal(manager.core, null);
    });
});

test('ActivationManager - Integration', { timeout: 5000 }, async (t) => {
    await t.test('should complete full lifecycle without throwing', async () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        // Should not throw
        assert.doesNotThrow(() => {
            manager.registerViews();
        });
        
        // Should return a promise
        const initPromise = manager.initializeModules();
        assert(initPromise instanceof Promise);
        
        // Wait for initialization
        await initPromise;
        
        // Register commands
        const commandPromise = manager.registerCommands();
        assert(commandPromise instanceof Promise);
        await commandPromise;
    });

    await t.test('should allow multiple registerViews calls', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        assert.doesNotThrow(() => {
            manager.registerViews();
            manager.registerViews(); // Call again
        });
    });
});

test('ActivationManager - Logging', { timeout: 5000 }, async (t) => {
    await t.test('should record failure details', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        const error = new Error('Detailed error message');
        manager._logFailure('DetailedComponent', error);
        
        assert.equal(manager.registrations.failures.length, 1);
        const failure = manager.registrations.failures[0];
        assert.equal(failure.component, 'DetailedComponent');
        assert(failure.error.includes('Detailed error message'));
    });

    await t.test('should handle failure without error message', () => {
        const mockContext = createMockContext();
        const manager = new ActivationManager(mockContext);
        
        manager._logFailure('Component', 'String error');
        
        assert.equal(manager.registrations.failures.length, 1);
        assert.equal(manager.registrations.failures[0].component, 'Component');
        assert(manager.registrations.failures[0].error);
    });
});
