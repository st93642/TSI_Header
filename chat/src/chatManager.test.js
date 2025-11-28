/**
 * Chat Manager - Test Suite
 * Tests for standardized lifecycle pattern
 * Using Node.js built-in test runner
 */

process.env.NODE_ENV = 'test';

const test = require('node:test');
const assert = require('node:assert');
const { createVSCodeMock } = require('../../test/utils/vscodeMock');
const { createMockExtensionContext } = require('../../test/utils/globalStateMock');

// Mock vscode before requiring ChatManager
global.vscode = createVSCodeMock({
    configuration: {
        tsiheader: {
            chatTimeout: 30000,
            ollamaUrl: 'http://localhost:11434'
        }
    }
});

const { ChatManager } = require('./chatManager');

const createMockContext = () => createMockExtensionContext({
    extensionUri: { fsPath: '/mock/path' }
});

test('ChatManager - Lifecycle Pattern', async (t) => {
    await t.test('should implement BaseManager lifecycle methods', async () => {

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

        assert.equal(typeof manager.registerViews, 'function');
        assert.equal(typeof manager.registerCommands, 'function');
        assert.equal(typeof manager.setupListeners, 'function');
        assert.equal(typeof manager.dispose, 'function');
    });

    await t.test('should initialize views phase correctly', async () => {

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

        manager.registerViews(mockContext);

        assert(manager.webviewProvider !== null, 'Webview provider should be initialized');
        assert.equal(mockContext.subscriptions.length, 1, 'Should add webview provider to subscriptions');
        
        const status = manager.getInitializationStatus();
        assert.equal(status.views, true);
        assert.equal(status.commands, false);
        assert.equal(status.listeners, false);
    });

    await t.test('should initialize commands phase correctly', async () => {

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

        manager.registerViews(mockContext);
        manager.registerCommands(mockContext);

        assert(mockContext.subscriptions.length > 1, 'Should add commands to subscriptions');
        
        const status = manager.getInitializationStatus();
        assert.equal(status.views, true);
        assert.equal(status.commands, true);
        assert.equal(status.listeners, false);
    });

    await t.test('should initialize listeners phase correctly', async () => {

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

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
        const manager = new ChatManager(mockContext);

        manager.registerViews(mockContext);
        const firstCount = mockContext.subscriptions.length;

        manager.registerViews(mockContext);
        const secondCount = mockContext.subscriptions.length;

        assert.equal(firstCount, secondCount, 'Should not register views twice');
    });

    await t.test('should prevent duplicate command registration', async () => {

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

        manager.registerViews(mockContext);
        manager.registerCommands(mockContext);
        const firstCount = mockContext.subscriptions.length;

        manager.registerCommands(mockContext);
        const secondCount = mockContext.subscriptions.length;

        assert.equal(firstCount, secondCount, 'Should not register commands twice');
    });

    await t.test('should prevent duplicate listener setup', async () => {

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

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
        const manager = new ChatManager(mockContext);

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
        const brokenVSCode = createVSCodeMock({
            window: {
                registerWebviewViewProvider: () => {
                    throw new Error('Registration failed');
                }
            }
        });
        global.vscode = brokenVSCode;

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

        assert.throws(() => {
            manager.registerViews(mockContext);
        }, /Registration failed/);
    });
});

test('ChatManager - Integration', async (t) => {
    await t.test('should have all required chat commands', async () => {
        // Reset vscode mock for this test
        global.vscode = createVSCodeMock({
            configuration: {
                tsiheader: {
                    chatTimeout: 30000,
                    ollamaUrl: 'http://localhost:11434'
                }
            }
        });

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

        manager.registerViews(mockContext);
        manager.registerCommands(mockContext);

        const registeredCommands = global.vscode._getRegisteredCommands();

        const expectedCommands = [
            'tsiheader.openChat',
            'tsiheader.newChatConversation',
            'tsiheader.clearChatHistory'
        ];

        expectedCommands.forEach(cmd => {
            assert(registeredCommands.has(cmd), `Command ${cmd} should be registered`);
        });
    });

    await t.test('should properly chain lifecycle phases', async () => {
        // Reset vscode mock for this test
        global.vscode = createVSCodeMock({
            configuration: {
                tsiheader: {
                    chatTimeout: 30000,
                    ollamaUrl: 'http://localhost:11434'
                }
            }
        });

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

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

    await t.test('should have chatService and dataManager initialized', async () => {

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

        assert(manager.chatService !== null, 'Chat service should be initialized');
        assert(manager.dataManager !== null, 'Data manager should be initialized');
        assert(manager.webviewProvider !== null, 'Webview provider should be initialized');
    });

    await t.test('configuration listener should be tracked as disposable', async () => {
        // Reset vscode mock for this test
        global.vscode = createVSCodeMock({
            configuration: {
                tsiheader: {
                    chatTimeout: 30000,
                    ollamaUrl: 'http://localhost:11434'
                }
            }
        });

        let configListenerDisposed = false;
        global.vscode.workspace.onDidChangeConfiguration = () => ({
            dispose: () => { configListenerDisposed = true; }
        });

        const mockContext = createMockContext();
        const manager = new ChatManager(mockContext);

        manager.registerViews(mockContext);
        manager.registerCommands(mockContext);
        manager.setupListeners(mockContext);

        manager.dispose();

        assert.equal(configListenerDisposed, true, 'Config listener should be disposed');
    });
});
