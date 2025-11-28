/**
 * Chat Data Manager - Test Suite
 * Test-Driven Development for Chat Persistence
 * Using Node.js built-in test runner
 */

process.env.NODE_ENV = 'test';

const test = require('node:test');
const assert = require('node:assert');
const { ChatDataManager } = require('./chatDataManager');
const { createMockExtensionContext } = require('../../test/utils/globalStateMock');

const createMockContext = (initialState = {}) => createMockExtensionContext({
    initialState,
    extensionPath: '/test/path'
});

test('ChatDataManager - Initialization', async (t) => {
    await t.test('should initialize with default data structure', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const data = await manager.getData();

        assert.equal(data.version, '1.0.0');
        assert(Array.isArray(data.conversations));
        assert.equal(data.conversations.length, 0);
        assert.equal(data.activeConversationId, null);
    });

    await t.test('should use existing data if present', async () => {
        const mockContext = createMockContext();
        const existingData = {
            version: '1.0.0',
            conversations: [
                {
                    id: 'test-1',
                    title: 'Test Chat',
                    model: 'mistral',
                    messages: [],
                    createdAt: new Date().toISOString(),
                    updatedAt: new Date().toISOString()
                }
            ],
            activeConversationId: 'test-1'
        };

        await mockContext.globalState.update('tsi.chat.data', existingData);

        const manager = new ChatDataManager(mockContext);
        const data = await manager.getData();

        assert.equal(data.conversations.length, 1);
        assert.equal(data.conversations[0].id, 'test-1');
        assert.equal(data.activeConversationId, 'test-1');
    });
});

test('ChatDataManager - Conversation CRUD', async (t) => {
    await t.test('should create a new conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation('My Chat', 'llama2');

        assert(conversation.id);
        assert.equal(conversation.title, 'My Chat');
        assert.equal(conversation.model, 'llama2');
        assert(Array.isArray(conversation.messages));
        assert.equal(conversation.messages.length, 0);
        assert(conversation.createdAt);
        assert(conversation.updatedAt);

        const activeId = await manager.getActiveConversationId();
        assert.equal(activeId, conversation.id);
    });

    await t.test('should create conversation with default values', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation();

        assert.equal(conversation.title, 'New Conversation');
        assert.equal(conversation.model, 'mistral');
    });

    await t.test('should get all conversations', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        await manager.createConversation('Chat 1');
        await manager.createConversation('Chat 2');

        const conversations = await manager.getConversations();

        assert.equal(conversations.length, 2);
        assert.equal(conversations[0].title, 'Chat 1');
        assert.equal(conversations[1].title, 'Chat 2');
    });

    await t.test('should get a specific conversation by ID', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const created = await manager.createConversation('Find Me');
        const found = await manager.getConversation(created.id);

        assert(found);
        assert.equal(found.id, created.id);
        assert.equal(found.title, 'Find Me');
    });

    await t.test('should return null for non-existent conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const found = await manager.getConversation('non-existent-id');

        assert.equal(found, undefined);
    });

    await t.test('should rename a conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation('Old Name');
        const renamed = await manager.renameConversation(conversation.id, 'New Name');

        assert(renamed);
        assert.equal(renamed.title, 'New Name');
        assert(renamed.updatedAt >= conversation.updatedAt);
    });

    await t.test('should return null when renaming non-existent conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const result = await manager.renameConversation('non-existent', 'New Name');

        assert.equal(result, null);
    });

    await t.test('should delete a conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation('To Delete');
        const deleted = await manager.deleteConversation(conversation.id);

        assert.equal(deleted, true);

        const conversations = await manager.getConversations();
        assert.equal(conversations.length, 0);
    });

    await t.test('should update activeConversationId when deleting active conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conv1 = await manager.createConversation('Chat 1');
        const conv2 = await manager.createConversation('Chat 2');

        await manager.setActiveConversation(conv1.id);
        await manager.deleteConversation(conv1.id);

        const activeId = await manager.getActiveConversationId();
        assert.equal(activeId, conv2.id);
    });

    await t.test('should set activeConversationId to null when deleting last conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation('Last One');
        await manager.deleteConversation(conversation.id);

        const activeId = await manager.getActiveConversationId();
        assert.equal(activeId, null);
    });
});

test('ChatDataManager - Message Operations', async (t) => {
    await t.test('should append user message to conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation('Test');
        const message = await manager.appendMessage(conversation.id, 'user', 'Hello, world!');

        assert(message);
        assert.equal(message.role, 'user');
        assert.equal(message.content, 'Hello, world!');
        assert(message.timestamp);

        const updated = await manager.getConversation(conversation.id);
        assert.equal(updated.messages.length, 1);
        assert.equal(updated.messages[0].content, 'Hello, world!');
    });

    await t.test('should append assistant message to conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation('Test');
        await manager.appendMessage(conversation.id, 'user', 'Hello');
        const assistantMsg = await manager.appendMessage(conversation.id, 'assistant', 'Hi there!');

        assert.equal(assistantMsg.role, 'assistant');
        assert.equal(assistantMsg.content, 'Hi there!');

        const updated = await manager.getConversation(conversation.id);
        assert.equal(updated.messages.length, 2);
    });

    await t.test('should return null when appending to non-existent conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const result = await manager.appendMessage('non-existent', 'user', 'Hello');

        assert.equal(result, null);
    });

    await t.test('should update conversation updatedAt when appending message', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation('Test');
        const originalUpdatedAt = conversation.updatedAt;

        await new Promise(resolve => setTimeout(resolve, 10));
        await manager.appendMessage(conversation.id, 'user', 'Test message');

        const updated = await manager.getConversation(conversation.id);
        assert.notEqual(updated.updatedAt, originalUpdatedAt);
    });
});

test('ChatDataManager - History Trimming', async (t) => {
    await t.test('should trim conversation history beyond limit', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation('Test');

        for (let i = 0; i < 15; i++) {
            await manager.appendMessage(conversation.id, 'user', `Message ${i}`);
        }

        const trimmed = await manager.trimConversationHistory(conversation.id, 10);

        assert.equal(trimmed.length, 10);
        assert.equal(trimmed[0].content, 'Message 5');
        assert.equal(trimmed[9].content, 'Message 14');
    });

    await t.test('should not trim if messages are within limit', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation('Test');

        for (let i = 0; i < 5; i++) {
            await manager.appendMessage(conversation.id, 'user', `Message ${i}`);
        }

        const trimmed = await manager.trimConversationHistory(conversation.id, 10);

        assert.equal(trimmed.length, 5);
    });

    await t.test('should return empty array for non-existent conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const result = await manager.trimConversationHistory('non-existent', 10);

        assert(Array.isArray(result));
        assert.equal(result.length, 0);
    });
});

test('ChatDataManager - Active Conversation', async (t) => {
    await t.test('should set and get active conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conv1 = await manager.createConversation('Chat 1');
        const conv2 = await manager.createConversation('Chat 2');

        await manager.setActiveConversation(conv1.id);

        const activeId = await manager.getActiveConversationId();
        assert.equal(activeId, conv1.id);

        const activeConv = await manager.getActiveConversation();
        assert.equal(activeConv.id, conv1.id);
        assert.equal(activeConv.title, 'Chat 1');
    });

    await t.test('should return null for no active conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const activeConv = await manager.getActiveConversation();

        assert.equal(activeConv, null);
    });
});

test('ChatDataManager - Model Update', async (t) => {
    await t.test('should update conversation model', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const conversation = await manager.createConversation('Test', 'mistral');
        const updated = await manager.updateConversationModel(conversation.id, 'llama2');

        assert(updated);
        assert.equal(updated.model, 'llama2');
    });

    await t.test('should return null when updating non-existent conversation', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const result = await manager.updateConversationModel('non-existent', 'llama2');

        assert.equal(result, null);
    });
});

test('ChatDataManager - Export/Import', async (t) => {
    await t.test('should export all data', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        await manager.createConversation('Chat 1');
        await manager.createConversation('Chat 2');

        const exported = await manager.exportData();

        assert.equal(exported.version, '1.0.0');
        assert.equal(exported.conversations.length, 2);
        assert(exported.activeConversationId);
    });

    await t.test('should import valid data', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const importData = {
            version: '1.0.0',
            conversations: [
                {
                    id: 'import-1',
                    title: 'Imported Chat',
                    model: 'mistral',
                    messages: [
                        { role: 'user', content: 'Hello', timestamp: new Date().toISOString() }
                    ],
                    createdAt: new Date().toISOString(),
                    updatedAt: new Date().toISOString()
                }
            ]
        };

        await manager.importData(importData);

        const conversations = await manager.getConversations();
        assert.equal(conversations.length, 1);
        assert.equal(conversations[0].title, 'Imported Chat');
        assert.equal(conversations[0].messages.length, 1);
    });

    await t.test('should validate imported conversations', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        const importData = {
            version: '1.0.0',
            conversations: [
                {
                    id: 'valid-1',
                    title: 'Valid Chat',
                    model: 'mistral',
                    messages: [],
                    createdAt: new Date().toISOString(),
                    updatedAt: new Date().toISOString()
                },
                {
                    title: 'Invalid - No ID',
                    messages: []
                },
                {
                    id: 'invalid-2',
                    title: 'Invalid - Bad Messages',
                    messages: [{ role: 'invalid-role', content: 'test' }]
                }
            ]
        };

        await manager.importData(importData);

        const conversations = await manager.getConversations();
        assert.equal(conversations.length, 1);
        assert.equal(conversations[0].title, 'Valid Chat');
    });

    await t.test('should throw error for invalid import data', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        await assert.rejects(
            async () => await manager.importData(null),
            { message: 'Invalid import data format' }
        );

        await assert.rejects(
            async () => await manager.importData('invalid'),
            { message: 'Invalid import data format' }
        );
    });

    await t.test('should merge imported conversations with existing', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        await manager.createConversation('Existing Chat');

        const importData = {
            version: '1.0.0',
            conversations: [
                {
                    id: 'import-1',
                    title: 'Imported Chat',
                    model: 'mistral',
                    messages: [],
                    createdAt: new Date().toISOString(),
                    updatedAt: new Date().toISOString()
                }
            ]
        };

        await manager.importData(importData);

        const conversations = await manager.getConversations();
        assert.equal(conversations.length, 2);
    });
});

test('ChatDataManager - Data Migration', async (t) => {
    await t.test('should migrate data without version', async () => {
        const mockContext = createMockContext();
        const dataWithoutVersion = {
            conversations: [],
            activeConversationId: null
        };

        await mockContext.globalState.update('tsi.chat.data', dataWithoutVersion);

        const manager = new ChatDataManager(mockContext);
        const data = await manager.getData();

        assert.equal(data.version, '1.0.0');
    });

    await t.test('should ensure arrays exist in migrated data', async () => {
        const mockContext = createMockContext();
        const incompleteData = {
            version: '1.0.0'
        };

        await mockContext.globalState.update('tsi.chat.data', incompleteData);

        const manager = new ChatDataManager(mockContext);
        const data = await manager.getData();

        assert(Array.isArray(data.conversations));
        assert.equal(data.conversations.length, 0);
        assert.equal(data.activeConversationId, null);
    });
});

test('ChatDataManager - Clear Data', async (t) => {
    await t.test('should clear all data', async () => {
        const mockContext = createMockContext();
        const manager = new ChatDataManager(mockContext);

        await manager.createConversation('Chat 1');
        await manager.createConversation('Chat 2');

        await manager.clearAllData();

        const data = await manager.getData();
        assert.equal(data.conversations.length, 0);
        assert.equal(data.activeConversationId, null);
    });
});
