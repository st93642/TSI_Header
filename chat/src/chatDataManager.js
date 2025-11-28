/**
 * Chat Data Manager
 * Handles persistence of chat conversations using VS Code global state
 */

const { GlobalStateStore } = require('../../core/src/storage/globalStateStore');

class ChatDataManager {
    constructor(context) {
        this.context = context;
        this.STORAGE_KEY = 'tsi.chat.data';
        this.store = new GlobalStateStore(context, {
            namespace: '',
            version: '1.0.0',
            defaults: {
                [this.STORAGE_KEY]: {
                    conversations: [],
                    activeConversationId: null,
                    version: '1.0.0'
                }
            },
            migrations: {}
        });
    }

    /**
     * Get all chat data
     */
    async getData() {
        const data = await this.store.getState(this.STORAGE_KEY, {
            conversations: [],
            activeConversationId: null,
            version: '1.0.0'
        });

        return this.migrateData(data);
    }

    /**
     * Save chat data
     */
    async saveData(data) {
        await this.store.updateState(this.STORAGE_KEY, data);
    }

    /**
     * Get all conversations
     */
    async getConversations() {
        const data = await this.getData();
        return data.conversations || [];
    }

    /**
     * Get a specific conversation by ID
     */
    async getConversation(conversationId) {
        const data = await this.getData();
        return data.conversations.find(c => c.id === conversationId);
    }

    /**
     * Get the active conversation
     */
    async getActiveConversation() {
        const data = await this.getData();
        if (!data.activeConversationId) {
            return null;
        }
        return data.conversations.find(c => c.id === data.activeConversationId);
    }

    /**
     * Get the active conversation ID
     */
    async getActiveConversationId() {
        const data = await this.getData();
        return data.activeConversationId;
    }

    /**
     * Set the active conversation
     */
    async setActiveConversation(conversationId) {
        const data = await this.getData();
        data.activeConversationId = conversationId;
        await this.saveData(data);
    }

    /**
     * Create a new conversation
     */
    async createConversation(title = 'New Conversation', model = 'mistral') {
        const data = await this.getData();
        data.conversations = data.conversations || [];

        const conversation = {
            id: `chat-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
            title,
            model,
            messages: [],
            createdAt: new Date().toISOString(),
            updatedAt: new Date().toISOString()
        };

        data.conversations.push(conversation);
        data.activeConversationId = conversation.id;
        await this.saveData(data);

        return conversation;
    }

    /**
     * Rename a conversation
     */
    async renameConversation(conversationId, newTitle) {
        const data = await this.getData();
        const index = data.conversations.findIndex(c => c.id === conversationId);

        if (index !== -1) {
            data.conversations[index].title = newTitle;
            data.conversations[index].updatedAt = new Date().toISOString();
            await this.saveData(data);
            return data.conversations[index];
        }

        return null;
    }

    /**
     * Delete a conversation
     */
    async deleteConversation(conversationId) {
        const data = await this.getData();
        const initialLength = data.conversations.length;
        data.conversations = data.conversations.filter(c => c.id !== conversationId);

        if (data.activeConversationId === conversationId) {
            data.activeConversationId = data.conversations.length > 0 ? data.conversations[0].id : null;
        }

        if (data.conversations.length !== initialLength) {
            await this.saveData(data);
            return true;
        }

        return false;
    }

    /**
     * Append a message to a conversation
     */
    async appendMessage(conversationId, role, content) {
        const data = await this.getData();
        const index = data.conversations.findIndex(c => c.id === conversationId);

        if (index !== -1) {
            const message = {
                role,
                content,
                timestamp: new Date().toISOString()
            };

            data.conversations[index].messages.push(message);
            data.conversations[index].updatedAt = new Date().toISOString();
            await this.saveData(data);

            return message;
        }

        return null;
    }

    /**
     * Trim conversation history beyond a specified limit
     */
    async trimConversationHistory(conversationId, limit) {
        const data = await this.getData();
        const index = data.conversations.findIndex(c => c.id === conversationId);

        if (index !== -1 && data.conversations[index].messages.length > limit) {
            const messages = data.conversations[index].messages;
            const trimmedMessages = messages.slice(-limit);
            data.conversations[index].messages = trimmedMessages;
            data.conversations[index].updatedAt = new Date().toISOString();
            await this.saveData(data);

            return trimmedMessages;
        }

        return data.conversations[index]?.messages || [];
    }

    /**
     * Update conversation model
     */
    async updateConversationModel(conversationId, model) {
        const data = await this.getData();
        const index = data.conversations.findIndex(c => c.id === conversationId);

        if (index !== -1) {
            data.conversations[index].model = model;
            data.conversations[index].updatedAt = new Date().toISOString();
            await this.saveData(data);
            return data.conversations[index];
        }

        return null;
    }

    /**
     * Export all conversations
     */
    async exportData() {
        return await this.getData();
    }

    /**
     * Import conversations (with validation)
     */
    async importData(importData) {
        if (!importData || typeof importData !== 'object') {
            throw new Error('Invalid import data format');
        }

        if (!Array.isArray(importData.conversations)) {
            importData.conversations = [];
        }

        if (!importData.version) {
            importData.version = '1.0.0';
        }

        importData.conversations = this.validateConversations(importData.conversations);

        const existingData = await this.getData();

        const mergedData = {
            version: importData.version || existingData.version || '1.0.0',
            conversations: this.mergeConversations(existingData.conversations || [], importData.conversations),
            activeConversationId: existingData.activeConversationId
        };

        await this.saveData(mergedData);
    }

    /**
     * Merge conversations - avoid duplicates by ID, regenerate IDs for imported data
     */
    mergeConversations(existing, imported) {
        const merged = [...existing];

        imported.forEach(importedConversation => {
            const newConversation = {
                ...importedConversation,
                id: `imported-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
                createdAt: importedConversation.createdAt || new Date().toISOString(),
                updatedAt: new Date().toISOString()
            };
            merged.push(newConversation);
        });

        return merged;
    }

    /**
     * Validate conversations array
     */
    validateConversations(conversations) {
        if (!Array.isArray(conversations)) return [];

        return conversations.filter(conversation => {
            return conversation &&
                   typeof conversation === 'object' &&
                   conversation.id &&
                   conversation.title &&
                   Array.isArray(conversation.messages) &&
                   conversation.messages.every(msg =>
                       msg &&
                       typeof msg === 'object' &&
                       msg.role &&
                       msg.content &&
                       (msg.role === 'user' || msg.role === 'assistant' || msg.role === 'system')
                   );
        });
    }

    /**
     * Migrate data structure if needed
     */
    migrateData(data) {
        if (!data.version) {
            data.version = '1.0.0';
        }

        if (!Array.isArray(data.conversations)) {
            data.conversations = [];
        }

        if (data.activeConversationId === undefined) {
            data.activeConversationId = null;
        }

        return data;
    }

    /**
     * Clear all data (for testing/reset)
     */
    async clearAllData() {
        await this.context.globalState.update(this.STORAGE_KEY, null);
    }
}

module.exports = { ChatDataManager };
