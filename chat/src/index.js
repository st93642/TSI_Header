/**
 * Chat Module
 * Exports all chat-related components
 */

const { ChatDataManager } = require('./chatDataManager');
const { ChatService } = require('./chatService');
const { ChatWebviewProvider } = require('./chatWebviewProvider');
const { ChatManager } = require('./chatManager');

module.exports = {
    ChatDataManager,
    ChatService,
    ChatWebviewProvider,
    ChatManager
};
