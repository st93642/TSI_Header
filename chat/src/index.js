/**
 * Chat Module
 * Exports all chat-related components
 */

const { ChatDataManager } = require('./chatDataManager');
const { ChatService } = require('./chatService');
const { ChatWebviewProvider } = require('./chatWebviewProvider');

module.exports = {
    ChatDataManager,
    ChatService,
    ChatWebviewProvider
};
