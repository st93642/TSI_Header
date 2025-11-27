/**
 * Chat Manager
 * Orchestrates ChatDataManager, ChatService, and ChatWebviewProvider
 * Handles commands, configuration changes, and message protocols
 */

const vscode = require('vscode');
const { ChatDataManager } = require('./chatDataManager');
const { ChatService } = require('./chatService');
const { ChatWebviewProvider } = require('./chatWebviewProvider');

class ChatManager {
    constructor(context) {
        this.context = context;
        this.dataManager = new ChatDataManager(context);
        this.chatService = new ChatService(vscode);
        this.webviewProvider = new ChatWebviewProvider(context, {
            dataManager: this.dataManager,
            chatService: this.chatService
        });
        this.configListener = null;
    }

    /**
     * Initialize the chat manager
     */
    async initialize() {
        try {
            // Register the webview view provider
            const webviewViewDisposable = vscode.window.registerWebviewViewProvider(
                'tsi-chat-view',
                this.webviewProvider
            );
            this.context.subscriptions.push(webviewViewDisposable);

            // Register commands
            this.registerCommands();

            // Register configuration change listener
            this.setupConfigListener();

            console.log('Chat manager initialized successfully');
        } catch (error) {
            console.error('Failed to initialize chat manager:', error);
            throw error;
        }
    }

    /**
     * Register chat commands
     */
    registerCommands() {
        // Open chat command - reveals the activity bar container
        const openChatCmd = vscode.commands.registerCommand('tsiheader.openChat', async () => {
            try {
                await vscode.commands.executeCommand('workbench.view.extension.tsi-chat-container');
            } catch (error) {
                console.error('Error opening chat:', error);
                vscode.window.showErrorMessage('Failed to open chat');
            }
        });

        // New chat conversation command
        const newConversationCmd = vscode.commands.registerCommand('tsiheader.newChatConversation', async () => {
            try {
                if (!this.webviewProvider.view) {
                    await vscode.commands.executeCommand('workbench.view.extension.tsi-chat-container');
                    // Wait for view to be ready
                    await new Promise(resolve => setTimeout(resolve, 100));
                }
                const config = this.chatService.getConfig();
                const conversation = await this.dataManager.createConversation('New Conversation', config.model);
                if (this.webviewProvider.view) {
                    await this.webviewProvider._postState({ reason: 'newConversation' });
                }
            } catch (error) {
                console.error('Error creating new conversation:', error);
                vscode.window.showErrorMessage('Failed to create new conversation');
            }
        });

        // Clear chat history command
        const clearHistoryCmd = vscode.commands.registerCommand('tsiheader.clearChatHistory', async () => {
            try {
                const result = await vscode.window.showWarningMessage(
                    'Are you sure you want to clear all chat history? This cannot be undone.',
                    'Clear',
                    'Cancel'
                );

                if (result === 'Clear') {
                    await this.dataManager.clearAllData();
                    if (this.webviewProvider.view) {
                        await this.webviewProvider._handleInitialize();
                    }
                    vscode.window.showInformationMessage('Chat history cleared');
                }
            } catch (error) {
                console.error('Error clearing chat history:', error);
                vscode.window.showErrorMessage('Failed to clear chat history');
            }
        });

        // Add commands to subscriptions
        this.context.subscriptions.push(
            openChatCmd,
            newConversationCmd,
            clearHistoryCmd
        );
    }

    /**
     * Setup configuration change listener
     */
    setupConfigListener() {
        this.configListener = vscode.workspace.onDidChangeConfiguration((event) => {
            if (event.affectsConfiguration('tsiheader.chat')) {
                this.chatService.refreshConfig();
                if (this.webviewProvider.view) {
                    this.webviewProvider.refreshModels();
                }
            }
        });

        this.context.subscriptions.push(this.configListener);
    }

    /**
     * Dispose of resources
     */
    dispose() {
        if (this.configListener) {
            this.configListener.dispose();
        }
        if (this.webviewProvider) {
            this.webviewProvider.dispose();
        }
    }
}

module.exports = { ChatManager };
