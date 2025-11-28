/**
 * Chat Manager
 * Orchestrates ChatDataManager, ChatService, and ChatWebviewProvider
 * Handles commands, configuration changes, and message protocols
 */

let vscode;
try {
    vscode = require('vscode');
} catch (error) {
    // In test environment, use global mock
    vscode = global.vscode || {};
}
const { BaseManager } = require('../../core/src/baseManager');
const { ChatDataManager } = require('./chatDataManager');
const { ChatService } = require('./chatService');
const { ChatWebviewProvider } = require('./chatWebviewProvider');

class ChatManager extends BaseManager {
    constructor(context) {
        super(context);
        this.dataManager = new ChatDataManager(context);
        this.chatService = new ChatService(vscode);
        this.webviewProvider = new ChatWebviewProvider(context, {
            dataManager: this.dataManager,
            chatService: this.chatService
        });
    }

    /**
     * Phase 1: Register views (webview provider)
     */
    registerViews(context) {
        if (this._initialized.views) {
            console.warn('ChatManager: registerViews called multiple times, skipping');
            return;
        }
        this._initialized.views = true;

        try {
            const webviewViewDisposable = vscode.window.registerWebviewViewProvider(
                'tsi-chat-view',
                this.webviewProvider
            );
            this._addDisposable(webviewViewDisposable);
            context.subscriptions.push(webviewViewDisposable);
            console.log('ChatManager: Webview provider registered successfully');
        } catch (error) {
            console.error('ChatManager: Failed to register webview provider:', error);
            throw error;
        }
    }

    /**
     * Phase 2: Register commands
     */
    registerCommands(context) {
        if (this._initialized.commands) {
            console.warn('ChatManager: registerCommands called multiple times, skipping');
            return;
        }
        this._initialized.commands = true;

        try {
            this._registerAllCommands(context);
            console.log('ChatManager: Commands registered successfully');
        } catch (error) {
            console.error('ChatManager: Failed to register commands:', error);
            throw error;
        }
    }

    /**
     * Phase 3: Setup listeners (configuration change listener)
     */
    setupListeners(context) {
        if (this._initialized.listeners) {
            console.warn('ChatManager: setupListeners called multiple times, skipping');
            return;
        }
        this._initialized.listeners = true;

        try {
            const configListener = vscode.workspace.onDidChangeConfiguration((event) => {
                if (event.affectsConfiguration('tsiheader.chat')) {
                    this.chatService.refreshConfig();
                    if (this.webviewProvider.view) {
                        this.webviewProvider.refreshModels();
                    }
                }
            });

            this._addDisposable(configListener);
            context.subscriptions.push(configListener);
            console.log('ChatManager: Configuration listener setup successfully');
        } catch (error) {
            console.error('ChatManager: Failed to setup listeners:', error);
            throw error;
        }
    }

    /**
     * Register all chat commands
     */
    _registerAllCommands(context) {
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

        // Track all commands for disposal
        const commands = [
            openChatCmd,
            newConversationCmd,
            clearHistoryCmd
        ];

        commands.forEach(cmd => {
            this._addDisposable(cmd);
            context.subscriptions.push(cmd);
        });
    }

    /**
     * Dispose of resources
     */
    dispose() {
        if (this.webviewProvider) {
            try {
                this.webviewProvider.dispose();
            } catch (error) {
                console.error('ChatManager: Error disposing webview provider:', error);
            }
        }
        super.dispose();
    }
}

module.exports = { ChatManager };
