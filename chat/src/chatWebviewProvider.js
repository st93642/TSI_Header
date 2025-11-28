let vscode;
try {
    vscode = require('vscode');
} catch (error) {
    // In test environment, use global mock
    vscode = global.vscode || {};
}
const { ChatDataManager } = require('./chatDataManager');
const { ChatService } = require('./chatService');

class ChatWebviewProvider {
    constructor(context, options = {}) {
        this.context = context;
        this.extensionUri = context.extensionUri;
        this.chatDataManager = options.dataManager || new ChatDataManager(context);
        this.chatService = options.chatService || new ChatService(vscode);
        this.view = null;
        this.cachedModels = [];
        this.connectionStatus = { state: 'unknown', message: 'Model list has not been loaded yet.' };
        this.pendingRequest = null;
        this._configListener = vscode.workspace.onDidChangeConfiguration((event) => {
            if (event.affectsConfiguration('tsiheader.chat')) {
                this.chatService.refreshConfig();
                this.refreshModels();
            }
        });
    }

    resolveWebviewView(webviewView) {
        this.view = webviewView;
        const { webview } = webviewView;

        webview.options = {
            enableScripts: true,
            localResourceRoots: [
                vscode.Uri.joinPath(this.extensionUri, 'chat', 'resources')
            ]
        };

        webview.html = this._getHtmlForWebview(webview);

        webview.onDidReceiveMessage(async (message) => {
            await this._handleMessage(message);
        });

        webviewView.onDidDispose(() => {
            if (this.pendingRequest?.abortController) {
                try {
                    this.pendingRequest.abortController.abort();
                } catch (error) {
                    console.warn('Failed to abort chat request on dispose', error);
                }
            }
            this.pendingRequest = null;
            this.view = null;
        });
    }

    dispose() {
        if (this._configListener) {
            this._configListener.dispose();
            this._configListener = null;
        }
    }

    async _handleMessage(message) {
        if (!message || !message.type) {
            return;
        }

        try {
            switch (message.type) {
                case 'chat:init':
                case 'chat:requestState':
                    await this._handleInitialize();
                    break;
                case 'chat:createConversation':
                    await this._handleCreateConversation(message.title);
                    break;
                case 'chat:renameConversation':
                    await this._handleRenameConversation(message.conversationId);
                    break;
                case 'chat:deleteConversation':
                    await this._handleDeleteConversation(message.conversationId);
                    break;
                case 'chat:selectConversation':
                    await this._handleSelectConversation(message.conversationId);
                    break;
                case 'chat:updateConversationModel':
                    await this._handleUpdateConversationModel(message.conversationId, message.model);
                    break;
                case 'chat:sendMessage':
                    await this._handleSendMessage(message.conversationId, message.content);
                    break;
                case 'chat:refreshModels':
                    await this.refreshModels();
                    break;
                case 'chat:openSettings':
                    await vscode.commands.executeCommand('workbench.action.openSettings', '@ext:st93642.uni-header chat');
                    break;
                case 'chat:stopGeneration':
                    await this._handleStopGeneration();
                    break;
                default:
                    console.warn('Unknown chat message received:', message);
                    break;
            }
        } catch (error) {
            console.error('Failed to handle chat webview message', error);
            this._postError('Chat action failed', error.message);
        }
    }

    async _handleInitialize() {
        await this._ensureConversation();
        await this.refreshModels({ notify: false, emitState: false });
        await this._postState({ reason: 'init' });
    }

    async _handleCreateConversation(title) {
        const config = this.chatService.getConfig();
        const conversationTitle = (title && title.trim()) || 'New Conversation';
        await this.chatDataManager.createConversation(conversationTitle, config.model);
        await this._postState({ reason: 'createConversation' });
    }

    async _handleRenameConversation(conversationId) {
        if (!conversationId) {
            return;
        }

        const conversation = await this.chatDataManager.getConversation(conversationId);
        if (!conversation) {
            return;
        }

        const currentTitle = conversation.title || 'Conversation';
        const newTitle = await vscode.window.showInputBox({
            prompt: 'Rename conversation',
            value: currentTitle,
            placeHolder: 'Enter new conversation title'
        });

        if (!newTitle || !newTitle.trim()) {
            return;
        }

        await this.chatDataManager.renameConversation(conversationId, newTitle.trim());
        await this._postState({ reason: 'renameConversation' });
    }

    async _handleDeleteConversation(conversationId) {
        if (!conversationId) {
            return;
        }

        const conversation = await this.chatDataManager.getConversation(conversationId);
        const title = conversation ? (conversation.title || 'Conversation') : 'Conversation';

        const result = await vscode.window.showWarningMessage(
            `Delete "${title}"? This cannot be undone.`,
            { modal: true },
            'Delete'
        );

        if (result !== 'Delete') {
            return;
        }

        await this.chatDataManager.deleteConversation(conversationId);
        await this._ensureConversation();
        await this._postState({ reason: 'deleteConversation' });
    }

    async _handleSelectConversation(conversationId) {
        if (!conversationId) {
            return;
        }

        await this.chatDataManager.setActiveConversation(conversationId);
        await this._postState({ reason: 'selectConversation' });
    }

    async _handleUpdateConversationModel(conversationId, model) {
        if (!conversationId || !model) {
            return;
        }

        const updated = await this.chatDataManager.updateConversationModel(conversationId, model);
        if (!updated) {
            this._postError('Unable to update model', 'Conversation not found.');
            return;
        }
        await this._postState({ reason: 'updateModel' });
    }

    async _handleSendMessage(conversationId, content) {
        if (!conversationId || !content || !content.trim()) {
            return;
        }

        if (this.pendingRequest) {
            this._postError('Assistant is already responding', 'Stop the current response before sending a new message.');
            return;
        }

        const cleanedContent = content.trim();
        const conversation = await this.chatDataManager.getConversation(conversationId);
        if (!conversation) {
            this._postError('Conversation not found', 'Please create a new conversation.');
            return;
        }

        await this.chatDataManager.appendMessage(conversationId, 'user', cleanedContent);
        await this._postState({ reason: 'userMessageAppended' });

        const abortController = new AbortController();
        this.pendingRequest = { conversationId, abortController };
        this._postMessage({ type: 'chat:messagePending', conversationId });

        const refreshedConversation = await this.chatDataManager.getConversation(conversationId);
        const configuredLimit = this.chatService.getConfig().historyLimit || refreshedConversation.messages.length;
        const historyLimit = Math.max(1, configuredLimit);
        const contextMessages = refreshedConversation.messages.slice(-historyLimit);

        const result = await this.chatService.sendMessage(contextMessages, refreshedConversation.model, {
            abortController
        });

        this.pendingRequest = null;

        if (result.success && result.message) {
            await this.chatDataManager.appendMessage(conversationId, 'assistant', result.message.content || '');
            await this._postState({ reason: 'assistantResponse' });
            this._postMessage({ type: 'chat:messageCompleted', conversationId });
        } else {
            if (result.error?.code === 'REQUEST_CANCELLED') {
                this._postMessage({
                    type: 'chat:messageCompleted',
                    conversationId,
                    reason: 'cancelled'
                });
                this._notify('Generation cancelled', 'info');
            } else {
                this._postMessage({ type: 'chat:messageCompleted', conversationId });
                this._postError(result.error?.message || 'Failed to generate response', result.error?.detail);
            }
        }
    }

    async _handleStopGeneration() {
        if (this.pendingRequest?.abortController) {
            try {
                this.pendingRequest.abortController.abort();
            } catch (error) {
                console.warn('Failed to abort chat request', error);
            }
        }
    }

    async refreshModels(options = {}) {
        const notify = options.notify !== false;
        const emitState = options.emitState !== false;

        if (notify) {
            this.connectionStatus = { state: 'loading', message: 'Refreshing model list...' };
            this._postMessage({
                type: 'chat:models',
                models: this.cachedModels,
                connection: this.connectionStatus
            });
        }

        const result = await this.chatService.listModels();
        if (result.success) {
            this.cachedModels = (result.models || []).map(model => ({
                name: model.name,
                size: model.size || null,
                modifiedAt: model.modified_at || model.modifiedAt || null
            }));
            this.connectionStatus = {
                state: 'connected',
                message: `${this.cachedModels.length} model${this.cachedModels.length === 1 ? '' : 's'} available`
            };
        } else {
            this.connectionStatus = {
                state: 'error',
                message: result.error?.message || 'Failed to load models',
                detail: result.error?.detail
            };
        }

        if (notify) {
            this._postMessage({
                type: 'chat:models',
                models: this.cachedModels,
                connection: this.connectionStatus
            });
            if (!result.success) {
                this._postError(this.connectionStatus.message, this.connectionStatus.detail);
            }
        }

        if (emitState) {
            await this._postState({ reason: 'refreshModels' });
        }
        return result;
    }

    async _ensureConversation() {
        const conversations = await this.chatDataManager.getConversations();
        if (!conversations || conversations.length === 0) {
            const config = this.chatService.getConfig();
            await this.chatDataManager.createConversation('New Conversation', config.model);
        }
    }

    async _postState(extra = {}) {
        if (!this.view) {
            return;
        }

        const data = await this.chatDataManager.getData();
        const conversations = Array.isArray(data.conversations) ? data.conversations : [];
        const normalized = conversations
            .map(conversation => ({ ...conversation }))
            .sort((a, b) => {
                const aDate = new Date(a.updatedAt || a.createdAt || 0).getTime();
                const bDate = new Date(b.updatedAt || b.createdAt || 0).getTime();
                return bDate - aDate;
            });

        let activeConversationId = data.activeConversationId;
        if (!activeConversationId && normalized.length > 0) {
            activeConversationId = normalized[0].id;
            await this.chatDataManager.setActiveConversation(activeConversationId);
        }

        const activeConversation = normalized.find(conversation => conversation.id === activeConversationId) || null;

        this._postMessage({
            type: 'chat:data',
            conversations: normalized,
            activeConversationId,
            activeConversation,
            models: this.cachedModels,
            connection: this.connectionStatus,
            defaultModel: this.chatService.getConfig().model,
            timestamp: new Date().toISOString(),
            ...extra
        });
    }

    _postMessage(payload) {
        if (this.view) {
            this.view.webview.postMessage(payload);
        }
    }

    _notify(message, variant = 'info') {
        this._postMessage({
            type: 'chat:notification',
            message,
            variant
        });
    }

    _postError(message, detail) {
        this._postMessage({
            type: 'chat:error',
            message,
            detail
        });
    }

    _getHtmlForWebview(webview) {
        const nonce = getNonce();
        const styleUri = webview.asWebviewUri(
            vscode.Uri.joinPath(this.extensionUri, 'chat', 'resources', 'chat.css')
        );
        const highlightCssUri = webview.asWebviewUri(
            vscode.Uri.joinPath(this.extensionUri, 'chat', 'resources', 'highlight-github-dark.min.css')
        );
        const markedUri = webview.asWebviewUri(
            vscode.Uri.joinPath(this.extensionUri, 'chat', 'resources', 'marked.min.js')
        );
        const highlightUri = webview.asWebviewUri(
            vscode.Uri.joinPath(this.extensionUri, 'chat', 'resources', 'highlight.min.js')
        );
        const scriptUri = webview.asWebviewUri(
            vscode.Uri.joinPath(this.extensionUri, 'chat', 'resources', 'chat.js')
        );

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src ${webview.cspSource} https: data:; style-src ${webview.cspSource}; script-src 'nonce-${nonce}'; font-src ${webview.cspSource} https: data:;">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="stylesheet" href="${styleUri}">
    <link rel="stylesheet" href="${highlightCssUri}">
    <title>Chat Assistant</title>
</head>
<body>
    <div class="chat-root">
        <aside class="chat-sidebar">
            <div class="sidebar-header">
                <div class="sidebar-title">
                    <span>Conversations</span>
                    <small>Stay in sync across sessions</small>
                </div>
                <button id="new-conversation-btn" class="icon-button" title="Start a new conversation">＋</button>
            </div>
            <div id="conversation-list" class="conversation-list"></div>
        </aside>
        <section class="chat-main">
            <div class="chat-toolbar">
                <div class="model-controls">
                    <label for="model-select">Model</label>
                    <select id="model-select" class="model-select"></select>
                    <button id="refresh-models-btn" class="icon-button" title="Refresh available models">⟳</button>
                </div>
                <div class="toolbar-actions">
                    <span id="connection-pill" class="connection-pill">Unknown</span>
                    <button id="open-settings-btn" class="ghost-button">Settings</button>
                </div>
            </div>
            <div id="chat-transcript" class="chat-transcript">
                <div class="empty-chat-state">Select or create a conversation to get started.</div>
            </div>
            <div class="chat-composer">
                <textarea id="chat-input" rows="3" placeholder="Message the assistant..."></textarea>
                <div class="composer-actions">
                    <span id="composer-hint" class="composer-hint">Press Ctrl/Cmd + Enter to send</span>
                    <div class="composer-buttons">
                        <button id="stop-btn" class="secondary" disabled>Stop</button>
                        <button id="send-btn" class="primary">Send</button>
                    </div>
                </div>
            </div>
        </section>
    </div>
    <div id="chat-notifications" class="chat-notifications"></div>
    <script nonce="${nonce}" src="${markedUri}"></script>
    <script nonce="${nonce}" src="${highlightUri}"></script>
    <script nonce="${nonce}" src="${scriptUri}"></script>
</body>
</html>`;
    }
}

function getNonce() {
    let text = '';
    const possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    for (let i = 0; i < 32; i++) {
        text += possible.charAt(Math.floor(Math.random() * possible.length));
    }
    return text;
}

module.exports = { ChatWebviewProvider };
