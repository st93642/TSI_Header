(function() {
    'use strict';

    const vscode = acquireVsCodeApi();
    const persistedState = vscode.getState() || {};

    const state = {
        conversations: [],
        activeConversationId: null,
        models: [],
        connection: { state: 'unknown', message: 'Not connected' },
        defaultModel: null,
        isStreaming: false,
        drafts: persistedState.drafts || {},
        autoScroll: true,
        lastRenderedConversationId: null
    };

    const elements = {};
    const scrollToBottom = debounce(() => {
        if (!elements.transcript) {
            return;
        }
        elements.transcript.scrollTop = elements.transcript.scrollHeight;
    }, 120);

    document.addEventListener('DOMContentLoaded', () => {
        cacheElements();
        attachEventListeners();
        updateComposerHint();
        vscode.postMessage({ type: 'chat:init' });
    });

    window.addEventListener('message', (event) => {
        const message = event.data;
        if (!message || !message.type) {
            return;
        }

        switch (message.type) {
            case 'chat:data':
                applyState(message);
                break;
            case 'chat:models':
                updateModels(message);
                break;
            case 'chat:error':
                showNotification(message.message || 'Something went wrong', 'error', message.detail);
                setStreaming(false);
                break;
            case 'chat:messagePending':
                setStreaming(true);
                break;
            case 'chat:messageCompleted':
                setStreaming(false);
                if (message.reason === 'cancelled') {
                    showNotification('Generation cancelled', 'info');
                }
                break;
            case 'chat:notification':
                showNotification(message.message, message.variant || 'info', message.detail);
                break;
            default:
                break;
        }
    });

    function cacheElements() {
        elements.conversationList = document.getElementById('conversation-list');
        elements.newConversationBtn = document.getElementById('new-conversation-btn');
        elements.modelSelect = document.getElementById('model-select');
        elements.refreshModelsBtn = document.getElementById('refresh-models-btn');
        elements.connectionPill = document.getElementById('connection-pill');
        elements.openSettingsBtn = document.getElementById('open-settings-btn');
        elements.transcript = document.getElementById('chat-transcript');
        elements.input = document.getElementById('chat-input');
        elements.sendButton = document.getElementById('send-btn');
        elements.stopButton = document.getElementById('stop-btn');
        elements.notifications = document.getElementById('chat-notifications');
        elements.composerHint = document.getElementById('composer-hint');
    }

    function attachEventListeners() {
        if (elements.newConversationBtn) {
            elements.newConversationBtn.addEventListener('click', createConversation);
        }

        if (elements.modelSelect) {
            elements.modelSelect.addEventListener('change', handleModelChange);
        }

        if (elements.refreshModelsBtn) {
            elements.refreshModelsBtn.addEventListener('click', () => {
                vscode.postMessage({ type: 'chat:refreshModels' });
            });
        }

        if (elements.openSettingsBtn) {
            elements.openSettingsBtn.addEventListener('click', () => {
                vscode.postMessage({ type: 'chat:openSettings' });
            });
        }

        if (elements.sendButton) {
            elements.sendButton.addEventListener('click', sendMessage);
        }

        if (elements.stopButton) {
            elements.stopButton.addEventListener('click', () => {
                vscode.postMessage({ type: 'chat:stopGeneration' });
            });
        }

        if (elements.input) {
            elements.input.addEventListener('keydown', (event) => {
                if ((event.metaKey || event.ctrlKey) && event.key === 'Enter') {
                    event.preventDefault();
                    sendMessage();
                }
            });

            elements.input.addEventListener('input', (event) => {
                updateDraft(event.target.value || '');
                updateSendButtonState();
            });
        }

        if (elements.transcript) {
            elements.transcript.addEventListener('scroll', () => {
                if (!elements.transcript) {
                    return;
                }
                const { scrollTop, scrollHeight, clientHeight } = elements.transcript;
                const distanceFromBottom = scrollHeight - (scrollTop + clientHeight);
                state.autoScroll = distanceFromBottom < 80;
            });
        }
    }

    function applyState(message) {
        const previousConversationId = state.activeConversationId;
        state.conversations = Array.isArray(message.conversations) ? message.conversations : [];
        state.activeConversationId = message.activeConversationId || previousConversationId || (state.conversations[0]?.id ?? null);
        if (state.activeConversationId !== previousConversationId) {
            state.autoScroll = true;
        }
        if (Array.isArray(message.models)) {
            state.models = message.models;
        }
        if (message.connection) {
            state.connection = message.connection;
        }
        if (message.defaultModel) {
            state.defaultModel = message.defaultModel;
        }

        renderAll();
    }

    function updateModels(message) {
        if (Array.isArray(message.models)) {
            state.models = message.models;
        }
        if (message.connection) {
            state.connection = message.connection;
        }
        renderToolbar();
    }

    function renderAll() {
        renderConversationList();
        renderToolbar();
        renderTranscript();
        renderComposer();
        updateSendButtonState();
    }

    function renderConversationList() {
        if (!elements.conversationList) {
            return;
        }

        elements.conversationList.innerHTML = '';

        if (!state.conversations.length) {
            const empty = document.createElement('div');
            empty.className = 'empty-conversation-list';
            empty.textContent = 'No conversations yet. Start a new one to begin chatting.';
            elements.conversationList.appendChild(empty);
            return;
        }

        state.conversations.forEach((conversation) => {
            const item = document.createElement('div');
            item.className = 'conversation-item';
            if (conversation.id === state.activeConversationId) {
                item.classList.add('is-active');
            }
            item.addEventListener('click', () => {
                if (conversation.id !== state.activeConversationId) {
                    vscode.postMessage({ type: 'chat:selectConversation', conversationId: conversation.id });
                }
            });

            const title = document.createElement('div');
            title.className = 'conversation-title';
            title.textContent = conversation.title || 'Untitled conversation';
            item.appendChild(title);

            const meta = document.createElement('div');
            meta.className = 'conversation-meta';
            meta.textContent = `Updated ${formatRelativeTime(conversation.updatedAt || conversation.createdAt)}`;
            item.appendChild(meta);

            const actions = document.createElement('div');
            actions.className = 'conversation-actions';

            const renameBtn = document.createElement('button');
            renameBtn.type = 'button';
            renameBtn.textContent = 'Rename';
            renameBtn.addEventListener('click', (event) => {
                event.stopPropagation();
                renameConversation(conversation);
            });

            const deleteBtn = document.createElement('button');
            deleteBtn.type = 'button';
            deleteBtn.textContent = 'Delete';
            deleteBtn.addEventListener('click', (event) => {
                event.stopPropagation();
                deleteConversation(conversation);
            });

            actions.appendChild(renameBtn);
            actions.appendChild(deleteBtn);
            item.appendChild(actions);

            elements.conversationList.appendChild(item);
        });
    }

    function renderToolbar() {
        renderConnectionStatus();
        renderModelSelect();
    }

    function renderConnectionStatus() {
        if (!elements.connectionPill) {
            return;
        }

        const statusClass = state.connection?.state ? `status-${state.connection.state}` : '';
        elements.connectionPill.className = `connection-pill ${statusClass}`.trim();
        elements.connectionPill.textContent = state.connection?.message || 'Unknown';

        if (state.connection?.detail) {
            elements.connectionPill.title = state.connection.detail;
        } else {
            elements.connectionPill.removeAttribute('title');
        }
    }

    function renderModelSelect() {
        if (!elements.modelSelect) {
            return;
        }

        const activeConversation = getActiveConversation();
        elements.modelSelect.innerHTML = '';

        if (!activeConversation) {
            const option = document.createElement('option');
            option.textContent = 'No conversation selected';
            option.value = '';
            elements.modelSelect.appendChild(option);
            elements.modelSelect.disabled = true;
            return;
        }

        elements.modelSelect.disabled = false;

        const models = state.models.length ? state.models : [];
        const seenModels = new Set();

        models.forEach((model) => {
            if (!model?.name) {
                return;
            }
            seenModels.add(model.name);
            const option = document.createElement('option');
            option.value = model.name;
            option.textContent = model.name;
            elements.modelSelect.appendChild(option);
        });

        if (activeConversation.model && !seenModels.has(activeConversation.model)) {
            const option = document.createElement('option');
            option.value = activeConversation.model;
            option.textContent = `${activeConversation.model} (saved)`;
            elements.modelSelect.appendChild(option);
        }

        const fallbackModel = activeConversation.model || state.defaultModel || (state.models[0]?.name ?? '');

        if (fallbackModel && !Array.from(elements.modelSelect.options).some((option) => option.value === fallbackModel)) {
            const option = document.createElement('option');
            option.value = fallbackModel;
            option.textContent = fallbackModel;
            elements.modelSelect.appendChild(option);
        }

        if (!fallbackModel) {
            const placeholder = document.createElement('option');
            placeholder.value = '';
            placeholder.textContent = state.models.length ? 'Select a model' : 'No models detected';
            elements.modelSelect.appendChild(placeholder);
        }

        elements.modelSelect.value = fallbackModel;
    }

    function renderTranscript() {
        if (!elements.transcript) {
            return;
        }

        elements.transcript.innerHTML = '';
        const activeConversation = getActiveConversation();

        if (!activeConversation) {
            const empty = document.createElement('div');
            empty.className = 'empty-chat-state';
            empty.textContent = 'Select or create a conversation to start chatting.';
            elements.transcript.appendChild(empty);
            return;
        }

        if (!Array.isArray(activeConversation.messages) || !activeConversation.messages.length) {
            const placeholder = document.createElement('div');
            placeholder.className = 'empty-chat-state';
            placeholder.textContent = 'Send a message to begin the conversation.';
            elements.transcript.appendChild(placeholder);
        } else {
            activeConversation.messages.forEach((message) => {
                const bubble = document.createElement('div');
                const role = (message.role || 'assistant').toLowerCase();
                bubble.className = `message-bubble ${role}`;

                const roleLabel = document.createElement('div');
                roleLabel.className = 'message-role';
                roleLabel.textContent = role === 'user' ? 'You' : 'Assistant';

                const content = document.createElement('div');
                content.className = 'message-content';
                content.textContent = message.content || '';

                const timestamp = document.createElement('div');
                timestamp.className = 'message-timestamp';
                timestamp.textContent = formatTimestamp(message.timestamp);

                bubble.appendChild(roleLabel);
                bubble.appendChild(content);
                bubble.appendChild(timestamp);
                elements.transcript.appendChild(bubble);
            });
        }

        if (state.isStreaming) {
            const streamingIndicator = document.createElement('div');
            streamingIndicator.className = 'streaming-indicator';
            streamingIndicator.textContent = 'Assistant is thinking...';
            elements.transcript.appendChild(streamingIndicator);
        }

        if (state.autoScroll) {
            scrollToBottom();
        }
    }

    function renderComposer() {
        if (!elements.input) {
            return;
        }

        const activeConversation = getActiveConversation();
        const hasConversation = Boolean(activeConversation);

        if (!hasConversation) {
            elements.input.value = '';
            elements.input.disabled = true;
            state.lastRenderedConversationId = null;
        } else {
            elements.input.disabled = false;
            const draft = state.drafts[activeConversation.id] || '';
            if (state.lastRenderedConversationId !== activeConversation.id || document.activeElement !== elements.input) {
                elements.input.value = draft;
            }
            state.lastRenderedConversationId = activeConversation.id;
        }

        if (elements.stopButton) {
            elements.stopButton.disabled = !state.isStreaming;
        }
    }

    function updateSendButtonState() {
        if (!elements.sendButton) {
            return;
        }
        const activeConversation = getActiveConversation();
        if (!activeConversation || state.isStreaming) {
            elements.sendButton.disabled = true;
            return;
        }
        const draft = (state.drafts[activeConversation.id] || '').trim();
        elements.sendButton.disabled = draft.length === 0;
    }

    function createConversation() {
        const suggested = `Conversation ${state.conversations.length + 1}`;
        const response = prompt('Name your new conversation', suggested);
        const title = response && response.trim() ? response.trim() : suggested;
        vscode.postMessage({ type: 'chat:createConversation', title });
    }

    function renameConversation(conversation) {
        const currentTitle = conversation.title || 'Conversation';
        const result = prompt('Rename conversation', currentTitle);
        if (result === null) {
            return;
        }
        const trimmed = result.trim();
        if (!trimmed) {
            showNotification('Conversation title cannot be empty', 'error');
            return;
        }
        vscode.postMessage({
            type: 'chat:renameConversation',
            conversationId: conversation.id,
            title: trimmed
        });
    }

    function deleteConversation(conversation) {
        const confirmed = confirm(`Delete "${conversation.title || 'Conversation'}"? This cannot be undone.`);
        if (!confirmed) {
            return;
        }
        delete state.drafts[conversation.id];
        persistDrafts();
        vscode.postMessage({ type: 'chat:deleteConversation', conversationId: conversation.id });
    }

    function handleModelChange(event) {
        const conversation = getActiveConversation();
        if (!conversation) {
            return;
        }
        const value = event.target.value;
        if (!value || value === conversation.model) {
            return;
        }
        vscode.postMessage({
            type: 'chat:updateConversationModel',
            conversationId: conversation.id,
            model: value
        });
    }

    function sendMessage() {
        const conversation = getActiveConversation();
        if (!conversation || state.isStreaming) {
            return;
        }

        const content = (state.drafts[conversation.id] || '').trim();
        if (!content) {
            return;
        }

        vscode.postMessage({
            type: 'chat:sendMessage',
            conversationId: conversation.id,
            content
        });

        state.drafts[conversation.id] = '';
        persistDrafts();
        if (elements.input) {
            elements.input.value = '';
        }
        updateSendButtonState();
        state.autoScroll = true;
        setStreaming(true);
    }

    function setStreaming(value) {
        state.isStreaming = Boolean(value);
        renderComposer();
        updateSendButtonState();
        if (!value) {
            renderTranscript();
        }
    }

    function updateDraft(value) {
        const conversationId = state.activeConversationId;
        if (!conversationId) {
            return;
        }
        state.drafts[conversationId] = value;
        persistDrafts();
    }

    function persistDrafts() {
        vscode.setState({ drafts: state.drafts });
    }

    function getActiveConversation() {
        if (!state.activeConversationId) {
            return null;
        }
        return state.conversations.find((conversation) => conversation.id === state.activeConversationId) || null;
    }

    function showNotification(message, variant = 'info', detail) {
        if (!message || !elements.notifications) {
            return;
        }
        const notification = document.createElement('div');
        notification.className = `chat-notification ${variant}`.trim();
        notification.textContent = detail ? `${message}: ${detail}` : message;
        elements.notifications.appendChild(notification);
        setTimeout(() => {
            notification.remove();
        }, 4000);
    }

    function formatRelativeTime(dateString) {
        if (!dateString) {
            return 'recently';
        }
        const date = new Date(dateString);
        if (isNaN(date.getTime())) {
            return 'recently';
        }
        const diffMs = Date.now() - date.getTime();
        const seconds = Math.floor(diffMs / 1000);
        if (seconds < 60) {
            return 'just now';
        }
        const minutes = Math.floor(seconds / 60);
        if (minutes < 60) {
            return `${minutes}m ago`;
        }
        const hours = Math.floor(minutes / 60);
        if (hours < 24) {
            return `${hours}h ago`;
        }
        const days = Math.floor(hours / 24);
        if (days === 1) {
            return 'yesterday';
        }
        if (days < 7) {
            return `${days}d ago`;
        }
        return date.toLocaleDateString();
    }

    function formatTimestamp(dateString) {
        if (!dateString) {
            return '';
        }
        const date = new Date(dateString);
        if (isNaN(date.getTime())) {
            return '';
        }
        return date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
    }

    function updateComposerHint() {
        if (!elements.composerHint) {
            return;
        }
        const isMac = navigator.platform.toUpperCase().includes('MAC');
        elements.composerHint.textContent = `Press ${isMac ? '⌘' : 'Ctrl'} + Enter to send`;
    }

    function debounce(fn, wait) {
        let timeout;
        return (...args) => {
            clearTimeout(timeout);
            timeout = setTimeout(() => fn.apply(null, args), wait);
        };
    }
})();
