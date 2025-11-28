/**
 * ActivationManager
 * 
 * Encapsulates extension activation logic with two phases:
 * 1. registerViews() - Register all tree/webview providers synchronously
 * 2. initializeModules() - Lazy-load and initialize managers
 * 3. registerCommands() - Register all commands
 */

let vscode;
try {
    vscode = require('vscode');
} catch (error) {
    // In test environment, vscode may not be available
    vscode = {};
}

const path = require('path');

class ActivationManager {
    constructor(context) {
        this.context = context;
        this.tsiCommandsProvider = null;
        this.tsiProjectsProvider = null;
        this.calendarManager = null;
        this.chatManager = null;
        this.core = null;
        this.studyModeExtension = null;
        
        // Track registrations for logging
        this.registrations = {
            treeProviders: [],
            webviewProviders: [],
            commands: [],
            failures: []
        };
    }

    /**
     * Phase 1: Register all views synchronously
     * This must happen before any other initialization to prevent "no provider" errors
     */
    registerViews() {
        console.log('ActivationManager: Phase 1 - Registering views');
        
        this._registerTreeProviders();
        this._registerCalendarTreeProvider();
        this._registerChatWebviewProvider();
        
        console.log(`ActivationManager: Registered ${this.registrations.treeProviders.length} tree providers`);
        console.log(`ActivationManager: Registered ${this.registrations.webviewProviders.length} webview providers`);
        if (this.registrations.failures.length > 0) {
            console.warn(`ActivationManager: ${this.registrations.failures.length} failures during view registration`);
        }
    }

    /**
     * Phase 2: Lazy-load and initialize all managers
     */
    async initializeModules() {
        console.log('ActivationManager: Phase 2 - Initializing modules');
        
        // Initialize core
        this._initializeCore();
        
        // Initialize study mode
        this._initializeStudyMode();
        
        // Initialize calendar commands
        await this._initializeCalendarCommands();
        
        // Initialize chat commands
        await this._initializeChatCommands();
        
        console.log('ActivationManager: Module initialization complete');
    }

    /**
     * Phase 3: Register all commands
     * This includes commands from core, calendar, chat, learn, etc.
     */
    async registerCommands() {
        console.log('ActivationManager: Phase 3 - Registering commands');
        
        // Commands are registered during initializeModules phase
        // This method can be used for additional command registration
        // or to register commands that don't need lazy-loading
        
        console.log(`ActivationManager: Registered ${this.registrations.commands.length} commands`);
    }

    /**
     * Dispose all resources
     */
    dispose() {
        // Dispose managers
        if (this.calendarManager) {
            try {
                this.calendarManager.dispose?.();
            } catch (error) {
                console.error('Error disposing calendar manager:', error);
            }
        }
        
        if (this.studyModeExtension) {
            try {
                this.studyModeExtension.dispose?.();
            } catch (error) {
                console.error('Error disposing study mode:', error);
            }
        }
    }

    // ===== PRIVATE METHODS =====

    /**
     * Register TSI tree view providers
     */
    _registerTreeProviders() {
        try {
            const { TSITreeDataProvider, TSIProjectDataProvider } = require('../tsiViewProvider');
            
            this.tsiCommandsProvider = new TSITreeDataProvider();
            this.tsiProjectsProvider = new TSIProjectDataProvider();
            
            vscode.window.registerTreeDataProvider('tsi-commands', this.tsiCommandsProvider);
            vscode.window.registerTreeDataProvider('tsi-projects', this.tsiProjectsProvider);
            
            this.registrations.treeProviders.push('tsi-commands', 'tsi-projects');
            console.log('ActivationManager: TSI tree providers registered successfully');
        } catch (error) {
            this._logFailure('TSI tree providers', error);
        }
    }

    /**
     * Register calendar tree provider
     */
    _registerCalendarTreeProvider() {
        try {
            const { CalendarManager } = require(path.join(__dirname, '..', '..', '..', 'calendar', 'src'));
            
            this.calendarManager = new CalendarManager(this.context);
            this.calendarManager.registerTreeProvider();
            
            this.registrations.treeProviders.push('tsi-calendar');
            console.log('ActivationManager: Calendar tree provider registered successfully');
        } catch (error) {
            this._logFailure('Calendar tree provider', error);
        }
    }

    /**
     * Register chat webview provider
     */
    _registerChatWebviewProvider() {
        try {
            const chatPath = path.join(__dirname, '..', '..', '..', 'chat', 'src');
            const { ChatDataManager } = require(path.join(chatPath, 'chatDataManager'));
            const { ChatService } = require(path.join(chatPath, 'chatService'));
            const { ChatWebviewProvider } = require(path.join(chatPath, 'chatWebviewProvider'));
            
            const chatDataManager = new ChatDataManager(this.context);
            const chatService = new ChatService(vscode);
            const chatWebviewProvider = new ChatWebviewProvider(this.context, {
                dataManager: chatDataManager,
                chatService: chatService
            });
            
            const webviewViewDisposable = vscode.window.registerWebviewViewProvider(
                'tsi-chat-view',
                chatWebviewProvider
            );
            this.context.subscriptions.push(webviewViewDisposable);
            
            this.chatManager = {
                webviewProvider: chatWebviewProvider,
                dataManager: chatDataManager,
                chatService: chatService
            };
            
            this.registrations.webviewProviders.push('tsi-chat-view');
            console.log('ActivationManager: Chat webview provider registered successfully');
        } catch (error) {
            this._logFailure('Chat webview provider', error);
        }
    }

    /**
     * Initialize core interface
     */
    _initializeCore() {
        try {
            const { TSICore } = require('../index');
            this.core = new TSICore(this.context.extensionPath);
            console.log('ActivationManager: Core initialized successfully');
        } catch (error) {
            this._logFailure('Core initialization', error);
        }
    }

    /**
     * Initialize study mode extension
     */
    _initializeStudyMode() {
        try {
            const { StudyModeExtension } = require('../studyModeExtension');
            this.studyModeExtension = new StudyModeExtension(vscode, this.context);
            this.studyModeExtension.activate();
            console.log('ActivationManager: Study mode initialized successfully');
        } catch (error) {
            this._logFailure('Study mode initialization', error);
        }
    }

    /**
     * Initialize calendar commands
     */
    async _initializeCalendarCommands() {
        if (!this.calendarManager) {
            return;
        }
        
        try {
            this.calendarManager.initializeCommands();
            console.log('ActivationManager: Calendar commands initialized successfully');
        } catch (error) {
            this._logFailure('Calendar commands initialization', error);
        }
    }

    /**
     * Initialize chat commands
     */
    async _initializeChatCommands() {
        if (!this.chatManager || !this.chatManager.webviewProvider) {
            return;
        }
        
        try {
            const openChatCmd = vscode.commands.registerCommand('tsiheader.openChat', async () => {
                await vscode.commands.executeCommand('workbench.view.extension.tsi-chat-container');
            });
            
            const newConversationCmd = vscode.commands.registerCommand('tsiheader.newChatConversation', async () => {
                const config = this.chatManager.chatService.getConfig();
                await this.chatManager.dataManager.createConversation('New Conversation', config.model);
                if (this.chatManager.webviewProvider.view) {
                    await this.chatManager.webviewProvider._postState({ reason: 'newConversation' });
                }
            });
            
            const clearHistoryCmd = vscode.commands.registerCommand('tsiheader.clearChatHistory', async () => {
                const result = await vscode.window.showWarningMessage(
                    'Are you sure you want to clear all chat history?',
                    'Clear', 'Cancel'
                );
                if (result === 'Clear') {
                    await this.chatManager.dataManager.clearAllData();
                    if (this.chatManager.webviewProvider.view) {
                        await this.chatManager.webviewProvider._handleInitialize();
                    }
                }
            });
            
            this.context.subscriptions.push(openChatCmd, newConversationCmd, clearHistoryCmd);
            this.registrations.commands.push('tsiheader.openChat', 'tsiheader.newChatConversation', 'tsiheader.clearChatHistory');
            
            console.log('ActivationManager: Chat commands initialized successfully');
        } catch (error) {
            this._logFailure('Chat commands initialization', error);
        }
    }

    /**
     * Log a failure during initialization
     */
    _logFailure(component, error) {
        console.error(`ActivationManager: Failed to initialize ${component}:`, error);
        this.registrations.failures.push({
            component,
            error: error.message || String(error)
        });
    }

    /**
     * Get the calendar manager (for dependency injection)
     */
    getCalendarManager() {
        return this.calendarManager;
    }

    /**
     * Get the chat manager (for dependency injection)
     */
    getChatManager() {
        return this.chatManager;
    }

    /**
     * Get the core instance (for dependency injection)
     */
    getCore() {
        return this.core;
    }

    /**
     * Get the study mode extension (for dependency injection)
     */
    getStudyModeExtension() {
        return this.studyModeExtension;
    }

    /**
     * Get registration statistics
     */
    getStats() {
        return {
            treeProviders: this.registrations.treeProviders.length,
            webviewProviders: this.registrations.webviewProviders.length,
            commands: this.registrations.commands.length,
            failures: this.registrations.failures.length
        };
    }
}

module.exports = { ActivationManager };
