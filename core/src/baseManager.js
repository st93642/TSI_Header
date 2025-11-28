/**
 * BaseManager
 * 
 * Base class that defines a standard lifecycle pattern for feature managers.
 * All feature managers (Calendar, Chat, etc.) should follow this pattern for 
 * consistent initialization and cleanup.
 * 
 * Lifecycle Contract:
 * 1. registerViews(context) - Register tree providers and webview providers (must be synchronous)
 * 2. registerCommands(context) - Register all commands for the feature
 * 3. setupListeners(context) - Setup configuration listeners and other event handlers
 * 4. dispose() - Clean up all resources (commands, listeners, webviews, etc.)
 * 
 * Usage:
 * - Managers can extend this class or implement the same interface
 * - All disposables should be tracked internally in this._disposables
 * - Each lifecycle method should be idempotent (safe to call multiple times)
 */

class BaseManager {
    constructor(context) {
        this.context = context;
        this._disposables = [];
        this._initialized = {
            views: false,
            commands: false,
            listeners: false
        };
    }

    /**
     * Phase 1: Register views (tree providers, webview providers)
     * Must be synchronous and called during extension activation
     * Should be idempotent - calling multiple times has no effect after first call
     */
    registerViews(context) {
        if (this._initialized.views) {
            console.warn(`${this.constructor.name}: registerViews called multiple times, skipping`);
            return;
        }
        this._initialized.views = true;
    }

    /**
     * Phase 2: Register commands
     * Can be called after views are registered
     * Should be idempotent - calling multiple times has no effect after first call
     */
    registerCommands(context) {
        if (this._initialized.commands) {
            console.warn(`${this.constructor.name}: registerCommands called multiple times, skipping`);
            return;
        }
        this._initialized.commands = true;
    }

    /**
     * Phase 3: Setup listeners (configuration changes, events, etc.)
     * Can be called after commands are registered
     * Should be idempotent - calling multiple times has no effect after first call
     */
    setupListeners(context) {
        if (this._initialized.listeners) {
            console.warn(`${this.constructor.name}: setupListeners called multiple times, skipping`);
            return;
        }
        this._initialized.listeners = true;
    }

    /**
     * Dispose of all resources
     * Should clean up all commands, listeners, webviews, etc.
     */
    dispose() {
        this._disposables.forEach(disposable => {
            try {
                disposable.dispose();
            } catch (error) {
                console.error(`${this.constructor.name}: Error disposing resource:`, error);
            }
        });
        this._disposables = [];
        this._initialized = {
            views: false,
            commands: false,
            listeners: false
        };
    }

    /**
     * Track a disposable for cleanup
     */
    _addDisposable(disposable) {
        this._disposables.push(disposable);
    }

    /**
     * Check if all phases are initialized
     */
    isFullyInitialized() {
        return this._initialized.views && 
               this._initialized.commands && 
               this._initialized.listeners;
    }

    /**
     * Get initialization status
     */
    getInitializationStatus() {
        return { ...this._initialized };
    }
}

module.exports = { BaseManager };
