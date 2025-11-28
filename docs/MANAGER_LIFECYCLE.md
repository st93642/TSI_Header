# Manager Lifecycle Pattern

This document describes the standardized lifecycle pattern for feature managers in the TSI Header extension.

## Overview

All feature managers (CalendarManager, ChatManager, and future managers) should follow a consistent lifecycle pattern defined by the `BaseManager` class. This ensures:

- Predictable initialization order
- Consistent cleanup and disposal
- Prevention of duplicate registrations
- Graceful error handling
- Easy integration with the ActivationManager

## BaseManager Class

Located in `core/src/baseManager.js`, this base class defines the lifecycle contract that all managers should follow.

### Lifecycle Phases

The manager lifecycle consists of three distinct phases that must be executed in order:

#### 1. registerViews(context)

**Purpose**: Register tree providers and webview providers with VS Code

**When**: Must be called synchronously during extension activation (before any commands are registered)

**Characteristics**:
- Must be synchronous
- Idempotent (safe to call multiple times - only first call has effect)
- Should track all view disposables
- Should push disposables to context.subscriptions

**Example**:
```javascript
registerViews(context) {
    if (this._initialized.views) {
        console.warn('Manager: registerViews called multiple times, skipping');
        return;
    }
    this._initialized.views = true;

    try {
        const provider = new MyTreeDataProvider();
        const disposable = vscode.window.registerTreeDataProvider('my-view', provider);
        this._addDisposable(disposable);
        context.subscriptions.push(disposable);
    } catch (error) {
        console.error('Manager: Failed to register views:', error);
        throw error;
    }
}
```

#### 2. registerCommands(context)

**Purpose**: Register all commands for the feature

**When**: Can be called after views are registered, typically during the initialization phase

**Characteristics**:
- Can be synchronous or asynchronous
- Idempotent (safe to call multiple times - only first call has effect)
- Should track all command disposables
- Should push disposables to context.subscriptions

**Example**:
```javascript
registerCommands(context) {
    if (this._initialized.commands) {
        console.warn('Manager: registerCommands called multiple times, skipping');
        return;
    }
    this._initialized.commands = true;

    try {
        const commands = [
            vscode.commands.registerCommand('my.command', () => {}),
            vscode.commands.registerCommand('my.otherCommand', () => {})
        ];

        commands.forEach(cmd => {
            this._addDisposable(cmd);
            context.subscriptions.push(cmd);
        });
    } catch (error) {
        console.error('Manager: Failed to register commands:', error);
        throw error;
    }
}
```

#### 3. setupListeners(context)

**Purpose**: Setup configuration listeners, event handlers, and other reactive behavior

**When**: Can be called after commands are registered

**Characteristics**:
- Can be synchronous or asynchronous
- Idempotent (safe to call multiple times - only first call has effect)
- Should track all listener disposables
- Should push disposables to context.subscriptions

**Example**:
```javascript
setupListeners(context) {
    if (this._initialized.listeners) {
        console.warn('Manager: setupListeners called multiple times, skipping');
        return;
    }
    this._initialized.listeners = true;

    try {
        const configListener = vscode.workspace.onDidChangeConfiguration((event) => {
            if (event.affectsConfiguration('myExtension.settings')) {
                this.handleConfigChange();
            }
        });

        this._addDisposable(configListener);
        context.subscriptions.push(configListener);
    } catch (error) {
        console.error('Manager: Failed to setup listeners:', error);
        throw error;
    }
}
```

#### 4. dispose()

**Purpose**: Clean up all resources (commands, listeners, webviews, etc.)

**When**: Called when the extension is deactivated or the manager needs to be cleaned up

**Characteristics**:
- Should dispose all tracked disposables
- Should reset initialization state
- Should handle errors gracefully (don't throw)
- Can be overridden to add custom cleanup logic

**Example**:
```javascript
dispose() {
    // Custom cleanup
    if (this.webviewProvider) {
        try {
            this.webviewProvider.dispose();
        } catch (error) {
            console.error('Manager: Error disposing webview provider:', error);
        }
    }

    // Call parent dispose to clean up tracked disposables
    super.dispose();
}
```

## Creating a New Manager

To create a new manager that follows this pattern:

### 1. Extend BaseManager

```javascript
const { BaseManager } = require('../../core/src/baseManager');

class MyFeatureManager extends BaseManager {
    constructor(context) {
        super(context);
        // Initialize your data managers, services, etc.
        this.dataManager = new MyDataManager(context);
        this.service = new MyService();
    }

    // Implement lifecycle methods...
}
```

### 2. Implement Lifecycle Methods

Implement all three lifecycle methods (`registerViews`, `registerCommands`, `setupListeners`) even if some are no-ops:

```javascript
registerViews(context) {
    if (this._initialized.views) {
        console.warn('MyFeatureManager: registerViews called multiple times, skipping');
        return;
    }
    this._initialized.views = true;
    
    // Register your views...
}

registerCommands(context) {
    if (this._initialized.commands) {
        console.warn('MyFeatureManager: registerCommands called multiple times, skipping');
        return;
    }
    this._initialized.commands = true;
    
    // Register your commands...
}

setupListeners(context) {
    if (this._initialized.listeners) {
        console.warn('MyFeatureManager: setupListeners called multiple times, skipping');
        return;
    }
    this._initialized.listeners = true;
    
    // Setup your listeners (if any)...
}
```

### 3. Track All Disposables

Use `this._addDisposable()` to track everything that needs cleanup:

```javascript
const command = vscode.commands.registerCommand('my.command', () => {});
this._addDisposable(command);
context.subscriptions.push(command);
```

### 4. Override dispose() if Needed

If you have custom cleanup logic (like disposing providers), override dispose():

```javascript
dispose() {
    // Custom cleanup first
    if (this.myProvider) {
        try {
            this.myProvider.dispose();
        } catch (error) {
            console.error('MyFeatureManager: Error disposing provider:', error);
        }
    }
    
    // Then call parent dispose
    super.dispose();
}
```

## Integration with ActivationManager

The ActivationManager orchestrates the lifecycle for all managers:

```javascript
// Phase 1: Register Views (synchronous)
registerViews() {
    this.myManager = new MyFeatureManager(this.context);
    this.myManager.registerViews(this.context);
}

// Phase 2: Initialize Modules (async)
async initializeModules() {
    if (this.myManager) {
        this.myManager.registerCommands(this.context);
        this.myManager.setupListeners(this.context);
    }
}

// Cleanup
dispose() {
    if (this.myManager) {
        try {
            this.myManager.dispose();
        } catch (error) {
            console.error('Error disposing my manager:', error);
        }
    }
}
```

## Testing

All managers should have comprehensive tests covering:

1. **Lifecycle methods exist**: Verify all methods are implemented
2. **Initialization order**: Test that phases work correctly in sequence
3. **Idempotency**: Test that calling methods multiple times doesn't cause issues
4. **Disposal**: Test that dispose() cleans up all resources
5. **Error handling**: Test graceful failure scenarios

Example test structure:

```javascript
test('MyFeatureManager - Lifecycle Pattern', async (t) => {
    await t.test('should implement BaseManager lifecycle methods', async () => {
        const manager = new MyFeatureManager(mockContext);
        assert.equal(typeof manager.registerViews, 'function');
        assert.equal(typeof manager.registerCommands, 'function');
        assert.equal(typeof manager.setupListeners, 'function');
        assert.equal(typeof manager.dispose, 'function');
    });

    await t.test('should prevent duplicate view registration', async () => {
        const manager = new MyFeatureManager(mockContext);
        manager.registerViews(mockContext);
        const firstCount = mockContext.subscriptions.length;
        
        manager.registerViews(mockContext);
        const secondCount = mockContext.subscriptions.length;
        
        assert.equal(firstCount, secondCount);
    });

    await t.test('should dispose all resources', async () => {
        const manager = new MyFeatureManager(mockContext);
        manager.registerViews(mockContext);
        manager.registerCommands(mockContext);
        manager.setupListeners(mockContext);
        
        assert.equal(manager.isFullyInitialized(), true);
        
        manager.dispose();
        
        assert.equal(manager.isFullyInitialized(), false);
    });
});
```

## Existing Implementations

- **CalendarManager** (`calendar/src/calendarManager.js`): Manages calendar tree view, commands, and webview
- **ChatManager** (`chat/src/chatManager.js`): Manages chat webview, commands, and configuration listeners

Refer to these implementations as examples when creating new managers.

## Benefits

1. **Consistency**: All managers follow the same pattern
2. **Predictability**: Clear initialization order
3. **Safety**: Built-in protection against duplicate registration
4. **Clean Disposal**: Automatic cleanup of all tracked resources
5. **Testability**: Easy to write comprehensive tests
6. **Maintainability**: Clear separation of concerns between lifecycle phases
7. **Documentation**: Self-documenting code structure

## Migration Guide

If you have an existing manager that doesn't follow this pattern:

1. Make it extend `BaseManager`
2. Rename/refactor existing methods to match lifecycle:
   - `initialize()` → split into `registerViews()`, `registerCommands()`, `setupListeners()`
   - `registerTreeProvider()` → `registerViews()`
   - Custom command registration → `registerCommands()`
3. Add idempotency checks using `this._initialized` flags
4. Track all disposables with `this._addDisposable()`
5. Update `dispose()` to call `super.dispose()`
6. Update ActivationManager integration
7. Write/update tests to cover new lifecycle
