# GlobalStateStore - Storage Helper Documentation

## Overview

`GlobalStateStore` is a centralized, reusable storage utility for managing VS Code's `globalState` with support for:
- Default values
- Versioning and migrations
- Namespaced keys
- Nested property access
- Export/import functionality

## Location

`core/src/storage/globalStateStore.js`

## Basic Usage

### Creating a Store

```javascript
const { GlobalStateStore } = require('../../core/src/storage/globalStateStore');

const store = new GlobalStateStore(context, {
    namespace: 'myFeature',
    version: '1.0.0',
    defaults: {
        myKey: { count: 0 }
    },
    migrations: {
        '2.0.0': async (data) => {
            // Transform old format to new
            return { ...data, newField: 'value' };
        }
    }
});
```

### Get State

```javascript
// Get with default from options
const data = await store.getState('myKey');

// Get with inline default
const data = await store.getState('myKey', { count: 0 });

// Get nested property
const count = await store.getProperty('myKey', 'count');

// Get nested with default
const count = await store.getProperty('myKey', 'nonexistent.path', 5);
```

### Update State

```javascript
// Update entire state
await store.updateState('myKey', { count: 1, active: true });

// Update nested property
await store.updateProperty('myKey', 'count', 1);
await store.updateProperty('myKey', 'settings.theme', 'dark');
```

### Clear State

```javascript
// Clear specific key
await store.clear('myKey');
```

### Export/Import

```javascript
// Export for backup
const data = await store.exportData('myKey');

// Import from backup
await store.importData('myKey', importedData);
```

## Options

### namespace
- **Type:** string
- **Default:** ''
- **Description:** Prefix for all keys. Useful for feature-specific storage without conflicts.

### version
- **Type:** string
- **Default:** '1.0.0'
- **Description:** Current schema version. Used for migrations.

### defaults
- **Type:** object
- **Default:** {}
- **Description:** Default values keyed by storage key. Used when no data exists.

### migrations
- **Type:** object
- **Default:** {}
- **Description:** Version-to-migration-function mapping. Functions transform old data to new schema.

## Integration Examples

### ChatDataManager

```javascript
class ChatDataManager {
    constructor(context) {
        this.store = new GlobalStateStore(context, {
            namespace: '',
            version: '1.0.0',
            defaults: {
                'tsi.chat.data': {
                    conversations: [],
                    activeConversationId: null,
                    version: '1.0.0'
                }
            }
        });
    }

    async getData() {
        return await this.store.getState('tsi.chat.data', {
            conversations: [],
            activeConversationId: null,
            version: '1.0.0'
        });
    }

    async saveData(data) {
        await this.store.updateState('tsi.chat.data', data);
    }
}
```

### StudyModeExtension (Namespaced)

```javascript
class StudyModeExtension {
    constructor(vscode, context) {
        this.store = new GlobalStateStore(context, {
            namespace: 'studyMode',
            version: '1.0.0',
            defaults: {
                state: {
                    currentPhase: 'stopped',
                    sessionLog: []
                }
            }
        });
    }

    async loadPersistedState() {
        const persistedState = await this.store.getState('state');
        // Use persistedState...
    }

    async savePersistedState() {
        const stateToSave = { /* ... */ };
        await this.store.updateState('state', stateToSave);
    }
}
```

## Migration Pattern

Migrations run automatically when data is retrieved via `getState()` if the stored version differs from the target version.

```javascript
const migrations = {
    '1.1.0': async (data) => {
        // Update from 1.0.0 to 1.1.0
        return { ...data, newField: null };
    },
    '2.0.0': async (data) => {
        // Update from 1.1.0 to 2.0.0
        return { ...data, oldField: undefined };
    }
};

const store = new GlobalStateStore(context, {
    version: '2.0.0',
    migrations
});
```

## Backward Compatibility

When migrating existing managers to GlobalStateStore, use the **same storage keys** to ensure existing persisted data is preserved:

- ChatDataManager: Uses `'tsi.chat.data'` (unchanged)
- CalendarDataManager: Uses `'tsi.calendar.data'` (unchanged)
- StudyModeExtension: Uses namespace `'studyMode'` + key `'state'` (equates to `'studyMode.state'`)

## Testing

Mock context for tests:

```javascript
const createMockContext = () => {
    const storage = new Map();
    return {
        globalState: {
            get: (key, defaultValue) => {
                return storage.has(key) ? storage.get(key) : defaultValue;
            },
            update: async (key, value) => {
                if (value === null || value === undefined) {
                    storage.delete(key);
                } else {
                    storage.set(key, value);
                }
            }
        }
    };
};

const mockContext = createMockContext();
const store = new GlobalStateStore(mockContext);
```

## See Also

- `core/src/storage/globalStateStore.test.js` - Comprehensive test suite
- `chat/src/chatDataManager.js` - Real-world usage example
- `calendar/src/calendarDataManager.js` - Real-world usage example
- `core/src/studyModeExtension.js` - Namespaced usage example
