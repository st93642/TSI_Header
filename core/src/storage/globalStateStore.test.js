/**
 * Global State Store - Test Suite
 * Test-Driven Development for GlobalStateStore utility
 * Using Node.js built-in test runner
 */

const test = require('node:test');
const assert = require('node:assert');
const { GlobalStateStore } = require('./globalStateStore');
const { createMockExtensionContext } = require('../../../test/utils/globalStateMock');

const createMockContext = (initialState = {}) => createMockExtensionContext({
    initialState,
    extensionPath: '/test/path'
});

test('GlobalStateStore - Basic Operations', async (t) => {
    await t.test('should initialize with default options', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        assert.ok(store.context);
        assert.equal(store.namespace, '');
        assert.equal(store.version, '1.0.0');
    });

    await t.test('should initialize with custom options', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext, {
            namespace: 'test',
            version: '2.0.0',
            defaults: { count: 0 }
        });
        
        assert.equal(store.namespace, 'test');
        assert.equal(store.version, '2.0.0');
        assert.deepEqual(store.defaults, { count: 0 });
    });
});

test('GlobalStateStore - Get/Update State', async (t) => {
    await t.test('should return null for missing key with no default', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        const data = await store.getState('missing');
        assert.equal(data, null);
    });

    await t.test('should return default value for missing key', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        const data = await store.getState('missing', { count: 0 });
        assert.deepEqual(data, { count: 0 });
    });

    await t.test('should use defaults from options', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext, {
            defaults: { myCount: 0 }
        });
        
        const data = await store.getState('myCount');
        assert.equal(data, 0);
    });

    await t.test('should save and retrieve state', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        const testData = { name: 'test', value: 123 };
        await store.updateState('test-key', testData);
        
        const retrieved = await store.getState('test-key');
        assert.deepEqual(retrieved, testData);
        assert.equal(retrieved.version, '1.0.0');
    });

    await t.test('should update existing state', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        await store.updateState('counter', { count: 1 });
        let data = await store.getState('counter');
        assert.equal(data.count, 1);
        
        data.count = 2;
        await store.updateState('counter', data);
        data = await store.getState('counter');
        assert.equal(data.count, 2);
    });

    await t.test('should clear state', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        await store.updateState('test-key', { value: 'test' });
        await store.clear('test-key');
        
        const data = await store.getState('test-key');
        assert.equal(data, null);
    });

    await t.test('should handle null values', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        await store.updateState('test-key', null);
        const data = await store.getState('test-key');
        assert.equal(data, null);
    });
});

test('GlobalStateStore - Namespacing', async (t) => {
    await t.test('should prefix keys with namespace', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext, { namespace: 'chat' });
        
        await store.updateState('data', { conversations: [] });
        
        // The key should be prefixed with namespace
        const allData = await mockContext.globalState.get('chat.data');
        assert.ok(allData);
        assert(Array.isArray(allData.conversations));
    });

    await t.test('should not prefix keys without namespace', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext, { namespace: '' });
        
        await store.updateState('data', { value: 'test' });
        
        // The key should not have a prefix
        const allData = await mockContext.globalState.get('data');
        assert.ok(allData);
        assert.equal(allData.value, 'test');
    });

    await t.test('multiple namespaces should not interfere', async () => {
        const mockContext = createMockContext();
        const store1 = new GlobalStateStore(mockContext, { namespace: 'chat' });
        const store2 = new GlobalStateStore(mockContext, { namespace: 'calendar' });
        
        await store1.updateState('data', { type: 'chat' });
        await store2.updateState('data', { type: 'calendar' });
        
        const data1 = await store1.getState('data');
        const data2 = await store2.getState('data');
        
        assert.equal(data1.type, 'chat');
        assert.equal(data2.type, 'calendar');
    });
});

test('GlobalStateStore - Nested Properties', async (t) => {
    await t.test('should get nested property', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        await store.updateState('app', {
            user: { name: 'John', age: 30 }
        });
        
        const name = await store.getProperty('app', 'user.name');
        assert.equal(name, 'John');
    });

    await t.test('should get nested property with default', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        await store.updateState('app', {
            user: { name: 'John' }
        });
        
        const age = await store.getProperty('app', 'user.age', 25);
        assert.equal(age, 25);
    });

    await t.test('should update nested property', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        await store.updateState('app', {
            user: { name: 'John', age: 30 }
        });
        
        await store.updateProperty('app', 'user.age', 31);
        
        const data = await store.getState('app');
        assert.equal(data.user.age, 31);
    });

    await t.test('should create nested properties if missing', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        await store.updateState('app', {});
        await store.updateProperty('app', 'settings.theme.dark', true);
        
        const data = await store.getState('app');
        assert.equal(data.settings.theme.dark, true);
    });
});

test('GlobalStateStore - Migrations', async (t) => {
    await t.test('should apply migration when version mismatch', async () => {
        const mockContext = createMockContext();
        const migrations = {
            '2.0.0': async (data) => {
                return {
                    ...data,
                    migrated: true
                };
            }
        };
        
        const store = new GlobalStateStore(mockContext, {
            version: '2.0.0',
            migrations
        });
        
        // Store data with old version
        await mockContext.globalState.update('data', {
            version: '1.0.0',
            value: 'test'
        });
        
        const data = await store.getState('data');
        assert.equal(data.version, '2.0.0');
        assert.equal(data.migrated, true);
        assert.equal(data.value, 'test');
    });

    await t.test('should apply multiple migrations in order', async () => {
        const mockContext = createMockContext();
        const migrations = {
            '1.1.0': async (data) => {
                data.step1 = true;
                return data;
            },
            '2.0.0': async (data) => {
                data.step2 = true;
                return data;
            }
        };
        
        const store = new GlobalStateStore(mockContext, {
            version: '2.0.0',
            migrations
        });
        
        // Store data with old version
        await mockContext.globalState.update('data', {
            version: '1.0.0',
            value: 'test'
        });
        
        const data = await store.getState('data');
        assert.equal(data.version, '2.0.0');
        assert.equal(data.step1, true);
        assert.equal(data.step2, true);
    });

    await t.test('should not apply migrations if already at target version', async () => {
        const mockContext = createMockContext();
        let migrationCalled = false;
        
        const migrations = {
            '2.0.0': async (data) => {
                migrationCalled = true;
                return data;
            }
        };
        
        const store = new GlobalStateStore(mockContext, {
            version: '2.0.0',
            migrations
        });
        
        // Store data with current version
        await mockContext.globalState.update('data', {
            version: '2.0.0',
            value: 'test'
        });
        
        await store.getState('data');
        assert.equal(migrationCalled, false);
    });

    await t.test('should skip migrations older than current version', async () => {
        const mockContext = createMockContext();
        const callLog = [];
        
        const migrations = {
            '1.1.0': async (data) => {
                callLog.push('1.1.0');
                return data;
            },
            '2.0.0': async (data) => {
                callLog.push('2.0.0');
                return data;
            }
        };
        
        const store = new GlobalStateStore(mockContext, {
            version: '2.1.0',
            migrations
        });
        
        // Store data with version 1.5.0
        await mockContext.globalState.update('data', {
            version: '1.5.0',
            value: 'test'
        });
        
        await store.getState('data');
        assert.deepEqual(callLog, ['2.0.0']);
    });
});

test('GlobalStateStore - Export/Import', async (t) => {
    await t.test('should export data', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        const testData = { conversations: [], version: '1.0.0' };
        await store.updateState('data', testData);
        
        const exported = await store.exportData('data');
        assert.deepEqual(exported, testData);
    });

    await t.test('should import data', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        const importData = { conversations: [], version: '1.0.0' };
        await store.importData('data', importData);
        
        const retrieved = await store.getState('data');
        assert.deepEqual(retrieved, importData);
    });

    await t.test('should throw on invalid import data', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        try {
            await store.importData('data', 'invalid');
            assert.fail('Should have thrown');
        } catch (error) {
            assert(error.message.includes('Invalid import data format'));
        }
    });

    await t.test('should throw on null import data', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        try {
            await store.importData('data', null);
            assert.fail('Should have thrown');
        } catch (error) {
            assert(error.message.includes('Invalid import data format'));
        }
    });
});

test('GlobalStateStore - Version Comparison', async (t) => {
    await t.test('should compare versions correctly', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        assert.equal(store._compareVersions('1.0.0', '2.0.0'), -1);
        assert.equal(store._compareVersions('2.0.0', '1.0.0'), 1);
        assert.equal(store._compareVersions('1.0.0', '1.0.0'), 0);
        assert.equal(store._compareVersions('1.1.0', '1.2.0'), -1);
        assert.equal(store._compareVersions('1.0.1', '1.0.0'), 1);
    });

    await t.test('should handle missing version components', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext);
        
        assert.equal(store._compareVersions('1', '1.0.0'), 0);
        assert.equal(store._compareVersions('1.0', '1.0.0'), 0);
        assert.equal(store._compareVersions('1.1', '1.0.0'), 1);
    });
});

test('GlobalStateStore - Integration', async (t) => {
    await t.test('should work with nested data and migrations', async () => {
        const mockContext = createMockContext();
        const migrations = {
            '2.0.0': async (data) => {
                // Migrate old structure to new structure
                return {
                    version: '2.0.0',
                    items: data.items || [],
                    metadata: {
                        count: (data.items || []).length,
                        lastUpdated: new Date().toISOString()
                    }
                };
            }
        };
        
        const store = new GlobalStateStore(mockContext, {
            namespace: 'test',
            version: '2.0.0',
            migrations
        });
        
        // Initialize with old format
        await mockContext.globalState.update('test.config', {
            version: '1.0.0',
            items: ['a', 'b', 'c']
        });
        
        // Get data - should trigger migration
        const data = await store.getState('config');
        assert.equal(data.version, '2.0.0');
        assert.deepEqual(data.items, ['a', 'b', 'c']);
        assert.equal(data.metadata.count, 3);
    });

    await t.test('should handle complex nested updates', async () => {
        const mockContext = createMockContext();
        const store = new GlobalStateStore(mockContext, { namespace: 'app' });
        
        await store.updateState('state', {
            user: { name: 'John', settings: { theme: 'dark' } },
            cache: []
        });
        
        await store.updateProperty('state', 'user.settings.theme', 'light');
        await store.updateProperty('state', 'cache', ['item1', 'item2']);
        
        const user = await store.getProperty('state', 'user.settings.theme');
        const cache = await store.getProperty('state', 'cache');
        
        assert.equal(user, 'light');
        assert.deepEqual(cache, ['item1', 'item2']);
    });
});
