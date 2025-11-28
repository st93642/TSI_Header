/**
 * Shared global state and extension context mocks for Node-based tests.
 */

const createMockGlobalState = (initialState = {}) => {
    const storage = new Map(Object.entries(initialState));

    return {
        _storage: storage,
        get(key, defaultValue) {
            return storage.has(key) ? storage.get(key) : defaultValue;
        },
        async update(key, value) {
            if (value === undefined || value === null) {
                storage.delete(key);
            } else {
                storage.set(key, value);
            }
        },
        async clear() {
            storage.clear();
        },
        keys() {
            return Array.from(storage.keys());
        }
    };
};

const createMockExtensionContext = (options = {}) => {
    const {
        globalState,
        initialState,
        subscriptions,
        extensionPath = '/test/path',
        extensionUri = { fsPath: '/test/path' },
        storageUri = { fsPath: '/test/storage' },
        workspaceState,
        ...rest
    } = options;

    return {
        subscriptions: subscriptions ? [...subscriptions] : [],
        extensionPath,
        extensionUri,
        storageUri,
        globalState: globalState || createMockGlobalState(initialState),
        workspaceState: workspaceState || {
            get: () => undefined,
            update: () => Promise.resolve()
        },
        ...rest
    };
};

module.exports = {
    createMockGlobalState,
    createMockExtensionContext
};
