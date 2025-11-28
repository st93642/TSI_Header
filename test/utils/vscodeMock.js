/**
 * Shared VS Code mock utilities for Node-based test suites.
 * Provides a configurable API surface that mirrors the pieces of the
 * vscode module used throughout the extension.
 */

const isPlainObject = (value) => {
    return value !== null && typeof value === 'object' && !Array.isArray(value);
};

const mergeDeep = (target, source) => {
    if (!isPlainObject(source)) {
        return target;
    }

    const output = { ...target };

    for (const [key, value] of Object.entries(source)) {
        if (isPlainObject(value)) {
            output[key] = mergeDeep(output[key] || {}, value);
        } else {
            output[key] = value;
        }
    }

    return output;
};

const cloneConfigSection = (sectionConfig) => {
    if (!isPlainObject(sectionConfig) || typeof sectionConfig.get === 'function') {
        return sectionConfig;
    }

    return { ...sectionConfig };
};

const createVSCodeMock = (options = {}) => {
    const { configuration = {}, ...overrides } = options;
    const configStore = Object.fromEntries(
        Object.entries(configuration).map(([section, values]) => [section, cloneConfigSection(values)])
    );

    const registeredTreeProviders = new Map();
    const registeredWebviewProviders = new Map();
    const registeredCommands = new Map();

    const DefaultStatusBarItem = overrides.StatusBarItem || class {
        constructor(alignment = 1, priority = 0) {
            this.alignment = alignment;
            this.priority = priority;
            this.text = '';
            this.tooltip = '';
            this.command = '';
        }
        show() {}
        hide() {}
        dispose() {}
    };

    const mock = {
        window: {
            registerTreeDataProvider: (viewId, provider) => {
                registeredTreeProviders.set(viewId, provider);
                return { dispose: () => registeredTreeProviders.delete(viewId) };
            },
            registerWebviewViewProvider: (viewId, provider) => {
                registeredWebviewProviders.set(viewId, provider);
                return { dispose: () => registeredWebviewProviders.delete(viewId) };
            },
            showInformationMessage: () => Promise.resolve(undefined),
            showWarningMessage: () => Promise.resolve(undefined),
            showErrorMessage: () => Promise.resolve(undefined),
            createOutputChannel: (name) => ({
                name,
                append: () => {},
                appendLine: () => {},
                clear: () => {},
                show: () => {},
                hide: () => {},
                dispose: () => {}
            }),
            createStatusBarItem: (alignment, priority) => new DefaultStatusBarItem(alignment, priority),
            onDidChangeWindowState: () => ({ dispose: () => {} })
        },
        commands: {
            registerCommand: (commandId, handler) => {
                registeredCommands.set(commandId, handler);
                return { dispose: () => registeredCommands.delete(commandId) };
            },
            executeCommand: () => Promise.resolve(undefined)
        },
        workspace: {
            getConfiguration: (section) => ({
                get: (key, defaultValue) => {
                    const sectionConfig = configStore[section];
                    if (!sectionConfig) {
                        return defaultValue;
                    }
                    if (typeof sectionConfig.get === 'function') {
                        return sectionConfig.get(key, defaultValue);
                    }
                    if (Object.prototype.hasOwnProperty.call(sectionConfig, key)) {
                        return sectionConfig[key];
                    }
                    return defaultValue;
                },
                update: (key, value) => {
                    if (!configStore[section] || typeof configStore[section].update === 'function') {
                        if (configStore[section] && typeof configStore[section].update === 'function') {
                            return configStore[section].update(key, value);
                        }
                        configStore[section] = {};
                    }
                    configStore[section][key] = value;
                    return Promise.resolve();
                }
            }),
            onDidChangeConfiguration: () => ({ dispose: () => {} })
        },
        env: {
            openExternal: () => Promise.resolve(true)
        },
        StatusBarAlignment: { Left: 1, Right: 2 },
        StatusBarItem: DefaultStatusBarItem,
        _getRegisteredCommands: () => registeredCommands,
        _getRegisteredTreeProviders: () => registeredTreeProviders,
        _getRegisteredWebviewProviders: () => registeredWebviewProviders
    };

    return mergeDeep(mock, overrides);
};

module.exports = {
    createVSCodeMock
};
