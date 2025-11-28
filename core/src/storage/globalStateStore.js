/**
 * Global State Store Helper
 * Provides KISS utilities for managing VS Code globalState with versioning, migrations, and namespacing
 */

class GlobalStateStore {
    constructor(context, options = {}) {
        this.context = context;
        this.namespace = options.namespace || '';
        this.version = options.version || '1.0.0';
        this.defaults = options.defaults || {};
        this.migrations = options.migrations || {};
    }

    /**
     * Get the full storage key with optional namespace prefix
     */
    _getStorageKey(baseKey) {
        if (this.namespace) {
            return `${this.namespace}.${baseKey}`;
        }
        return baseKey;
    }

    /**
     * Get state from storage with defaults and migrations
     */
    async getState(baseKey, defaultValue = null) {
        const storageKey = this._getStorageKey(baseKey);
        
        let data = this.context.globalState.get(storageKey);

        // Return default if no data exists
        if (data === null || data === undefined) {
            if (defaultValue !== null && defaultValue !== undefined) {
                return defaultValue;
            }
            return this.defaults.hasOwnProperty(baseKey) ? this.defaults[baseKey] : null;
        }

        // Apply migrations if needed
        if (data.version !== undefined) {
            data = await this._applyMigrations(data, storageKey);
        }

        return data;
    }

    /**
     * Update state in storage
     */
    async updateState(baseKey, data) {
        const storageKey = this._getStorageKey(baseKey);
        
        if (data === null || data === undefined) {
            await this.context.globalState.update(storageKey, null);
            return null;
        }

        // Ensure version is set
        if (data.version === undefined && this.version) {
            data.version = this.version;
        }

        await this.context.globalState.update(storageKey, data);
        return data;
    }

    /**
     * Get a nested property from state
     */
    async getProperty(baseKey, propertyPath, defaultValue = null) {
        const data = await this.getState(baseKey, {});
        return this._getNestedProperty(data, propertyPath, defaultValue);
    }

    /**
     * Update a nested property in state
     */
    async updateProperty(baseKey, propertyPath, value) {
        const data = await this.getState(baseKey, {});
        this._setNestedProperty(data, propertyPath, value);
        return await this.updateState(baseKey, data);
    }

    /**
     * Clear a specific key
     */
    async clear(baseKey) {
        const storageKey = this._getStorageKey(baseKey);
        await this.context.globalState.update(storageKey, null);
    }

    /**
     * Clear all namespaced keys
     */
    async clearAll() {
        // Get all keys from globalState (this is a limitation, but works for our purposes)
        // In practice, we rely on the caller to specify what to clear
        // This is a safety mechanism when used with a namespace
        if (this.namespace) {
            // In a real implementation, we'd iterate over all keys,
            // but VS Code doesn't expose that. We document that callers should track their own keys.
            // For now, this is a no-op unless caller provides the keys to clear.
        }
    }

    /**
     * Get nested property helper
     */
    _getNestedProperty(obj, path, defaultValue) {
        if (!path) return obj;
        
        const keys = path.split('.');
        let result = obj;

        for (const key of keys) {
            if (result && typeof result === 'object' && key in result) {
                result = result[key];
            } else {
                return defaultValue;
            }
        }

        return result;
    }

    /**
     * Set nested property helper
     */
    _setNestedProperty(obj, path, value) {
        if (!path) return;

        const keys = path.split('.');
        let current = obj;

        for (let i = 0; i < keys.length - 1; i++) {
            const key = keys[i];
            if (!(key in current) || typeof current[key] !== 'object') {
                current[key] = {};
            }
            current = current[key];
        }

        current[keys[keys.length - 1]] = value;
    }

    /**
     * Apply migrations to data
     */
    async _applyMigrations(data, storageKey) {
        const currentVersion = data.version || '1.0.0';

        if (currentVersion === this.version) {
            return data;
        }

        // Apply migration functions in order
        const versions = Object.keys(this.migrations).sort();
        let migrated = data;

        for (const targetVersion of versions) {
            // Only apply migrations that are newer than current version
            if (this._compareVersions(currentVersion, targetVersion) < 0) {
                const migrationFn = this.migrations[targetVersion];
                if (typeof migrationFn === 'function') {
                    migrated = await migrationFn(migrated);
                    migrated.version = targetVersion;
                }
            }
        }

        // Save migrated data
        if (migrated.version !== currentVersion) {
            await this.context.globalState.update(storageKey, migrated);
        }

        return migrated;
    }

    /**
     * Compare semantic versions
     * Returns: -1 if v1 < v2, 0 if equal, 1 if v1 > v2
     */
    _compareVersions(v1, v2) {
        const parts1 = (v1 || '0.0.0').split('.').map(Number);
        const parts2 = (v2 || '0.0.0').split('.').map(Number);

        for (let i = 0; i < Math.max(parts1.length, parts2.length); i++) {
            const part1 = parts1[i] || 0;
            const part2 = parts2[i] || 0;

            if (part1 < part2) return -1;
            if (part1 > part2) return 1;
        }

        return 0;
    }

    /**
     * Export all data for a key (useful for export/import)
     */
    async exportData(baseKey) {
        return await this.getState(baseKey);
    }

    /**
     * Import data for a key (useful for import operations)
     */
    async importData(baseKey, importedData) {
        if (!importedData || typeof importedData !== 'object') {
            throw new Error('Invalid import data format');
        }
        
        return await this.updateState(baseKey, importedData);
    }
}

module.exports = { GlobalStateStore };
