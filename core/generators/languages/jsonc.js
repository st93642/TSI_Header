/**
 * JSONC Language Code Base Generator
 * Generates JSONC (JSON with comments) code base/boilerplate code
 */

/**
 * Generates JSONC code base
 * @returns {string} JSONC code base template
 */
function generateJSONCCodeBase() {
    return `\n{\n    // JSONC (JSON with Comments) Configuration File\n    // This file demonstrates JSONC syntax with comments\n\n    // Application settings\n    "app": {\n        "name": "My Application",\n        "version": "1.0.0",\n        /*\n         * Multi-line comment example\n         * for JSONC configuration\n         */\n        "debug": true,\n        "port": 3000\n    },\n\n    // Database configuration\n    "database": {\n        "host": "localhost",\n        "port": 5432,\n        // Use environment variable for password\n        "password": "\${DB_PASSWORD}",\n        "ssl": false\n    },\n\n    /*\n     * Feature flags\n     * Enable/disable application features\n     */\n    "features": {\n        "authentication": true,\n        "logging": true,\n        "metrics": false  // Disabled for now\n    },\n\n    // Array of allowed origins\n    "cors": {\n        "origins": [\n            "http://localhost:3000",\n            "https://myapp.com"\n        ]\n    }\n}\n`;
}

module.exports = {
    generateJSONCCodeBase
};