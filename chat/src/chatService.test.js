/**
 * Chat Service - Test Suite
 * Testing Ollama integration service
 * Using Node.js built-in test runner
 */

process.env.NODE_ENV = 'test';

const test = require('node:test');
const assert = require('node:assert');
const { ChatService } = require('./chatService');

const mockVSCode = {
    workspace: {
        getConfiguration: (section) => ({
            get: (key, defaultValue) => {
                if (section === 'tsiheader.chat') {
                    const config = {
                        ollamaUrl: 'http://localhost:11434',
                        defaultModel: 'mistral',
                        temperature: 0.7,
                        maxTokens: 2048,
                        historyLimit: 10
                    };
                    return config[key] !== undefined ? config[key] : defaultValue;
                }
                return defaultValue;
            }
        })
    }
};

test('ChatService - Initialization', async (t) => {
    await t.test('should initialize with default configuration', () => {
        const service = new ChatService(mockVSCode);

        assert.equal(service.config.url, 'http://localhost:11434');
        assert.equal(service.config.model, 'mistral');
        assert.equal(service.config.temperature, 0.7);
        assert.equal(service.config.maxTokens, 2048);
        assert.equal(service.config.historyLimit, 10);
    });

    await t.test('should have default timeout', () => {
        const service = new ChatService(mockVSCode);

        assert.equal(service.timeout, 30000);
    });
});

test('ChatService - Configuration', async (t) => {
    await t.test('should get current configuration', () => {
        const service = new ChatService(mockVSCode);
        const config = service.getConfig();

        assert.equal(config.url, 'http://localhost:11434');
        assert.equal(config.model, 'mistral');
        assert.equal(config.temperature, 0.7);
    });

    await t.test('should refresh configuration', () => {
        const customMockVSCode = {
            workspace: {
                getConfiguration: (section) => ({
                    get: (key, defaultValue) => {
                        if (section === 'tsiheader.chat') {
                            const config = {
                                ollamaUrl: 'http://custom-url:11434',
                                defaultModel: 'llama2',
                                temperature: 0.8,
                                maxTokens: 4096,
                                historyLimit: 20
                            };
                            return config[key] !== undefined ? config[key] : defaultValue;
                        }
                        return defaultValue;
                    }
                })
            }
        };

        const service = new ChatService(mockVSCode);
        service.vscode = customMockVSCode;
        service.refreshConfig();

        assert.equal(service.config.url, 'http://custom-url:11434');
        assert.equal(service.config.model, 'llama2');
        assert.equal(service.config.temperature, 0.8);
        assert.equal(service.config.maxTokens, 4096);
        assert.equal(service.config.historyLimit, 20);
    });
});

test('ChatService - Error Normalization', async (t) => {
    await t.test('should normalize ECONNREFUSED error', () => {
        const service = new ChatService(mockVSCode);
        const error = new Error('Connection refused');
        error.code = 'ECONNREFUSED';

        const normalized = service.normalizeError(error);

        assert.equal(normalized.code, 'OLLAMA_NOT_RUNNING');
        assert(normalized.message.includes('Cannot connect to Ollama'));
        assert(normalized.detail.includes('Connection refused'));
    });

    await t.test('should normalize ETIMEDOUT error', () => {
        const service = new ChatService(mockVSCode);
        const error = new Error('Request timeout');
        error.code = 'ETIMEDOUT';

        const normalized = service.normalizeError(error);

        assert.equal(normalized.code, 'TIMEOUT');
        assert(normalized.message.includes('timed out'));
    });

    await t.test('should normalize timeout by message', () => {
        const service = new ChatService(mockVSCode);
        const error = new Error('Request timeout exceeded');

        const normalized = service.normalizeError(error);

        assert.equal(normalized.code, 'TIMEOUT');
        assert(normalized.message.includes('timed out'));
    });

    await t.test('should normalize 404 error', () => {
        const service = new ChatService(mockVSCode);
        const error = new Error('HTTP 404: Not Found');

        const normalized = service.normalizeError(error);

        assert.equal(normalized.code, 'MODEL_NOT_FOUND');
        assert(normalized.message.includes('Model not found'));
        assert(normalized.detail.includes('mistral'));
    });

    await t.test('should normalize ENOTFOUND error', () => {
        const service = new ChatService(mockVSCode);
        const error = new Error('getaddrinfo ENOTFOUND');

        const normalized = service.normalizeError(error);

        assert.equal(normalized.code, 'INVALID_URL');
        assert(normalized.message.includes('Invalid Ollama URL'));
    });

    await t.test('should normalize abort error', () => {
        const service = new ChatService(mockVSCode);
        const error = new Error('The user aborted a request.');
        error.name = 'AbortError';

        const normalized = service.normalizeError(error);

        assert.equal(normalized.code, 'REQUEST_CANCELLED');
        assert(normalized.message.includes('cancelled'));
    });

    await t.test('should normalize unknown error', () => {
        const service = new ChatService(mockVSCode);
        const error = new Error('Some unexpected error');

        const normalized = service.normalizeError(error);

        assert.equal(normalized.code, 'UNKNOWN_ERROR');
        assert(normalized.message.includes('error occurred'));
        assert.equal(normalized.detail, 'Some unexpected error');
    });
});

test('ChatService - API Methods Structure', async (t) => {
    await t.test('should have listModels method', () => {
        const service = new ChatService(mockVSCode);

        assert.equal(typeof service.listModels, 'function');
    });

    await t.test('should have sendMessage method', () => {
        const service = new ChatService(mockVSCode);

        assert.equal(typeof service.sendMessage, 'function');
    });

    await t.test('should have sendStreamingMessage method', () => {
        const service = new ChatService(mockVSCode);

        assert.equal(typeof service.sendStreamingMessage, 'function');
    });

    await t.test('should have checkConnection method', () => {
        const service = new ChatService(mockVSCode);

        assert.equal(typeof service.checkConnection, 'function');
    });
});

test('ChatService - Request Structure', async (t) => {
    await t.test('listModels should return failure when Ollama not running', { timeout: 10000 }, async () => {
        const service = new ChatService(mockVSCode);
        service.timeout = 2000;

        const result = await service.listModels();

        assert.equal(result.success, false);
        assert(result.error);
        assert(result.error.message);
    });

    await t.test('sendMessage should return failure when Ollama not running', { timeout: 10000 }, async () => {
        const service = new ChatService(mockVSCode);
        service.timeout = 2000;

        const messages = [{ role: 'user', content: 'Hello' }];
        const result = await service.sendMessage(messages);

        assert.equal(result.success, false);
        assert(result.error);
        assert(result.error.message);
    });

    await t.test('checkConnection should return failure when Ollama not running', { timeout: 10000 }, async () => {
        const service = new ChatService(mockVSCode);

        const result = await service.checkConnection();

        assert.equal(result.success, false);
        assert(result.error);
    });
});
