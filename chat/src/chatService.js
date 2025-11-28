/**
 * Chat Service
 * Handles HTTP communication with Ollama server
 */

// Use native fetch (available in Node.js 18+) or fallback to node-fetch
let fetch;
try {
    fetch = globalThis.fetch;
    if (!fetch) {
        fetch = require('node-fetch');
    }
} catch (e) {
    // If node-fetch is not available, we'll use native fetch
    fetch = globalThis.fetch;
}

class ChatService {
    constructor(vscode) {
        this.vscode = vscode;
        this.config = this.loadConfig();
        this.timeout = 180000; // 3 minutes default timeout for local models
    }

    /**
     * Load configuration from VS Code settings
     */
    loadConfig() {
        const config = this.vscode.workspace.getConfiguration('tsiheader.chat');
        return {
            url: config.get('ollamaUrl', 'http://localhost:11434'),
            model: config.get('defaultModel', 'mistral'),
            temperature: config.get('temperature', 0.7),
            maxTokens: config.get('maxTokens', 2048),
            historyLimit: config.get('historyLimit', 10)
        };
    }

    /**
     * Refresh configuration from VS Code settings
     */
    refreshConfig() {
        this.config = this.loadConfig();
    }

    /**
     * Get current configuration
     */
    getConfig() {
        return { ...this.config };
    }

    /**
     * Normalize error messages for friendly display
     */
    normalizeError(error) {
        if (error.code === 'ECONNREFUSED') {
            return {
                message: 'Cannot connect to Ollama. Please ensure Ollama is running.',
                detail: `Connection refused at ${this.config.url}. Install and start Ollama from https://ollama.com/`,
                code: 'OLLAMA_NOT_RUNNING'
            };
        }

        if (error.code === 'ETIMEDOUT' || error.message.includes('timeout')) {
            return {
                message: 'Request to Ollama timed out.',
                detail: 'The request took too long to complete. Try again or check your Ollama server.',
                code: 'TIMEOUT'
            };
        }

        if (error.name === 'AbortError') {
            return {
                message: 'Request cancelled.',
                detail: 'The request was cancelled before it completed.',
                code: 'REQUEST_CANCELLED'
            };
        }

        if (error.message.includes('404')) {
            return {
                message: 'Model not found.',
                detail: `The model "${this.config.model}" is not available. Pull it using: ollama pull ${this.config.model}`,
                code: 'MODEL_NOT_FOUND'
            };
        }

        if (error.message.includes('ENOTFOUND')) {
            return {
                message: 'Invalid Ollama URL.',
                detail: `Cannot resolve hostname: ${this.config.url}`,
                code: 'INVALID_URL'
            };
        }

        return {
            message: 'An error occurred while communicating with Ollama.',
            detail: error.message || 'Unknown error',
            code: 'UNKNOWN_ERROR'
        };
    }

    /**
     * List available models from Ollama
     */
    async listModels() {
        try {
            const controller = new AbortController();
            const timeoutId = setTimeout(() => controller.abort(), this.timeout);

            const response = await fetch(`${this.config.url}/api/tags`, {
                method: 'GET',
                headers: {
                    'Content-Type': 'application/json'
                },
                signal: controller.signal
            });

            clearTimeout(timeoutId);

            if (!response.ok) {
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            const data = await response.json();
            return {
                success: true,
                models: data.models || []
            };
        } catch (error) {
            const normalized = this.normalizeError(error);
            return {
                success: false,
                error: normalized
            };
        }
    }

    /**
     * Send a chat message to Ollama
     */
    async sendMessage(messages, model = null, options = {}) {
        const selectedModel = model || this.config.model;
        const abortController = options.abortController || new AbortController();
        const timeoutMs = typeof options.timeout === 'number' ? options.timeout : this.timeout;
        let timeoutId;

        try {
            if (timeoutMs > 0) {
                timeoutId = setTimeout(() => {
                    if (!abortController.signal.aborted) {
                        abortController.abort();
                    }
                }, timeoutMs);
            }

            const requestBody = {
                model: selectedModel,
                messages: messages,
                stream: false,
                options: {
                    temperature: options.temperature ?? this.config.temperature,
                    num_predict: options.maxTokens || this.config.maxTokens
                }
            };

            const response = await fetch(`${this.config.url}/api/chat`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(requestBody),
                signal: abortController.signal
            });

            if (!response.ok) {
                if (response.status === 404) {
                    throw new Error('404');
                }
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            const data = await response.json();

            return {
                success: true,
                message: data.message,
                model: data.model,
                done: data.done,
                context: data.context
            };
        } catch (error) {
            const normalized = this.normalizeError(error);
            return {
                success: false,
                error: normalized
            };
        } finally {
            if (timeoutId) {
                clearTimeout(timeoutId);
            }
        }
    }

    /**
     * Send a streaming chat message to Ollama (for future implementation)
     */
    async sendStreamingMessage(messages, model = null, onChunk, options = {}) {
        const selectedModel = model || this.config.model;
        const abortController = options.abortController || new AbortController();
        const timeoutMs = typeof options.timeout === 'number' ? options.timeout : this.timeout;
        let timeoutId;

        try {
            if (timeoutMs > 0) {
                timeoutId = setTimeout(() => {
                    if (!abortController.signal.aborted) {
                        abortController.abort();
                    }
                }, timeoutMs);
            }

            const requestBody = {
                model: selectedModel,
                messages: messages,
                stream: true,
                options: {
                    temperature: options.temperature ?? this.config.temperature,
                    num_predict: options.maxTokens || this.config.maxTokens
                }
            };

            const response = await fetch(`${this.config.url}/api/chat`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(requestBody),
                signal: abortController.signal
            });

            if (!response.ok) {
                if (response.status === 404) {
                    throw new Error('404');
                }
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            const reader = response.body;
            let buffer = '';

            for await (const chunk of reader) {
                buffer += chunk.toString();
                const lines = buffer.split('\n');
                buffer = lines.pop() || '';

                for (const line of lines) {
                    if (line.trim()) {
                        try {
                            const data = JSON.parse(line);
                            if (onChunk) {
                                onChunk(data);
                            }
                            if (data.done) {
                                return {
                                    success: true,
                                    done: true
                                };
                            }
                        } catch (e) {
                            console.error('Failed to parse streaming chunk:', e);
                        }
                    }
                }
            }

            return {
                success: true,
                done: true
            };
        } catch (error) {
            const normalized = this.normalizeError(error);
            return {
                success: false,
                error: normalized
            };
        } finally {
            if (timeoutId) {
                clearTimeout(timeoutId);
            }
        }
    }

    /**
     * Check if Ollama is available
     */
    async checkConnection() {
        try {
            const controller = new AbortController();
            const timeoutId = setTimeout(() => controller.abort(), 5000);

            const response = await fetch(`${this.config.url}/api/tags`, {
                method: 'GET',
                headers: {
                    'Content-Type': 'application/json'
                },
                signal: controller.signal
            });

            clearTimeout(timeoutId);

            return {
                success: response.ok,
                status: response.status
            };
        } catch (error) {
            return {
                success: false,
                error: this.normalizeError(error)
            };
        }
    }
}

module.exports = { ChatService };
