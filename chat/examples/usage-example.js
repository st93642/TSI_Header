/**
 * Chat Module Usage Example
 * Demonstrates basic usage of ChatDataManager and ChatService
 */

const { ChatDataManager, ChatService } = require('../src');

// Mock VS Code context for demonstration
const createMockContext = () => {
    const storage = new Map();
    return {
        globalState: {
            get: (key, defaultValue) => storage.has(key) ? storage.get(key) : defaultValue,
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

// Mock VS Code API for demonstration
const mockVSCode = {
    workspace: {
        getConfiguration: (section) => ({
            get: (key, defaultValue) => {
                const config = {
                    ollamaUrl: 'http://localhost:11434',
                    defaultModel: 'mistral',
                    temperature: 0.7,
                    maxTokens: 2048,
                    historyLimit: 10
                };
                return config[key] !== undefined ? config[key] : defaultValue;
            }
        })
    }
};

async function demonstrateDataManager() {
    console.log('=== ChatDataManager Demo ===\n');

    const context = createMockContext();
    const dataManager = new ChatDataManager(context);

    // Create conversations
    console.log('1. Creating conversations...');
    const conv1 = await dataManager.createConversation('JavaScript Help', 'mistral');
    console.log(`   Created: "${conv1.title}" (${conv1.id})`);

    const conv2 = await dataManager.createConversation('Python Questions', 'llama2');
    console.log(`   Created: "${conv2.title}" (${conv2.id})\n`);

    // Add messages
    console.log('2. Adding messages to first conversation...');
    await dataManager.appendMessage(conv1.id, 'user', 'What is async/await?');
    await dataManager.appendMessage(conv1.id, 'assistant', 'Async/await is a syntax for handling promises...');
    console.log('   Added 2 messages\n');

    // Get conversation with messages
    console.log('3. Retrieving conversation...');
    const retrieved = await dataManager.getConversation(conv1.id);
    console.log(`   Title: ${retrieved.title}`);
    console.log(`   Messages: ${retrieved.messages.length}`);
    console.log(`   Latest message: "${retrieved.messages[retrieved.messages.length - 1].content.substring(0, 50)}..."\n`);

    // List all conversations
    console.log('4. Listing all conversations...');
    const allConvs = await dataManager.getConversations();
    allConvs.forEach((conv, i) => {
        console.log(`   ${i + 1}. ${conv.title} (${conv.messages.length} messages)`);
    });
    console.log();

    // Rename
    console.log('5. Renaming conversation...');
    await dataManager.renameConversation(conv1.id, 'JavaScript Advanced Topics');
    const renamed = await dataManager.getConversation(conv1.id);
    console.log(`   New title: "${renamed.title}"\n`);

    // Active conversation
    console.log('6. Working with active conversation...');
    const active = await dataManager.getActiveConversation();
    console.log(`   Active: "${active.title}"\n`);

    // Export
    console.log('7. Exporting data...');
    const exported = await dataManager.exportData();
    console.log(`   Exported ${exported.conversations.length} conversations`);
    console.log(`   Version: ${exported.version}\n`);

    // Trim history
    console.log('8. Demonstrating history trimming...');
    for (let i = 0; i < 15; i++) {
        await dataManager.appendMessage(conv1.id, 'user', `Message ${i}`);
    }
    console.log(`   Added 15 more messages`);
    const beforeTrim = await dataManager.getConversation(conv1.id);
    console.log(`   Total messages before trim: ${beforeTrim.messages.length}`);

    await dataManager.trimConversationHistory(conv1.id, 10);
    const afterTrim = await dataManager.getConversation(conv1.id);
    console.log(`   Total messages after trim: ${afterTrim.messages.length}\n`);

    // Delete
    console.log('9. Deleting conversation...');
    await dataManager.deleteConversation(conv2.id);
    const remaining = await dataManager.getConversations();
    console.log(`   Remaining conversations: ${remaining.length}\n`);
}

async function demonstrateChatService() {
    console.log('=== ChatService Demo ===\n');

    const service = new ChatService(mockVSCode);

    // Get configuration
    console.log('1. Current configuration:');
    const config = service.getConfig();
    console.log(`   URL: ${config.url}`);
    console.log(`   Model: ${config.model}`);
    console.log(`   Temperature: ${config.temperature}`);
    console.log(`   Max Tokens: ${config.maxTokens}\n`);

    // Check connection
    console.log('2. Checking Ollama connection...');
    const connection = await service.checkConnection();
    if (connection.success) {
        console.log('   ✓ Ollama is running\n');
    } else {
        console.log(`   ✗ ${connection.error.message}`);
        console.log(`   Detail: ${connection.error.detail}\n`);
    }

    // List models (will fail without Ollama running)
    console.log('3. Listing available models...');
    const modelsResult = await service.listModels();
    if (modelsResult.success) {
        console.log(`   Found ${modelsResult.models.length} models:`);
        modelsResult.models.slice(0, 3).forEach(model => {
            console.log(`   - ${model.name}`);
        });
    } else {
        console.log(`   ✗ ${modelsResult.error.message}`);
    }
    console.log();

    // Error normalization demo
    console.log('4. Error normalization examples:');
    const errors = [
        { code: 'ECONNREFUSED', message: 'Connection refused' },
        { code: 'ETIMEDOUT', message: 'Timeout' },
        { message: 'HTTP 404: Not Found' }
    ];

    errors.forEach(err => {
        const error = new Error(err.message);
        if (err.code) error.code = err.code;
        const normalized = service.normalizeError(error);
        console.log(`   ${normalized.code}: ${normalized.message}`);
    });
    console.log();
}

// Run demonstrations
async function main() {
    try {
        await demonstrateDataManager();
        console.log('\n' + '='.repeat(50) + '\n');
        await demonstrateChatService();
        console.log('\n✓ Demo completed successfully!');
    } catch (error) {
        console.error('Error:', error.message);
        process.exit(1);
    }
}

// Run if executed directly
if (require.main === module) {
    main();
}

module.exports = { demonstrateDataManager, demonstrateChatService };
