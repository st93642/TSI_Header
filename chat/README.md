# Chat Module

This module provides chat persistence and Ollama integration for the Uni Header extension.

## Components

### ChatDataManager

Manages persistence of chat conversations using VS Code's global state storage.

#### Features

- **CRUD Operations**: Create, read, update, and delete conversations
- **Message Management**: Append user and assistant messages to conversations
- **History Trimming**: Automatically trim conversation history beyond configurable limits
- **Active Conversation Tracking**: Keep track of the currently active conversation
- **Export/Import**: Export and import conversations for backup or sharing
- **Data Migration**: Automatic data structure migration for version updates

#### Usage

```javascript
const { ChatDataManager } = require('./chat/src/chatDataManager');

// Initialize with VS Code extension context
const dataManager = new ChatDataManager(context);

// Create a new conversation
const conversation = await dataManager.createConversation('My Chat', 'mistral');

// Append messages
await dataManager.appendMessage(conversation.id, 'user', 'Hello!');
await dataManager.appendMessage(conversation.id, 'assistant', 'Hi there!');

// Get all conversations
const conversations = await dataManager.getConversations();

// Get active conversation
const active = await dataManager.getActiveConversation();

// Trim history to last 10 messages
await dataManager.trimConversationHistory(conversation.id, 10);

// Export/Import
const exportedData = await dataManager.exportData();
await dataManager.importData(exportedData);
```

#### Data Schema

```javascript
{
  version: '1.0.0',
  conversations: [
    {
      id: 'chat-123456789-abc123',
      title: 'My Conversation',
      model: 'mistral',
      messages: [
        {
          role: 'user',
          content: 'Hello!',
          timestamp: '2024-01-01T00:00:00.000Z'
        },
        {
          role: 'assistant',
          content: 'Hi there!',
          timestamp: '2024-01-01T00:00:01.000Z'
        }
      ],
      createdAt: '2024-01-01T00:00:00.000Z',
      updatedAt: '2024-01-01T00:00:01.000Z'
    }
  ],
  activeConversationId: 'chat-123456789-abc123'
}
```

### ChatService

Handles HTTP communication with the Ollama server for LLM interactions.

#### Features

- **Model Listing**: Get available models from Ollama server
- **Chat API**: Send messages and receive responses from Ollama
- **Streaming Support**: Stream responses for real-time display (for future implementation)
- **Configuration Management**: Load and refresh settings from VS Code configuration
- **Error Handling**: Normalize errors for user-friendly display
- **Connection Testing**: Check if Ollama is running and accessible
- **Timeout Management**: Configurable timeouts for all requests

#### Usage

```javascript
const { ChatService } = require('./chat/src/chatService');

// Initialize with VS Code API
const chatService = new ChatService(vscode);

// Check connection
const connectionResult = await chatService.checkConnection();
if (!connectionResult.success) {
  console.error('Ollama not running:', connectionResult.error);
}

// List available models
const modelsResult = await chatService.listModels();
if (modelsResult.success) {
  console.log('Available models:', modelsResult.models);
}

// Send a message
const messages = [
  { role: 'user', content: 'What is TypeScript?' }
];
const result = await chatService.sendMessage(messages, 'mistral');

if (result.success) {
  console.log('Response:', result.message.content);
} else {
  console.error('Error:', result.error.message);
}

// Refresh configuration after settings change
chatService.refreshConfig();
```

#### Configuration

The service reads from VS Code settings (`tsiheader.chat`):

- `ollamaUrl`: URL for connecting to Ollama (default: `http://localhost:11434`)
- `defaultModel`: Default LLM model to use (default: `mistral`)
- `temperature`: Sampling temperature 0-2 (default: `0.7`)
- `maxTokens`: Maximum tokens in responses (default: `2048`)
- `historyLimit`: Maximum messages in context (default: `10`)

#### Error Codes

- `OLLAMA_NOT_RUNNING`: Cannot connect to Ollama server
- `TIMEOUT`: Request timed out
- `MODEL_NOT_FOUND`: Specified model is not available
- `INVALID_URL`: Cannot resolve Ollama URL
- `UNKNOWN_ERROR`: Other errors

## Testing

Run the test suites:

```bash
# Test data manager
node chat/src/chatDataManager.test.js

# Test service
node chat/src/chatService.test.js
```

## Dependencies

- `node-fetch@^2.6.7`: HTTP client for making requests to Ollama

## Integration

Import both components:

```javascript
const { ChatDataManager, ChatService } = require('./chat/src');
```

## Example: Complete Chat Flow

```javascript
const { ChatDataManager, ChatService } = require('./chat/src');

async function chat(context, vscode) {
  const dataManager = new ChatDataManager(context);
  const chatService = new ChatService(vscode);

  // Create or get active conversation
  let conversation = await dataManager.getActiveConversation();
  if (!conversation) {
    conversation = await dataManager.createConversation('New Chat');
  }

  // Add user message
  const userMessage = 'Explain async/await in JavaScript';
  await dataManager.appendMessage(conversation.id, 'user', userMessage);

  // Get recent messages for context
  const conv = await dataManager.getConversation(conversation.id);
  const messages = conv.messages.slice(-10); // Last 10 messages

  // Send to Ollama
  const result = await chatService.sendMessage(messages, conversation.model);

  if (result.success) {
    // Save assistant response
    await dataManager.appendMessage(
      conversation.id,
      'assistant',
      result.message.content
    );
    return result.message.content;
  } else {
    throw new Error(result.error.message);
  }
}
```

## Future Enhancements

- Streaming response support for real-time display
- Conversation templates
- Search/filter conversations
- Message editing and regeneration
- Token usage tracking
- Model switching mid-conversation
