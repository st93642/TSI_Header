# Release Notes

## Version 8.0.1 (November 28, 2025)

### New Features
- **Enhanced Chat Output Formatting**: Assistant responses now render as formatted text with proper markdown support, including syntax-highlighted code blocks using highlight.js.

### Bug Fixes
- **Fixed Rename and Delete Buttons**: Conversation rename and delete buttons now function properly by using VS Code's native input and confirmation dialogs instead of unsupported webview prompt/confirm methods.

### Technical Details
- Added marked.js for markdown parsing and highlight.js for code syntax highlighting
- Assistant messages are rendered as HTML with proper styling for code blocks and text formatting
- User messages remain as plain text for security

### Previous Changes (v8.0.0)
- **Chat Feature Fully Functional**: Resolved the "no data provider" error for the chat icon. The chat module now properly integrates with Ollama for AI-powered conversations.
- **Increased Timeout for Local Models**: Default timeout for chat requests increased from 30 seconds to 3 minutes to accommodate slower local Ollama model responses.

### Bug Fixes (v8.0.0)
- Fixed missing "type": "webview" attribute in chat view configuration, preventing VS Code from recognizing the chat panel.
- Resolved asynchronous view provider registration issues that caused chat functionality to fail on startup.
- Corrected JavaScript syntax errors and dependency handling in chat service.

### Improvements (v8.0.0)
- Enhanced error handling and user feedback for chat operations.
- Updated documentation to reflect new timeout defaults.

### Technical Details (v8.0.0)
- Chat service now uses native fetch API with configurable timeouts.
- Webview providers are registered synchronously at extension activation.
- All chat-related tests pass successfully.

### Known Issues
- Chat functionality requires Ollama to be installed and running locally.
- Some tests may fail if Ollama is not available during testing.

### Installation
Install Ollama from https://ollama.com/ and ensure it's running on localhost:11434 (default).

### Usage
- Access chat via the chat icon in the VS Code sidebar.
- Configure chat settings in VS Code settings under "TSI Header > Chat".
- Pull models using `ollama pull <model-name>` (e.g., `ollama pull mistral`).