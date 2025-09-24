/*****************************************************************************/
/*                                                                           */
/*  extension.js                                         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 23 2025 11:39 st93642                      TT    SSSSSSS II */
/*  Updated: Sep 24 2025 18:38 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

const vscode = require('vscode');
const { execSync } = require('child_process');
const path = require('path');

// Import modular functions
const { generateClass } = require('../generators/classGenerators');
const { generateCodeBase } = require('../generators/codeBaseGenerators');
const { hasSubstantialContent, findHeaderEndLine } = require('../utils/contentAnalyzer');
const { createTSIProject } = require('../generators/project/projectCreator');

function activate(context) {
    console.log('TSI Header extension is now active!');

    // Helper function to show configuration instructions
    function showConfigurationInstructions(type) {
        const message = type === 'username' 
            ? '🔧 TSI Header Setup: Please configure your username to get started!\n\n' +
              '📝 Choose one option:\n' +
              '• VS Code Settings: Search "tsiheader.username"\n' +
              '• Git config: git config --global user.name "YourUsername"\n' +
              '• Environment: Set TSI_USERNAME variable'
            : '🔧 TSI Header Setup: Please configure your email to get started!\n\n' +
              '📝 Choose one option:\n' +
              '• VS Code Settings: Search "tsiheader.email"\n' +
              '• Git config: git config --global user.email "your.email@example.com"\n' +
              '• Environment: Set TSI_EMAIL variable';
        
        vscode.window.showInformationMessage(message, 'Open Settings', 'Git Config Help')
            .then(selection => {
                if (selection === 'Open Settings') {
                    vscode.commands.executeCommand('workbench.action.openSettings', `@ext:st93642.tsi-header`);
                } else if (selection === 'Git Config Help') {
                    vscode.env.openExternal(vscode.Uri.parse('https://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup'));
                }
            });
    }

    // Register insert header command
    const insertHeaderCommand = vscode.commands.registerCommand('tsiheader.insertHeader', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        try {
            const document = editor.document;
            const languageId = document.languageId;
            const fileName = document.fileName;
            
            // Get configuration
            const config = vscode.workspace.getConfiguration('tsiheader');
            const username = config.get('username');
            const email = config.get('email');
            
            // Check for credentials and show helpful setup instructions if missing
            const hasUsername = username && username.trim() !== '';
            const hasEmail = email && email.trim() !== '';
            
            // Check git config as fallback
            let gitUsername = '';
            let gitEmail = '';
            try {
                gitUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
            } catch (e) { /* ignore */ }
            try {
                gitEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
            } catch (e) { /* ignore */ }
            
            const hasAnyUsername = hasUsername || gitUsername;
            const hasAnyEmail = hasEmail || gitEmail;
            
            // Show configuration instructions if credentials are missing
            if (!hasAnyUsername) {
                showConfigurationInstructions('username');
                return;
            }
            if (!hasAnyEmail) {
                showConfigurationInstructions('email');
                return;
            }
            
            // Get Ruby CLI path
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
            
            // Set environment variables for configuration (only if they exist and are not empty)
            const env = {
                ...process.env
            };
            
            if (username && username.trim() !== '') {
                env.TSI_USERNAME = username;
            }
            if (email && email.trim() !== '') {
                env.TSI_EMAIL = email;
            }
            
            // Execute Ruby CLI
            const command = `ruby "${cliPath}" insert "${languageId}" "${fileName}"`;
            console.log('Executing command:', command);
            const result = execSync(command, { encoding: 'utf8', cwd: extensionPath, env: env });
            console.log('CLI result:', result);
            const response = JSON.parse(result);
            
            if (response.success) {
                // Insert header at the beginning of the document
                const firstLine = document.lineAt(0);
                const insertPosition = new vscode.Position(0, 0);
                
                await editor.edit(editBuilder => {
                    editBuilder.insert(insertPosition, response.header + '\n');
                });
                
                vscode.window.showInformationMessage('TSI Header inserted successfully!');
            } else {
                vscode.window.showErrorMessage(`Failed to insert header: ${response.message}`);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error: ${error.message}`);
        }
    });

    // Register update header command
    const updateHeaderCommand = vscode.commands.registerCommand('tsiheader.updateHeader', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        try {
            const document = editor.document;
            const languageId = document.languageId;
            const fileName = document.fileName;
            
            // Get configuration
            const config = vscode.workspace.getConfiguration('tsiheader');
            const username = config.get('username');
            const email = config.get('email');
            
            // Check for credentials and show helpful setup instructions if missing
            const hasUsername = username && username.trim() !== '';
            const hasEmail = email && email.trim() !== '';
            
            // Check git config as fallback
            let gitUsername = '';
            let gitEmail = '';
            try {
                gitUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
            } catch (e) { /* ignore */ }
            try {
                gitEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
            } catch (e) { /* ignore */ }
            
            const hasAnyUsername = hasUsername || gitUsername;
            const hasAnyEmail = hasEmail || gitEmail;
            
            // Show configuration instructions if credentials are missing
            if (!hasAnyUsername) {
                showConfigurationInstructions('username');
                return;
            }
            if (!hasAnyEmail) {
                showConfigurationInstructions('email');
                return;
            }
            
            // Get Ruby CLI path
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
            
            // Set environment variables for configuration (only if they exist and are not empty)
            const env = {
                ...process.env
            };
            
            if (username && username.trim() !== '') {
                env.TSI_USERNAME = username;
            }
            if (email && email.trim() !== '') {
                env.TSI_EMAIL = email;
            }
            
            // Execute Ruby CLI for update
            const command = `ruby "${cliPath}" update "${languageId}" "${fileName}"`;
            console.log('Executing update command:', command);
            const result = execSync(command, { encoding: 'utf8', cwd: extensionPath, env: env });
            console.log('Update CLI result:', result);
            const response = JSON.parse(result);
            
            if (response.success) {
                vscode.window.showInformationMessage('TSI Header updated successfully!');
            } else {
                vscode.window.showErrorMessage(`Failed to update header: ${response.message}`);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error: ${error.message}`);
        }
    });

    // Auto-save functionality: Listen for file save events
    const onSaveListener = vscode.workspace.onDidSaveTextDocument(async (document) => {
        // Check if auto-update is enabled
        const config = vscode.workspace.getConfiguration('tsiheader');
        const autoUpdate = config.get('autoUpdate');
        
        if (!autoUpdate) {
            return; // Auto-update is disabled
        }

        // Check if file has a TSI header to update
        const text = document.getText();
        const lines = text.split('\n');
        
        // Look for TSI header pattern in first few lines
        let hasHeader = false;
        for (let i = 0; i < Math.min(15, lines.length); i++) {
            if (lines[i].includes('Transport and Telecommunication Institute')) {
                hasHeader = true;
                break;
            }
        }
        
        if (!hasHeader) {
            return; // No TSI header found, nothing to update
        }

        // Check for credentials (same logic as manual update)
        const username = config.get('username');
        const email = config.get('email');
        
        const hasUsername = username && username.trim() !== '';
        const hasEmail = email && email.trim() !== '';
        
        // Check git config as fallback
        let gitUsername = '';
        let gitEmail = '';
        try {
            gitUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
        } catch (e) { /* ignore */ }
        try {
            gitEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
        } catch (e) { /* ignore */ }
        
        const hasAnyUsername = hasUsername || gitUsername;
        const hasAnyEmail = hasEmail || gitEmail;
        
        if (!hasAnyUsername || !hasAnyEmail) {
            return; // Skip auto-update if credentials are missing
        }

        // Perform the auto-update
        try {
            const languageId = document.languageId;
            const fileName = document.fileName;
            
            // Get Ruby CLI path
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
            
            // Set environment variables for configuration
            const env = {
                ...process.env
            };
            
            if (hasUsername) {
                env.TSI_USERNAME = username;
            }
            if (hasEmail) {
                env.TSI_EMAIL = email;
            }
            
            // Execute Ruby CLI for auto-update
            const command = `ruby "${cliPath}" update "${languageId}" "${fileName}"`;
            const result = execSync(command, { encoding: 'utf8', cwd: extensionPath, env: env });
            const response = JSON.parse(result);
            
            if (response.success) {
                // Silent success for auto-update - no notification needed
                console.log('TSI Header auto-updated successfully');
            }
        } catch (error) {
            // Silent failure for auto-update - no error notifications for background operations
            console.log('TSI Header auto-update failed:', error.message);
        }
    });

    // Register add class command
    const addClassCommand = vscode.commands.registerCommand('tsiheader.addClass', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        const document = editor.document;
        const languageId = document.languageId;
        const fileName = document.fileName;

        // Get credentials for template
        const config = vscode.workspace.getConfiguration('tsiheader');
        const username = config.get('username');
        const email = config.get('email');

        let finalUsername = username || 'unknown';
        let finalEmail = email || 'unknown@students.tsi.lv';

        // Check git config as fallback
        try {
            if (!username) {
                finalUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
            }
            if (!email) {
                finalEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
            }
        } catch (e) {
            // Git config not available, use defaults
        }

        const extensionPath = context.extensionPath;
        const now = new Date();
        // Format date to match Ruby header generator format: "Sep 24 2025 02:32"
        const dateStr = now.toLocaleDateString('en-US', { month: 'short', day: '2-digit', year: 'numeric' }).replace(',', '') + 
                       ' ' + now.toTimeString().slice(0, 5);

        try {
            // Generate proper TSI header using Ruby CLI
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
            
            // Set environment variables for configuration
            const env = {
                ...process.env
            };
            
            if (finalUsername && finalUsername.trim() !== '') {
                env.TSI_USERNAME = finalUsername;
            }
            if (finalEmail && finalEmail.trim() !== '') {
                env.TSI_EMAIL = finalEmail;
            }
            
            // Generate header using Ruby CLI
            const headerCommand = `ruby "${cliPath}" insert "${languageId}" "${fileName}"`;
            const headerResult = execSync(headerCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
            const headerResponse = JSON.parse(headerResult);
            
            if (!headerResponse.success) {
                vscode.window.showErrorMessage(`Failed to generate header: ${headerResponse.message}`);
                return;
            }
            
            let fullContent = headerResponse.header;
            
            // Ask for class name
            const className = await vscode.window.showInputBox({
                prompt: 'Enter class name',
                placeHolder: 'MyClass'
            });

            if (!className) {
                return; // User cancelled
            }

            // Generate class code using modular function
            const classResult = generateClass(languageId, className, fileName, extensionPath, cliPath, env);
            if (!classResult.success) {
                vscode.window.showErrorMessage(classResult.message);
                return;
            }

            // Handle different return types (file creation vs content generation)
            if (classResult.files) {
                // Multiple files were created (like C++ .hpp and .cpp)
                vscode.window.showInformationMessage(classResult.message);
                return;
            } else {
                // Single file content generation
                fullContent += '\n' + classResult.content;
            }

            // Check if file has substantial content beyond header using modular function
            const currentText = document.getText();
            const hasSubstantialContentFlag = hasSubstantialContent(currentText);
            
            if (hasSubstantialContentFlag) {
                const choice = await vscode.window.showWarningMessage(
                    'File already contains code. Add class anyway?',
                    'Add at Cursor', 'Add at End', 'Cancel'
                );
                
                if (choice === 'Cancel' || !choice) {
                    return;
                }
                
                // Insert at cursor position for "Add at Cursor", or end of file for "Add at End"
                const position = choice === 'Add at Cursor' 
                    ? (editor.selection.isEmpty ? editor.selection.active : editor.selection.end)
                    : new vscode.Position(document.lineCount, 0);
                
                await editor.edit(editBuilder => {
                    editBuilder.insert(position, '\n' + fullContent);
                });
            } else {
                // File is mostly empty (just header), replace content after header
                const headerEndLine = findHeaderEndLine(currentText);
                const range = new vscode.Range(
                    new vscode.Position(headerEndLine, 0),
                    new vscode.Position(document.lineCount, 0)
                );
                
                await editor.edit(editBuilder => {
                    editBuilder.replace(range, fullContent);
                });
            }

            vscode.window.showInformationMessage(`TSI Header: Added ${languageId} class "${className}" to current file`);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to add class: ${error.message}`);
        }
    });

    // Register add code base command
    const addCodeBaseCommand = vscode.commands.registerCommand('tsiheader.addCodeBase', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        const document = editor.document;
        const languageId = document.languageId;
        const fileName = document.fileName;

        // Get credentials for template
        const config = vscode.workspace.getConfiguration('tsiheader');
        const username = config.get('username');
        const email = config.get('email');

        let finalUsername = username || 'unknown';
        let finalEmail = email || 'unknown@students.tsi.lv';

        // Check git config as fallback
        try {
            if (!username) {
                finalUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
            }
            if (!email) {
                finalEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
            }
        } catch (e) {
            // Git config not available, use defaults
        }

        const extensionPath = context.extensionPath;
        const now = new Date();
        // Format date to match Ruby header generator format: "Sep 24 2025 02:32"
        const dateStr = now.toLocaleDateString('en-US', { month: 'short', day: '2-digit', year: 'numeric' }).replace(',', '') + 
                       ' ' + now.toTimeString().slice(0, 5);

        try {
            // Generate proper TSI header using Ruby CLI
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
            
            // Set environment variables for configuration
            const env = {
                ...process.env
            };
            
            if (finalUsername && finalUsername.trim() !== '') {
                env.TSI_USERNAME = finalUsername;
            }
            if (finalEmail && finalEmail.trim() !== '') {
                env.TSI_EMAIL = finalEmail;
            }
            
            // Generate header using Ruby CLI
            const headerCommand = `ruby "${cliPath}" insert "${languageId}" "${fileName}"`;
            const headerResult = execSync(headerCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
            const headerResponse = JSON.parse(headerResult);
            
            if (!headerResponse.success) {
                vscode.window.showErrorMessage(`Failed to generate header: ${headerResponse.message}`);
                return;
            }
            
            let fullContent = headerResponse.header;
            
            // Generate code structure using modular function
            const codeBaseResult = generateCodeBase(languageId, fileName);
            if (!codeBaseResult.success) {
                vscode.window.showErrorMessage(codeBaseResult.message);
                return;
            }
            
            fullContent += codeBaseResult.content;

            // Insert the content at cursor position or end of file
            const position = editor.selection.isEmpty ? editor.selection.active : editor.selection.end;
            await editor.edit(editBuilder => {
                editBuilder.insert(position, fullContent);
            });

            vscode.window.showInformationMessage(`TSI Header: Added ${languageId} code base to current file`);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to add code base: ${error.message}`);
        }
    });

    context.subscriptions.push(insertHeaderCommand);
    context.subscriptions.push(updateHeaderCommand);
    context.subscriptions.push(addClassCommand);
    context.subscriptions.push(addCodeBaseCommand);
    context.subscriptions.push(onSaveListener);

    // Register create TSI project command
    const createTSIProjectCommand = vscode.commands.registerCommand('tsiheader.createTSIProject', createTSIProject);
    context.subscriptions.push(createTSIProjectCommand);
}

function deactivate() {}

module.exports = {
    activate,
    deactivate
};