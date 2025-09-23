/*****************************************************************************/
/*                                                                           */
/*  extension.js                                         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 23 2025 11:39 st93642                      TT    SSSSSSS II */
/*  Updated: Sep 24 2025 02:18 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

const vscode = require('vscode');
const { execSync } = require('child_process');
const path = require('path');

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
    const addClassCommand = vscode.commands.registerCommand('tsiheader.addClass', async (uri) => {
        // Get the target directory from the URI (clicked folder in explorer)
        let targetDir = uri ? vscode.Uri.parse(uri.path).fsPath : vscode.workspace.workspaceFolders[0].uri.fsPath;

        // Ask for class name
        const className = await vscode.window.showInputBox({
            prompt: 'Enter class name',
            placeHolder: 'MyClass'
        });

        if (!className) {
            return; // User cancelled
        }

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

        // Determine language from current file or ask user
        let language = 'java'; // default
        const activeEditor = vscode.window.activeTextEditor;
        if (activeEditor) {
            language = activeEditor.document.languageId;
        }

        // Normalize language for template selection
        let templateLang = language;
        if (language === 'cpp' || language === 'c++') {
            templateLang = 'cpp';
        } else if (language === 'javascript') {
            templateLang = 'javascript';
        }

        const extensionPath = context.extensionPath;
        const now = new Date();
        const dateStr = now.toLocaleDateString('en-US', {
            year: 'numeric',
            month: 'short',
            day: '2-digit',
            hour: '2-digit',
            minute: '2-digit'
        });

        try {
            if (templateLang === 'java') {
                // Java class - single file
                const fileName = `${className}.java`;
                const filePath = path.join(targetDir, fileName);
                const templatePath = path.join(extensionPath, 'templates', 'java', 'class.java.template');

                // Check if file exists
                try {
                    await vscode.workspace.fs.stat(vscode.Uri.file(filePath));
                    const overwrite = await vscode.window.showWarningMessage(
                        `File '${fileName}' already exists. Overwrite?`,
                        'Yes', 'No'
                    );
                    if (overwrite !== 'Yes') {
                        return;
                    }
                } catch (e) {
                    // File doesn't exist, continue
                }

                const templateContent = await vscode.workspace.fs.readFile(vscode.Uri.file(templatePath));
                let content = templateContent.toString();

                // Replace template variables
                content = content.replace(/\{\{FILENAME\}\}/g, fileName);
                content = content.replace(/\{\{USERNAME\}\}/g, finalUsername);
                content = content.replace(/\{\{EMAIL\}\}/g, finalEmail);
                content = content.replace(/\{\{DATE\}\}/g, dateStr);
                content = content.replace(/\{\{CLASSNAME\}\}/g, className);
                content = content.replace(/\{\{PACKAGE\}\}/g, 'com.tsi.example');

                // Write the file
                await vscode.workspace.fs.writeFile(vscode.Uri.file(filePath), Buffer.from(content));

                // Open the file in editor
                const document = await vscode.workspace.openTextDocument(filePath);
                await vscode.window.showTextDocument(document);

                vscode.window.showInformationMessage(`TSI Header: Created Java class ${fileName}`);

            } else if (templateLang === 'cpp') {
                // C++ class - header and implementation files
                const headerFile = `${className}.h`;
                const implFile = `${className}.cpp`;
                const headerPath = path.join(targetDir, headerFile);
                const implPath = path.join(targetDir, implFile);

                // Check if files exist
                try {
                    await vscode.workspace.fs.stat(vscode.Uri.file(headerPath));
                    const overwrite = await vscode.window.showWarningMessage(
                        `File '${headerFile}' already exists. Overwrite?`,
                        'Yes', 'No'
                    );
                    if (overwrite !== 'Yes') {
                        return;
                    }
                } catch (e) {}
                try {
                    await vscode.workspace.fs.stat(vscode.Uri.file(implPath));
                    const overwrite = await vscode.window.showWarningMessage(
                        `File '${implFile}' already exists. Overwrite?`,
                        'Yes', 'No'
                    );
                    if (overwrite !== 'Yes') {
                        return;
                    }
                } catch (e) {}

                // Create header file
                const headerTemplatePath = path.join(extensionPath, 'templates', 'cpp', 'class.h.template');
                const headerTemplate = await vscode.workspace.fs.readFile(vscode.Uri.file(headerTemplatePath));
                let headerContent = headerTemplate.toString();

                const headerGuard = `${className.toUpperCase()}_H`;
                headerContent = headerContent.replace(/\{\{FILENAME\}\}/g, headerFile);
                headerContent = headerContent.replace(/\{\{USERNAME\}\}/g, finalUsername);
                headerContent = headerContent.replace(/\{\{EMAIL\}\}/g, finalEmail);
                headerContent = headerContent.replace(/\{\{DATE\}\}/g, dateStr);
                headerContent = headerContent.replace(/\{\{CLASSNAME\}\}/g, className);
                headerContent = headerContent.replace(/\{\{HEADER_GUARD\}\}/g, headerGuard);

                await vscode.workspace.fs.writeFile(vscode.Uri.file(headerPath), Buffer.from(headerContent));

                // Create implementation file
                const implTemplatePath = path.join(extensionPath, 'templates', 'cpp', 'class.cpp.template');
                const implTemplate = await vscode.workspace.fs.readFile(vscode.Uri.file(implTemplatePath));
                let implContent = implTemplate.toString();

                implContent = implContent.replace(/\{\{FILENAME\}\}/g, implFile);
                implContent = implContent.replace(/\{\{USERNAME\}\}/g, finalUsername);
                implContent = implContent.replace(/\{\{EMAIL\}\}/g, finalEmail);
                implContent = implContent.replace(/\{\{DATE\}\}/g, dateStr);
                implContent = implContent.replace(/\{\{CLASSNAME\}\}/g, className);
                implContent = implContent.replace(/\{\{HEADER_FILE\}\}/g, headerFile);

                await vscode.workspace.fs.writeFile(vscode.Uri.file(implPath), Buffer.from(implContent));

                // Open the header file in editor
                const document = await vscode.workspace.openTextDocument(headerPath);
                await vscode.window.showTextDocument(document);

                vscode.window.showInformationMessage(`TSI Header: Created C++ class ${className} (${headerFile} + ${implFile})`);

            } else if (templateLang === 'python') {
                // Python class - single file
                const fileName = `${className}.py`;
                const filePath = path.join(targetDir, fileName);
                const templatePath = path.join(extensionPath, 'templates', 'python', 'class.py.template');

                // Check if file exists
                try {
                    await vscode.workspace.fs.stat(vscode.Uri.file(filePath));
                    const overwrite = await vscode.window.showWarningMessage(
                        `File '${fileName}' already exists. Overwrite?`,
                        'Yes', 'No'
                    );
                    if (overwrite !== 'Yes') {
                        return;
                    }
                } catch (e) {}

                const templateContent = await vscode.workspace.fs.readFile(vscode.Uri.file(templatePath));
                let content = templateContent.toString();

                // Replace template variables
                content = content.replace(/\{\{FILENAME\}\}/g, fileName);
                content = content.replace(/\{\{USERNAME\}\}/g, finalUsername);
                content = content.replace(/\{\{EMAIL\}\}/g, finalEmail);
                content = content.replace(/\{\{DATE\}\}/g, dateStr);
                content = content.replace(/\{\{CLASSNAME\}\}/g, className);

                // Write the file
                await vscode.workspace.fs.writeFile(vscode.Uri.file(filePath), Buffer.from(content));

                // Open the file in editor
                const document = await vscode.workspace.openTextDocument(filePath);
                await vscode.window.showTextDocument(document);

                vscode.window.showInformationMessage(`TSI Header: Created Python class ${fileName}`);
            } else {
                vscode.window.showErrorMessage(`Class generation not supported for language: ${language}`);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to create class: ${error.message}`);
        }
    });

    // Register add code base command
    const addCodeBaseCommand = vscode.commands.registerCommand('tsiheader.addCodeBase', async (uri) => {
        // Get the target directory from the URI (clicked folder in explorer)
        let targetDir = uri ? vscode.Uri.parse(uri.path).fsPath : vscode.workspace.workspaceFolders[0].uri.fsPath;

        // Ask for filename without extension
        const fileName = await vscode.window.showInputBox({
            prompt: 'Enter filename (without extension)',
            placeHolder: 'main'
        });

        if (!fileName) {
            return; // User cancelled
        }

        // Determine language from current file or ask user
        let language = 'c'; // default
        const activeEditor = vscode.window.activeTextEditor;
        if (activeEditor) {
            language = activeEditor.document.languageId;
        }

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
        const dateStr = now.toLocaleDateString('en-US', {
            year: 'numeric',
            month: 'short',
            day: '2-digit',
            hour: '2-digit',
            minute: '2-digit'
        });

        try {
            let templatePath, fullFileName, content;

            if (language === 'c') {
                fullFileName = `${fileName}.c`;
                templatePath = path.join(extensionPath, 'templates', 'c', 'main.c.template');
            } else if (language === 'python') {
                fullFileName = `${fileName}.py`;
                templatePath = path.join(extensionPath, 'templates', 'python', 'script.py.template');
            } else if (language === 'javascript') {
                fullFileName = `${fileName}.js`;
                templatePath = path.join(extensionPath, 'templates', 'javascript', 'script.js.template');
            } else {
                // Default to C for unsupported languages
                fullFileName = `${fileName}.c`;
                templatePath = path.join(extensionPath, 'templates', 'c', 'main.c.template');
            }

            const filePath = path.join(targetDir, fullFileName);

            // Check if file already exists
            try {
                await vscode.workspace.fs.stat(vscode.Uri.file(filePath));
                const overwrite = await vscode.window.showWarningMessage(
                    `File '${fullFileName}' already exists. Overwrite?`,
                    'Yes', 'No'
                );
                if (overwrite !== 'Yes') {
                    return;
                }
            } catch (e) {
                // File doesn't exist, which is fine
            }

            const templateContent = await vscode.workspace.fs.readFile(vscode.Uri.file(templatePath));
            content = templateContent.toString();

            // Replace template variables
            content = content.replace(/\{\{FILENAME\}\}/g, fullFileName);
            content = content.replace(/\{\{USERNAME\}\}/g, finalUsername);
            content = content.replace(/\{\{EMAIL\}\}/g, finalEmail);
            content = content.replace(/\{\{DATE\}\}/g, dateStr);

            // Write the file
            await vscode.workspace.fs.writeFile(vscode.Uri.file(filePath), Buffer.from(content));

            // Open the file in editor
            const document = await vscode.workspace.openTextDocument(filePath);
            await vscode.window.showTextDocument(document);

            vscode.window.showInformationMessage(`TSI Header: Created ${fullFileName} with basic code structure`);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to create file: ${error.message}`);
        }
    });

    context.subscriptions.push(insertHeaderCommand);
    context.subscriptions.push(updateHeaderCommand);
    context.subscriptions.push(addClassCommand);
    context.subscriptions.push(addCodeBaseCommand);
    context.subscriptions.push(onSaveListener);
}

function deactivate() {}

module.exports = {
    activate,
    deactivate
};