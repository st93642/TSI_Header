const { execSync } = require('child_process');
const path = require('path');

function showConfigurationInstructions(vscode, type) {
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
                vscode.commands.executeCommand('workbench.action.openSettings', `@ext:st93642.uni-header`);
            } else if (selection === 'Git Config Help') {
                vscode.env.openExternal(vscode.Uri.parse('https://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup'));
            }
        });
}

function getCredentials(vscode) {
    const config = vscode.workspace.getConfiguration('tsiheader');
    const username = config.get('username');
    const email = config.get('email');
    
    const hasUsername = username && username.trim() !== '';
    const hasEmail = email && email.trim() !== '';
    
    let gitUsername = '';
    let gitEmail = '';
    try {
        gitUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
    } catch (e) { /* ignore */ }
    try {
        gitEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
    } catch (e) { /* ignore */ }
    
    return {
        username,
        email,
        hasUsername,
        hasEmail,
        gitUsername,
        gitEmail,
        hasAnyUsername: hasUsername || gitUsername,
        hasAnyEmail: hasEmail || gitEmail
    };
}

function buildEnvironment(vscode, credentials) {
    const config = vscode.workspace.getConfiguration('tsiheader');
    const enableCustomHeader = config.get('customHeader.enableCustomHeader', false);
    const institutionName = config.get('customHeader.institutionName', 'Transport and Telecommunication Institute - Riga, Latvia');
    const institutionUrl = config.get('customHeader.institutionUrl', 'https://tsi.lv');
    
    const env = { ...process.env };
    
    if (credentials.username && credentials.username.trim() !== '') {
        env.TSI_USERNAME = credentials.username;
    }
    if (credentials.email && credentials.email.trim() !== '') {
        env.TSI_EMAIL = credentials.email;
    }
    
    if (enableCustomHeader) {
        env.TSI_CUSTOM_HEADER_ENABLED = 'true';
        if (institutionName && institutionName.trim() !== '') {
            env.TSI_CUSTOM_INSTITUTION_NAME = institutionName;
        }
        if (institutionUrl && institutionUrl.trim() !== '') {
            env.TSI_CUSTOM_INSTITUTION_URL = institutionUrl;
        }
    }
    
    return env;
}

function detectLanguageId(languageId, fileName) {
    let detectedLanguageId = languageId;
    const fileExtension = fileName.split('.').pop().toLowerCase();
    if (fileExtension === 'erb') {
        detectedLanguageId = 'erb';
    } else if (fileExtension === 'vue') {
        detectedLanguageId = 'vue';
    } else if (fileExtension === 'ejs') {
        detectedLanguageId = 'ejs';
    }
    return detectedLanguageId;
}

function hasHeaderPattern(lines) {
    for (let i = 0; i < Math.min(15, lines.length); i++) {
        const line = lines[i].trim();
        if (line.match(/^\/\*[\*]+\*\/$/) ||
            line.match(/^#[\*]+#$/) ||
            line.match(/^;; [\*]+ ;;$/) ||
            line.match(/^\(\* [\*]+ \*\)$/) ||
            line.match(/^-- [\*]+ --$/) ||
            line.match(/^%% [\*]+ %%$/) ||
            line.match(/^<!-- [\*]+ -->$/) ||
            line.match(/^<# [\*]+ #>$/) ||
            line.match(/^{ [\*]+ }$/) ||
            line.match(/^" [\*]+ "$/) ||
            line.match(/^\/\/ [\*]+ \/\/$/) ||
            line.match(/^<!--- [\*]+ --->$/) ||
            line.match(/^! [\*]+ !$/) ||
            line.match(/^\* [\*]+ \*$/) ||
            line.match(/^; [\*]+ ;$/) ||
            line.match(/^{# [\*]+ #}$/) ||
            line.match(/^-# [\*]+ -#$/) ||
            line.match(/^{{\!-- [\*]+ --}}$/) ||
            line.match(/^@\* [\*]+ \*@$/) ||
            line.match(/^[\*]{10,}$/)) {
            return { hasHeader: true, startLine: i };
        }
    }
    return { hasHeader: false, startLine: -1 };
}

function findHeaderEnd(lines, startLine) {
    for (let j = startLine + 1; j < Math.min(startLine + 20, lines.length); j++) {
        const checkLine = lines[j].trim();
        if (checkLine.match(/^\/\*[\*]+\*\/$/) ||
            checkLine.match(/^#[\*]+#$/) ||
            checkLine.match(/^;; [\*]+ ;;$/) ||
            checkLine.match(/^\(\* [\*]+ \*\)$/) ||
            checkLine.match(/^-- [\*]+ --$/) ||
            checkLine.match(/^%% [\*]+ %%$/) ||
            checkLine.match(/^<!-- [\*]+ -->$/) ||
            checkLine.match(/^<# [\*]+ #>$/) ||
            checkLine.match(/^{ [\*]+ }$/) ||
            checkLine.match(/^" [\*]+ "$/) ||
            checkLine.match(/^\/\/ [\*]+ \/\/$/) ||
            checkLine.match(/^<!--- [\*]+ --->$/) ||
            checkLine.match(/^! [\*]+ !$/) ||
            checkLine.match(/^\* [\*]+ \*$/) ||
            checkLine.match(/^; [\*]+ ;$/) ||
            checkLine.match(/^{# [\*]+ #}$/) ||
            checkLine.match(/^-# [\*]+ -#$/) ||
            checkLine.match(/^{{\!-- [\*]+ --}}$/) ||
            checkLine.match(/^@\* [\*]+ \*@$/) ||
            checkLine.match(/^[\*]{10,}$/)) {
            return j;
        }
    }
    return -1;
}

function register(context, deps) {
    const { vscode, core } = deps;
    
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
            
            const detectedLanguageId = detectLanguageId(languageId, fileName);
            
            const credentials = getCredentials(vscode);
            
            if (!credentials.hasAnyUsername) {
                showConfigurationInstructions(vscode, 'username');
                return;
            }
            if (!credentials.hasAnyEmail) {
                showConfigurationInstructions(vscode, 'email');
                return;
            }
            
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
            
            const env = buildEnvironment(vscode, credentials);
            
            const command = `ruby "${cliPath}" insert "${detectedLanguageId}" "${fileName}"`;
            console.log('Executing command:', command);
            const result = execSync(command, { encoding: 'utf8', cwd: extensionPath, env: env });
            console.log('CLI result:', result);
            const response = JSON.parse(result);
            
            if (response.success) {
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
            
            const detectedLanguageId = detectLanguageId(languageId, fileName);
            
            const credentials = getCredentials(vscode);
            
            if (!credentials.hasAnyUsername) {
                showConfigurationInstructions(vscode, 'username');
                return;
            }
            if (!credentials.hasAnyEmail) {
                showConfigurationInstructions(vscode, 'email');
                return;
            }
            
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
            
            const env = buildEnvironment(vscode, credentials);
            
            const command = `ruby "${cliPath}" update "${detectedLanguageId}" "${fileName}"`;
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

    const removeHeaderCommand = vscode.commands.registerCommand('tsiheader.removeHeader', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        try {
            const document = editor.document;
            const text = document.getText();
            
            const lines = text.split('\n');
            const headerCheck = hasHeaderPattern(lines);
            
            if (!headerCheck.hasHeader) {
                vscode.window.showErrorMessage('No header found to remove in this file.');
                return;
            }
            
            const headerEndLine = findHeaderEnd(lines, headerCheck.startLine);
            
            if (headerEndLine === -1) {
                vscode.window.showErrorMessage('Could not find the end of the header.');
                return;
            }
            
            const range = new vscode.Range(
                new vscode.Position(0, 0),
                new vscode.Position(headerEndLine + 1, 0)
            );
            
            await editor.edit(editBuilder => {
                editBuilder.delete(range);
            });
            
            vscode.window.showInformationMessage('TSI Header removed successfully!');
        } catch (error) {
            vscode.window.showErrorMessage(`Error: ${error.message}`);
        }
    });

    const onSaveListener = vscode.workspace.onDidSaveTextDocument(async (document) => {
        const config = vscode.workspace.getConfiguration('tsiheader');
        const autoUpdate = config.get('autoUpdate');
        
        if (!autoUpdate) {
            return;
        }

        const text = document.getText();
        const lines = text.split('\n');
        
        const headerCheck = hasHeaderPattern(lines);
        
        if (!headerCheck.hasHeader) {
            return;
        }

        const credentials = getCredentials(vscode);
        
        if (!credentials.hasAnyUsername || !credentials.hasAnyEmail) {
            return;
        }

        try {
            const languageId = document.languageId;
            const fileName = document.fileName;
            
            const detectedLanguageId = core.utils.detectLanguageFromExtension(languageId, fileName);
            
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
            
            const env = buildEnvironment(vscode, credentials);
            
            const command = `ruby "${cliPath}" update "${detectedLanguageId}" "${fileName}"`;
            const result = execSync(command, { encoding: 'utf8', cwd: extensionPath, env: env });
            const response = JSON.parse(result);
            
            if (response.success) {
                console.log('TSI Header auto-updated successfully');
            }
        } catch (error) {
            console.log('TSI Header auto-update failed:', error.message);
        }
    });
    
    context.subscriptions.push(insertHeaderCommand);
    context.subscriptions.push(updateHeaderCommand);
    context.subscriptions.push(removeHeaderCommand);
    context.subscriptions.push(onSaveListener);
}

module.exports = { register };
