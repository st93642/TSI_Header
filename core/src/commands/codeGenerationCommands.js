const { execSync } = require('child_process');
const path = require('path');

function getCredentials(vscode) {
    const config = vscode.workspace.getConfiguration('tsiheader');
    const username = config.get('username');
    const email = config.get('email');
    
    let finalUsername = username || 'unknown';
    let finalEmail = email || 'unknown@students.tsi.lv';
    
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
    
    return { username: finalUsername, email: finalEmail };
}

function buildEnvironment(credentials) {
    const env = { ...process.env };
    
    if (credentials.username && credentials.username.trim() !== '') {
        env.TSI_USERNAME = credentials.username;
    }
    if (credentials.email && credentials.email.trim() !== '') {
        env.TSI_EMAIL = credentials.email;
    }
    
    return env;
}

function register(context, deps) {
    const { vscode, core } = deps;
    
    const addClassCommand = vscode.commands.registerCommand('tsiheader.addClass', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        const document = editor.document;
        const languageId = document.languageId;
        const fileName = document.fileName;

        const detectedLanguageId = core.utils.detectLanguageFromExtension(languageId, fileName);

        const credentials = getCredentials(vscode);

        const extensionPath = context.extensionPath;

        try {
            const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
            
            const env = buildEnvironment(credentials);
            
            const headerCommand = `ruby "${cliPath}" insert "${detectedLanguageId}" "${fileName}"`;
            const headerResult = execSync(headerCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
            const headerResponse = JSON.parse(headerResult);
            
            if (!headerResponse.success) {
                vscode.window.showErrorMessage(`Failed to generate header: ${headerResponse.message}`);
                return;
            }
            
            let fullContent = headerResponse.header;
            
            const className = await vscode.window.showInputBox({
                prompt: 'Enter class name',
                placeHolder: 'MyClass'
            });

            if (!className) {
                return;
            }

            const classResult = core.codeGenerator.generateClass(detectedLanguageId, className, fileName, env);
            if (!classResult.success) {
                vscode.window.showErrorMessage(classResult.message);
                return;
            }

            if (classResult.files) {
                vscode.window.showInformationMessage(classResult.message);
                return;
            } else {
                fullContent += '\n' + classResult.content;
            }

            const currentText = document.getText();
            const hasSubstantialContentFlag = core.hasSubstantialContent(currentText);
            
            if (hasSubstantialContentFlag) {
                const choice = await vscode.window.showWarningMessage(
                    'File already contains code. Add class anyway?',
                    'Add at Cursor', 'Add at End', 'Cancel'
                );
                
                if (choice === 'Cancel' || !choice) {
                    return;
                }
                
                const position = choice === 'Add at Cursor' 
                    ? (editor.selection.isEmpty ? editor.selection.active : editor.selection.end)
                    : new vscode.Position(document.lineCount, 0);
                
                await editor.edit(editBuilder => {
                    editBuilder.insert(position, '\n' + fullContent);
                });
            } else {
                const headerEndLine = core.findHeaderEndLine(currentText);
                const range = new vscode.Range(
                    new vscode.Position(headerEndLine, 0),
                    new vscode.Position(document.lineCount, 0)
                );
                
                await editor.edit(editBuilder => {
                    editBuilder.replace(range, fullContent);
                });
            }

            vscode.window.showInformationMessage(`TSI Header: Added ${detectedLanguageId} class "${className}" to current file`);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to add class: ${error.message}`);
        }
    });

    const addCodeBaseCommand = vscode.commands.registerCommand('tsiheader.addCodeBase', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        const document = editor.document;
        const languageId = document.languageId;
        const fileName = document.fileName;

        const detectedLanguageId = core.utils.detectLanguageFromExtension(languageId, fileName);

        const credentials = getCredentials(vscode);

        const extensionPath = context.extensionPath;

        try {
            const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
            
            const env = buildEnvironment(credentials);
            
            const headerCommand = `ruby "${cliPath}" insert "${detectedLanguageId}" "${fileName}"`;
            const headerResult = execSync(headerCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
            const headerResponse = JSON.parse(headerResult);
            
            if (!headerResponse.success) {
                vscode.window.showErrorMessage(`Failed to generate header: ${headerResponse.message}`);
                return;
            }
            
            let fullContent = headerResponse.header;
            
            const codeBaseResult = core.codeGenerator.generateCodeBase(detectedLanguageId, fileName);
            if (!codeBaseResult.success) {
                vscode.window.showErrorMessage(codeBaseResult.message);
                return;
            }
            
            fullContent += codeBaseResult.content;

            const position = editor.selection.isEmpty ? editor.selection.active : editor.selection.end;
            await editor.edit(editBuilder => {
                editBuilder.insert(position, fullContent);
            });

            vscode.window.showInformationMessage(`TSI Header: Added ${detectedLanguageId} code base to current file`);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to add code base: ${error.message}`);
        }
    });
    
    context.subscriptions.push(addClassCommand);
    context.subscriptions.push(addCodeBaseCommand);
}

module.exports = { register };
