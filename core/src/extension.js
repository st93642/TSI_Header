/*****************************************************************************/
/*                                                                           */
/*  extension.js                                         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 23 2025 11:39 st93642                      TT    SSSSSSS II */
/*  Updated: Oct 01 2025 10:18 st93642                                       */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

const vscode = require('vscode');
const { execSync } = require('child_process');
const path = require('path');

// Import the core interface
const { TSICore } = require('../index');
// Import feature modules
// Code quality enforcement module removed
// Import project creator
const { createTSIProject } = require('../generators/project/projectCreator');
// Import language-specific file creator
const { createLanguageSpecificFiles } = require('../generators/project/projectcreators/index');
// Import build system generator
const { createBuildFiles } = require('../generators/project/buildSystemGenerator');
// Import documentation generator
const { createDocumentationFiles } = require('../generators/project/documentationGenerator');
// Import git ignore generator
const { createGitIgnoreFile } = require('../generators/project/gitIgnoreGenerator');
// Import study mode extension
const { StudyModeExtension } = require('./studyModeExtension');
// Import tree data providers
const { TSITreeDataProvider, TSIProjectDataProvider } = require('./tsiViewProvider');

function activate(context) {
    // Initialize core interface
    const core = new TSICore(context.extensionPath);

    // Initialize feature modules
    // Code quality enforcement module removed

    // Initialize Study Mode extension
    const studyModeExtension = new StudyModeExtension(vscode, context);
    studyModeExtension.activate();

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
            
            // Detect correct language based on file extension for unsupported languages
            let detectedLanguageId = languageId;
            const fileExtension = fileName.split('.').pop().toLowerCase();
            if (fileExtension === 'erb') {
                detectedLanguageId = 'erb';
            } else if (fileExtension === 'vue') {
                detectedLanguageId = 'vue';
            } else if (fileExtension === 'ejs') {
                detectedLanguageId = 'ejs';
            }
            
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
            const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
            
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
            const command = `ruby "${cliPath}" insert "${detectedLanguageId}" "${fileName}"`;
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
            
            // Detect correct language based on file extension for unsupported languages
            const detectedLanguageId = core.utils.detectLanguageFromExtension(languageId, fileName);
            
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
            const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
            
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

    // Register remove header command
    const removeHeaderCommand = vscode.commands.registerCommand('tsiheader.removeHeader', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        try {
            const document = editor.document;
            const text = document.getText();
            
            // Check if file has a TSI header using the same logic as auto-update
            const lines = text.split('\n');
            let hasHeader = false;
            let headerEndLine = -1;
            
            for (let i = 0; i < Math.min(15, lines.length); i++) {
                if (lines[i].includes('Transport and Telecommunication Institute')) {
                    hasHeader = true;
                    // Find the end of the header (empty line after header)
                    for (let j = i + 1; j < Math.min(i + 15, lines.length); j++) {
                        if (lines[j].trim() === '') {
                            headerEndLine = j;
                            break;
                        }
                    }
                    break;
                }
            }
            
            if (!hasHeader) {
                vscode.window.showErrorMessage('No TSI header found to remove in this file.');
                return;
            }
            
            // Remove the header (from start to headerEndLine)
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
            
            // Detect correct language based on file extension for unsupported languages
            const detectedLanguageId = core.utils.detectLanguageFromExtension(languageId, fileName);
            
            // Get Ruby CLI path
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
            
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
            const command = `ruby "${cliPath}" update "${detectedLanguageId}" "${fileName}"`;
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

        // Detect correct language based on file extension for unsupported languages
        const detectedLanguageId = core.utils.detectLanguageFromExtension(languageId, fileName);

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
            const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
            
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
            const headerCommand = `ruby "${cliPath}" insert "${detectedLanguageId}" "${fileName}"`;
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

            // Generate class code using core interface
            const classResult = core.codeGenerator.generateClass(detectedLanguageId, className, fileName, env);
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

            // Check if file has substantial content beyond header using core interface
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
                
                // Insert at cursor position for "Add at Cursor", or end of file for "Add at End"
                const position = choice === 'Add at Cursor' 
                    ? (editor.selection.isEmpty ? editor.selection.active : editor.selection.end)
                    : new vscode.Position(document.lineCount, 0);
                
                await editor.edit(editBuilder => {
                    editBuilder.insert(position, '\n' + fullContent);
                });
            } else {
                // File is mostly empty (just header), replace content after header
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

        // Detect correct language based on file extension for unsupported languages
        const detectedLanguageId = core.utils.detectLanguageFromExtension(languageId, fileName);

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
            const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
            
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
            const headerCommand = `ruby "${cliPath}" insert "${detectedLanguageId}" "${fileName}"`;
            const headerResult = execSync(headerCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
            const headerResponse = JSON.parse(headerResult);
            
            if (!headerResponse.success) {
                vscode.window.showErrorMessage(`Failed to generate header: ${headerResponse.message}`);
                return;
            }
            
            let fullContent = headerResponse.header;
            
            // Generate code structure using core interface
            const codeBaseResult = core.codeGenerator.generateCodeBase(detectedLanguageId, fileName);
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

            vscode.window.showInformationMessage(`TSI Header: Added ${detectedLanguageId} code base to current file`);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to add code base: ${error.message}`);
        }
    });

    context.subscriptions.push(insertHeaderCommand);
    context.subscriptions.push(updateHeaderCommand);
    context.subscriptions.push(removeHeaderCommand);
    context.subscriptions.push(addClassCommand);
    context.subscriptions.push(addCodeBaseCommand);
    context.subscriptions.push(onSaveListener);

    // Helper function to create project for specific language (skips language selection)
    async function createLanguageSpecificProject(language, uri) {
        try {
            // Skip language selection, go directly to project name input
            const projectName = await vscode.window.showInputBox({
                prompt: `Enter ${language.toUpperCase()} project name`,
                placeHolder: `my-${language}-project`,
                validateInput: (value) => {
                    if (!value || value.trim().length === 0) {
                        return 'Project name cannot be empty';
                    }
                    if (!/^[a-zA-Z0-9_-]+$/.test(value)) {
                        return 'Project name can only contain letters, numbers, hyphens, and underscores';
                    }
                    return null;
                }
            });
            
            if (!projectName) return;

            // Determine workspace location
            const workspaceUri = uri || await selectWorkspaceLocation();
            if (!workspaceUri) return;

            // Create project structure using the existing function
            await createProjectStructure(language, projectName, workspaceUri);
            
            // Show success message and open project
            vscode.window.showInformationMessage(
                `TSI ${language.toUpperCase()} project "${projectName}" created successfully!`,
                'Open Project'
            ).then(selection => {
                if (selection === 'Open Project') {
                    const projectUri = vscode.Uri.joinPath(workspaceUri, projectName);
                    vscode.commands.executeCommand('vscode.openFolder', projectUri);
                }
            });
            
        } catch (error) {
            console.error('TSI Project Creator Error:', error);
            vscode.window.showErrorMessage(`Failed to create TSI project: ${error.message}`);
        }
    }

    // Helper function to select workspace location (extracted from projectCreator.js)
    async function selectWorkspaceLocation() {
        // If we have workspace folders, use the first one
        if (vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 0) {
            return vscode.workspace.workspaceFolders[0].uri;
        }

        // Otherwise, show folder picker
        const uris = await vscode.window.showOpenDialog({
            canSelectFiles: false,
            canSelectFolders: true,
            canSelectMany: false,
            openLabel: 'Select Project Location'
        });

        return uris && uris.length > 0 ? uris[0] : undefined;
    }

    // Helper function to create project structure (imported from projectCreator.js)
    async function createProjectStructure(language, projectName, workspaceUri) {
        const projectUri = vscode.Uri.joinPath(workspaceUri, projectName);
        
        // Create base directories
        const directories = getDirectoryStructure(language);
        for (const dir of directories) {
            const dirUri = vscode.Uri.joinPath(projectUri, dir);
            await vscode.workspace.fs.createDirectory(dirUri);
        }
        
        // Generate main source file
        await createMainSourceFile(language, projectName, projectUri, vscode);
        
        // Create header file (for C/C++)
        if (language === 'c' || language === 'cpp') {
            await createHeaderFile(language, projectName, projectUri, vscode);
        }
        
        // Create language-specific project files
        await createLanguageSpecificFiles(language, projectName, projectUri, vscode);
        
        // Create build files (Makefiles, etc.)
        await createBuildFiles(language, projectName, projectUri);
        
        // Create documentation files (README.md)
        await createDocumentationFiles(language, projectName, projectUri, vscode);
        
        // Create .gitignore file
        await createGitIgnoreFile(language, projectUri, vscode);
    }

    // Helper function to get directory structure
    function getDirectoryStructure(language) {
        const commonDirs = ['src', 'docs'];
        
        if (language === 'c' || language === 'cpp') {
            return [...commonDirs, 'include', 'build'];
        } else if (language === 'python') {
            return [...commonDirs, 'tests', 'scripts'];
        } else if (language === 'java') {
            return [...commonDirs, 'src/main/java', 'src/test/java', 'target'];
        } else if (language === 'rust') {
            return [...commonDirs, 'src', 'tests', 'examples', 'benches'];
        } else if (language === 'ruby') {
            return [...commonDirs, 'lib', 'spec', 'bin', 'config'];
        } else if (language === 'php') {
            return [...commonDirs, 'src', 'public', 'tests'];
        } else if (language === 'html') {
            return [...commonDirs, 'src', 'assets', 'css', 'js'];
        }
        
        return commonDirs;
    }

    // Helper function to create main source file
    async function createMainSourceFile(language, projectName, projectUri, vscode) {
        const extension = getFileExtension(language);
        let fileName = `main.${extension}`;
        let fileUri;
        
        if (language === 'java') {
            // For Java, create Main.java in the proper package structure
            fileName = 'Main.java';
            fileUri = vscode.Uri.joinPath(projectUri, 'src', 'main', 'java', fileName);
        } else if (language === 'php') {
            // For PHP, create index.php in the public directory
            fileName = 'index.php';
            fileUri = vscode.Uri.joinPath(projectUri, 'public', fileName);
        } else {
            fileUri = vscode.Uri.joinPath(projectUri, 'src', fileName);
        }
        
        // Generate TSI header using Ruby CLI API
        const headerContent = await core.generateTSIHeaderContent(fileName, vscode);
        
        // Generate code base using core interface
        const codeResult = core.codeGenerator.generateCodeBase(language, fileName);
        const codeContent = codeResult.success ? codeResult.content : '';
        
        // Combine header and code
        const fullContent = headerContent + '\n' + codeContent;
        
        // Write to file
        const encoder = new TextEncoder();
        await vscode.workspace.fs.writeFile(fileUri, encoder.encode(fullContent));
    }

    // Helper function to create header file for C/C++
    async function createHeaderFile(language, projectName, projectUri, vscode) {
        const extension = language === 'c' ? 'h' : 'hpp';
        const fileName = `${projectName}.${extension}`;
        const fileUri = vscode.Uri.joinPath(projectUri, 'include', fileName);
        
        // Generate TSI header using Ruby CLI API
        const headerContent = await core.generateTSIHeaderContent(fileName, vscode);
        
        // Generate header guard
        const guardName = `${projectName.toUpperCase().replace(/-/g, '_')}_${extension.toUpperCase()}`;
        
        const content = `${headerContent}

#ifndef ${guardName}
#define ${guardName}

#ifdef __cplusplus
extern "C" {
#endif

// Function declarations go here

#ifdef __cplusplus
}
#endif

#endif // ${guardName}
`;
        
        const encoder = new TextEncoder();
        await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
    }

    // Helper function to generate test TSI header
    function generateTestTSIHeader() {
        const now = new Date();
        const dateStr = now.toLocaleDateString('en-US', {
            month: 'short',
            day: '2-digit',
            year: 'numeric'
        }).replace(',', '');
        
        const timeStr = now.toLocaleTimeString('en-US', {
            hour: '2-digit',
            minute: '2-digit',
            hour12: false
        });
        
        const username = 'Test User';
        const email = 'test@example.com';
        const dateTime = `${dateStr} ${timeStr}`;
        
        return `/******************************************************************************/
/*                                                                           */
/*  file.c                               TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: ${email.padEnd(50, ' ')} TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: ${dateTime} ${username.padEnd(30, ' ')} TT    SSSSSSS II */
/*  Updated: ${dateTime} ${username.padEnd(30, ' ')}                          */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/******************************************************************************/
`;
    }

    // Helper function to get file extension
    function getFileExtension(language) {
        const extensions = {
            'c': 'c',
            'cpp': 'cpp',
            'python': 'py',
            'java': 'java',
            'rust': 'rs',
            'ruby': 'rb',
            'php': 'php',
            'html': 'html'
        };
        return extensions[language] || 'txt';
    }

    // Register create TSI project command
    const createTSIProjectCommand = vscode.commands.registerCommand('tsiheader.createTSIProject', createTSIProject);
    context.subscriptions.push(createTSIProjectCommand);

    // Register specific language project commands
    const createCProjectCommand = vscode.commands.registerCommand('tsiheader.createCProject', (uri) => createLanguageSpecificProject('c', uri));
    const createCppProjectCommand = vscode.commands.registerCommand('tsiheader.createCppProject', (uri) => createLanguageSpecificProject('cpp', uri));
    const createPythonProjectCommand = vscode.commands.registerCommand('tsiheader.createPythonProject', (uri) => createLanguageSpecificProject('python', uri));
    const createJavaProjectCommand = vscode.commands.registerCommand('tsiheader.createJavaProject', (uri) => createLanguageSpecificProject('java', uri));
    const createRustProjectCommand = vscode.commands.registerCommand('tsiheader.createRustProject', (uri) => createLanguageSpecificProject('rust', uri));
    const createRubyProjectCommand = vscode.commands.registerCommand('tsiheader.createRubyProject', (uri) => createLanguageSpecificProject('ruby', uri));
    const createPhpProjectCommand = vscode.commands.registerCommand('tsiheader.createPhpProject', (uri) => createLanguageSpecificProject('php', uri));
    const createHtmlProjectCommand = vscode.commands.registerCommand('tsiheader.createHtmlProject', (uri) => createLanguageSpecificProject('html', uri));

    context.subscriptions.push(createCProjectCommand);
    context.subscriptions.push(createCppProjectCommand);
    context.subscriptions.push(createPythonProjectCommand);
    context.subscriptions.push(createJavaProjectCommand);
    context.subscriptions.push(createRustProjectCommand);
    context.subscriptions.push(createRubyProjectCommand);
    context.subscriptions.push(createPhpProjectCommand);
    context.subscriptions.push(createHtmlProjectCommand);

    // Register TSI Tree View Providers
    const tsiCommandsProvider = new TSITreeDataProvider();
    const tsiProjectsProvider = new TSIProjectDataProvider();
    
    vscode.window.registerTreeDataProvider('tsi-commands', tsiCommandsProvider);
    vscode.window.registerTreeDataProvider('tsi-projects', tsiProjectsProvider);
    
    // Add refresh commands for the views
    const refreshCommandsCommand = vscode.commands.registerCommand('tsiheader.refreshCommands', () => {
        tsiCommandsProvider.refresh();
    });
    
    const refreshProjectsCommand = vscode.commands.registerCommand('tsiheader.refreshProjects', () => {
        tsiProjectsProvider.refresh();
    });
    
    context.subscriptions.push(refreshCommandsCommand);
    context.subscriptions.push(refreshProjectsCommand);

    // Register Learn commands
    const learnRubyCommand = vscode.commands.registerCommand('tsiheader.learnRuby', async () => {
        vscode.window.showInformationMessage(
            '📚 Ruby Learning Resources',
            'Official Ruby Docs',
            'Ruby Tutorial',
            'TSI Ruby Course'
        ).then(selection => {
            if (selection === 'Official Ruby Docs') {
                vscode.env.openExternal(vscode.Uri.parse('https://www.ruby-lang.org/en/documentation/'));
            } else if (selection === 'Ruby Tutorial') {
                vscode.env.openExternal(vscode.Uri.parse('https://www.tutorialspoint.com/ruby/'));
            } else if (selection === 'TSI Ruby Course') {
                vscode.window.showInformationMessage('TSI Ruby course materials coming soon!');
            }
        });
    });
    
    context.subscriptions.push(learnRubyCommand);

    // Register feature module commands
    // Code quality enforcement module removed
    // context.subscriptions.push(diagnosticCollection);

    // Set up real-time diagnostics for open documents
    // Code quality enforcement module removed
    // vscode.workspace.onDidOpenTextDocument(document => {
    //     codeQualityModule.updateDiagnostics(document, diagnosticCollection);
    // });

    // vscode.workspace.onDidChangeTextDocument(event => {
    //     codeQualityModule.updateDiagnostics(event.document, diagnosticCollection);
    // });

    // vscode.workspace.onDidCloseTextDocument(document => {
    //     diagnosticCollection.delete(document.uri);
    // });

    // Analyze currently open documents
    // Code quality enforcement module removed
    // vscode.workspace.textDocuments.forEach(document => {
    //     codeQualityModule.updateDiagnostics(document, diagnosticCollection);
    // });
}

function deactivate() {}

module.exports = {
    activate,
    deactivate
};