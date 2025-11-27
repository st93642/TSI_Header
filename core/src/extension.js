/*****************************************************************************/
/*                                                                           */
/*  extension.js                                         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Oct 19 2025 15:36 st93642                      TT    SSSSSSS II */
/*  Updated: Oct 20 2025 18:22 st93642                                       */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

const vscode = require('vscode');
const { execSync } = require('child_process');
const path = require('path');

// Import the core interface
const { TSICore } = require('../index');
const { ChatManager } = require(path.join(__dirname, '..', '..', 'chat', 'src'));
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
// Import calendar module (lazy-loaded)
// const { CalendarManager } = require('../../calendar/src');
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

    // Initialize Calendar module (lazy-loaded)
    let calendarManager = null;
    try {
        const { CalendarManager } = require(path.join(__dirname, '..', '..', 'calendar', 'src'));
        calendarManager = new CalendarManager(context);
        calendarManager.initialize();
        console.log('Calendar module initialized successfully');
    } catch (error) {
        console.error('Failed to initialize calendar module:', error);
    }

    // Initialize Chat manager
    let chatManager = null;
    try {
        chatManager = new ChatManager(context);
        chatManager.initialize();
        console.log('Chat manager initialized successfully');
    } catch (error) {
        console.error('Failed to initialize chat manager:', error);
    }

    // Register test notification command (available even if calendar fails to initialize)
    const testNotificationCommand = vscode.commands.registerCommand('tsiheader.testNotification', async () => {
        try {
            const config = vscode.workspace.getConfiguration('tsiheader');
            const enableEmail = config.get('notifications.enableEmail', false);
            const notificationService = config.get('notifications.emailService');
            const email = config.get('notifications.emailAddress');
            
            if (!enableEmail) {
                vscode.window.showErrorMessage(
                    'Email notifications are disabled. Please enable tsiheader.notifications.enableEmail in settings.',
                    'Open Settings'
                ).then(selection => {
                    if (selection === 'Open Settings') {
                        vscode.commands.executeCommand('workbench.action.openSettings', '@ext:st93642.uni-header notifications');
                    }
                });
                return;
            }
            
            if (!notificationService || !email) {
                vscode.window.showErrorMessage(
                    'Notification settings not configured. Please set tsiheader.notifications.emailService and tsiheader.notifications.emailAddress in settings.',
                    'Open Settings'
                ).then(selection => {
                    if (selection === 'Open Settings') {
                        vscode.commands.executeCommand('workbench.action.openSettings', '@ext:st93642.uni-header notifications');
                    }
                });
                return;
            }

            if (!calendarManager || !calendarManager.webviewProvider || !calendarManager.webviewProvider.notificationService) {
                vscode.window.showErrorMessage(
                    'Calendar module not available. The test notification feature requires the calendar module to be properly initialized.',
                    'OK'
                );
                return;
            }
            
            // Create a test event for tomorrow
            const tomorrow = new Date();
            tomorrow.setDate(tomorrow.getDate() + 1);
            tomorrow.setHours(9, 0, 0, 0); // 9 AM tomorrow
            
            const testEvent = {
                id: 'test-notification-' + Date.now(),
                title: 'Test Notification Event',
                type: 'custom',
                start: tomorrow.toISOString(),
                description: 'This is a test notification to verify your email settings are working correctly.',
                category: 'Other'
            };
            
            // Send test notification
            const notificationResult = await calendarManager.webviewProvider.notificationService.sendNotification(testEvent, 'Test notification for TSI Header Calendar');
            
            if (notificationResult.success) {
                vscode.window.showInformationMessage('✅ Test notification sent successfully! Check your email.');
            } else {
                vscode.window.showErrorMessage(`❌ Test notification failed: ${notificationResult.error}`);
            }
        } catch (error) {
            console.error('Test notification error:', error);
            vscode.window.showErrorMessage(`❌ Test notification error: ${error.message}`);
        }
    });
    
    context.subscriptions.push(testNotificationCommand);

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
                    vscode.commands.executeCommand('workbench.action.openSettings', `@ext:st93642.uni-header`);
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
            const enableCustomHeader = config.get('customHeader.enableCustomHeader', false);
            const institutionName = config.get('customHeader.institutionName', 'Transport and Telecommunication Institute - Riga, Latvia');
            const institutionUrl = config.get('customHeader.institutionUrl', 'https://tsi.lv');
            
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
            
            // Set custom header environment variables
            if (enableCustomHeader) {
                env.TSI_CUSTOM_HEADER_ENABLED = 'true';
                if (institutionName && institutionName.trim() !== '') {
                    env.TSI_CUSTOM_INSTITUTION_NAME = institutionName;
                }
                if (institutionUrl && institutionUrl.trim() !== '') {
                    env.TSI_CUSTOM_INSTITUTION_URL = institutionUrl;
                }
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
            const enableCustomHeader = config.get('customHeader.enableCustomHeader', false);
            const institutionName = config.get('customHeader.institutionName', 'Transport and Telecommunication Institute - Riga, Latvia');
            const institutionUrl = config.get('customHeader.institutionUrl', 'https://tsi.lv');
            
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
            
            // Set custom header environment variables
            if (enableCustomHeader) {
                env.TSI_CUSTOM_HEADER_ENABLED = 'true';
                if (institutionName && institutionName.trim() !== '') {
                    env.TSI_CUSTOM_INSTITUTION_NAME = institutionName;
                }
                if (institutionUrl && institutionUrl.trim() !== '') {
                    env.TSI_CUSTOM_INSTITUTION_URL = institutionUrl;
                }
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
            
            // Check if file has a header produced by this extension
            const lines = text.split('\n');
            let hasHeader = false;
            let headerEndLine = -1;
            
            // Look for header pattern: starts with comment delimiter + asterisks (top border)
            for (let i = 0; i < Math.min(15, lines.length); i++) {
                const line = lines[i].trim();
                // Check for various header top border patterns for all supported languages:
                // /* ******** */ (C-style: c, cpp, java, javascript, etc.)
                // #*********# (Hash-style: python, ruby, perl, etc.)
                // ;; ******** ;; (Semicolon-style: lisp, scheme, etc.)
                // (* ******** *) (Paren-style: mathematica, ocaml, etc.)
                // -- ******** -- (Dash-style: haskell, lua, etc.)
                // %% ******** %% (Percent-style: latex, matlab, etc.)
                // <!-- ******** --> (HTML-style: html, xml, etc.)
                // <# ******** #> (Block-style: jinja, etc.)
                // { ******** } (Brace-style: pascal, etc.)
                // " ******** " (Quote-style: smalltalk, etc.)
                // // ******** // (Double-slash: labview, etc.)
                // <!--- ******** ---> (ColdFusion-style)
                // ! ******** ! (Exclamation-style: factor, etc.)
                // * ******** * (Asterisk-style: abap)
                // ; ******** ; (Single-semicolon: algol, etc.)
                // {# ******** #} (Twig-style)
                // -# ******** -# (HAML-style)
                // {{!-- ******** --}} (Handlebars-style)
                // @* ******** *@ (Razor-style)
                // ********** (Plain text: json, markdown, etc.)
                if (line.match(/^\/\*[\*]+\*\/$/) ||  // /* ******** */
                    line.match(/^#[\*]+#$/) ||        // #*********#
                    line.match(/^;; [\*]+ ;;$/) ||    // ;; ******** ;;
                    line.match(/^\(\* [\*]+ \*\)$/) || // (* ******** *)
                    line.match(/^-- [\*]+ --$/) ||    // -- ******** --
                    line.match(/^%% [\*]+ %%$/) ||    // %% ******** %%
                    line.match(/^<!-- [\*]+ -->$/) || // <!-- ******** -->
                    line.match(/^<# [\*]+ #>$/) ||    // <# ******** #>
                    line.match(/^{ [\*]+ }$/) ||      // { ******** }
                    line.match(/^" [\*]+ "$/) ||      // " ******** "
                    line.match(/^\/\/ [\*]+ \/\/$/) || // // ******** //
                    line.match(/^<!--- [\*]+ --->$/) || // <!--- ******** --->
                    line.match(/^! [\*]+ !$/) ||      // ! ******** !
                    line.match(/^\* [\*]+ \*$/) ||    // * ******** *
                    line.match(/^; [\*]+ ;$/) ||      // ; ******** ;
                    line.match(/^{# [\*]+ #}$/) ||    // {# ******** #}
                    line.match(/^-# [\*]+ -#$/) ||    // -# ******** -#
                    line.match(/^{{\!-- [\*]+ --}}$/) || // {{!-- ******** --}}
                    line.match(/^@\* [\*]+ \*@$/) ||  // @* ******** *@
                    line.match(/^[\*]{10,}$/)) {      // ********** (plain text)
                    hasHeader = true;
                    
                    // Find the end of the header (bottom border line)
                    for (let j = i + 1; j < Math.min(i + 20, lines.length); j++) {
                        const checkLine = lines[j].trim();
                        // Look for bottom border (similar pattern to top border)
                        if (checkLine.match(/^\/\*[\*]+\*\/$/) ||  // /* ******** */
                            checkLine.match(/^#[\*]+#$/) ||        // #*********#
                            checkLine.match(/^;; [\*]+ ;;$/) ||    // ;; ******** ;;
                            checkLine.match(/^\(\* [\*]+ \*\)$/) || // (* ******** *)
                            checkLine.match(/^-- [\*]+ --$/) ||    // -- ******** --
                            checkLine.match(/^%% [\*]+ %%$/) ||    // %% ******** %%
                            checkLine.match(/^<!-- [\*]+ -->$/) || // <!-- ******** -->
                            checkLine.match(/^<# [\*]+ #>$/) ||    // <# ******** #>
                            checkLine.match(/^{ [\*]+ }$/) ||      // { ******** }
                            checkLine.match(/^" [\*]+ "$/) ||      // " ******** "
                            checkLine.match(/^\/\/ [\*]+ \/\/$/) || // // ******** //
                            checkLine.match(/^<!--- [\*]+ --->$/) || // <!--- ******** --->
                            checkLine.match(/^! [\*]+ !$/) ||      // ! ******** !
                            checkLine.match(/^\* [\*]+ \*$/) ||    // * ******** *
                            checkLine.match(/^; [\*]+ ;$/) ||      // ; ******** ;
                            checkLine.match(/^{# [\*]+ #}$/) ||    // {# ******** #}
                            checkLine.match(/^-# [\*]+ -#$/) ||    // -# ******** -#
                            checkLine.match(/^{{\!-- [\*]+ --}}$/) || // {{!-- ******** --}}
                            checkLine.match(/^@\* [\*]+ \*@$/) ||  // @* ******** *@
                            checkLine.match(/^[\*]{10,}$/)) {      // ********** (plain text)
                            headerEndLine = j;
                            break;
                        }
                    }
                    break;
                }
            }
            
            if (!hasHeader) {
                vscode.window.showErrorMessage('No header found to remove in this file.');
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
        
        // Look for header pattern in first few lines
        let hasHeader = false;
        for (let i = 0; i < Math.min(15, lines.length); i++) {
            const line = lines[i].trim();
            // Check for various header top border patterns for all supported languages:
            // /* ******** */ (C-style: c, cpp, java, javascript, etc.)
            // #*********# (Hash-style: python, ruby, perl, etc.)
            // ;; ******** ;; (Semicolon-style: lisp, scheme, etc.)
            // (* ******** *) (Paren-style: mathematica, ocaml, etc.)
            // -- ******** -- (Dash-style: haskell, lua, etc.)
            // %% ******** %% (Percent-style: latex, matlab, etc.)
            // <!-- ******** --> (HTML-style: html, xml, etc.)
            // <# ******** #> (Block-style: jinja, etc.)
            // { ******** } (Brace-style: pascal, etc.)
            // " ******** " (Quote-style: smalltalk, etc.)
            // // ******** // (Double-slash: labview, etc.)
            // <!--- ******** ---> (ColdFusion-style)
            // ! ******** ! (Exclamation-style: factor, etc.)
            // * ******** * (Asterisk-style: abap)
            // ; ******** ; (Single-semicolon: algol, etc.)
            // {# ******** #} (Twig-style)
            // -# ******** -# (HAML-style)
            // {{!-- ******** --}} (Handlebars-style)
            // @* ******** *@ (Razor-style)
            // ********** (Plain text: json, markdown, etc.)
            if (line.match(/^\/\*[\*]+\*\/$/) ||  // /* ******** */
                line.match(/^#[\*]+#$/) ||        // #*********#
                line.match(/^;; [\*]+ ;;$/) ||    // ;; ******** ;;
                line.match(/^\(\* [\*]+ \*\)$/) || // (* ******** *)
                line.match(/^-- [\*]+ --$/) ||    // -- ******** --
                line.match(/^%% [\*]+ %%$/) ||    // %% ******** %%
                line.match(/^<!-- [\*]+ -->$/) || // <!-- ******** -->
                line.match(/^<# [\*]+ #>$/) ||    // <# ******** #>
                line.match(/^{ [\*]+ }$/) ||      // { ******** }
                line.match(/^" [\*]+ "$/) ||      // " ******** "
                line.match(/^\/\/ [\*]+ \/\/$/) || // // ******** //
                line.match(/^<!--- [\*]+ --->$/) || // <!--- ******** --->
                line.match(/^! [\*]+ !$/) ||      // ! ******** !
                line.match(/^\* [\*]+ \*$/) ||    // * ******** *
                line.match(/^; [\*]+ ;$/) ||      // ; ******** ;
                line.match(/^{# [\*]+ #}$/) ||    // {# ******** #}
                line.match(/^-# [\*]+ -#$/) ||    // -# ******** -#
                line.match(/^{{\!-- [\*]+ --}}$/) || // {{!-- ******** --}}
                line.match(/^@\* [\*]+ \*@$/) ||  // @* ******** *@
                line.match(/^[\*]{10,}$/)) {      // ********** (plain text)
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
        const enableCustomHeader = config.get('customHeader.enableCustomHeader', false);
        const institutionName = config.get('customHeader.institutionName', 'Transport and Telecommunication Institute - Riga, Latvia');
        const institutionUrl = config.get('customHeader.institutionUrl', 'https://tsi.lv');
        
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
            
            // Set custom header environment variables
            if (enableCustomHeader) {
                env.TSI_CUSTOM_HEADER_ENABLED = 'true';
                if (institutionName && institutionName.trim() !== '') {
                    env.TSI_CUSTOM_INSTITUTION_NAME = institutionName;
                }
                if (institutionUrl && institutionUrl.trim() !== '') {
                    env.TSI_CUSTOM_INSTITUTION_URL = institutionUrl;
                }
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
        // Lazy load the Learn module
        const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);
        
        // Show centered modal dialog
        vscode.window.showInformationMessage(
            '📚 Start Ruby Learning Journey?\n\n' +
            'You will begin an interactive Ruby programming course with:\n' +
            '• 6 modules from beginner to advanced\n' +
            '• 18 lessons with hands-on exercises\n' +
            '• Progress tracking and achievements\n' +
            '• Instant feedback on your code\n\n' +
            'Ready to start learning?',
            { modal: true },
            'Start Learning',
            'Browse Lessons',
            'View Progress',
            'Cancel'
        ).then(async selection => {
            if (selection === 'Start Learning') {
                await learnInstance.startLearning('ruby');
            } else if (selection === 'Browse Lessons') {
                await learnInstance.browseLessons('ruby');
            } else if (selection === 'View Progress') {
                try {
                    const stats = await learnInstance.getStats('ruby');
                    vscode.window.showInformationMessage(
                        `📊 Your Ruby Learning Progress\n\n` +
                        `Lessons Completed: ${stats.lessonsCompleted}\n` +
                        `Exercises Completed: ${stats.exercisesCompleted}\n` +
                        `Current Streak: ${stats.currentStreak} days\n` +
                        `Study Time: ${stats.totalStudyTime} minutes\n` +
                        `Achievements: ${stats.achievements}`,
                        { modal: true },
                        'Continue Learning',
                        'Got it!'
                    ).then(choice => {
                        if (choice === 'Continue Learning') {
                            learnInstance.startLearning('ruby');
                        }
                    });
                } catch (error) {
                    console.error('Error getting progress stats:', error);
                    vscode.window.showErrorMessage(
                        `Error loading progress: ${error.message}`,
                        { modal: true },
                        'OK'
                    );
                }
            }
        });
    });
    
    context.subscriptions.push(learnRubyCommand);

    // Register Learn Rust command
    const learnRustCommand = vscode.commands.registerCommand('tsiheader.learnRust', async () => {
        const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);

        vscode.window.showInformationMessage(
            '📚 Start Rust Learning Journey?\n\n' +
            'You will begin a concise Rust course with:\n' +
            '• A small module and a hands-on exercise\n' +
            '• Progress tracking and instant feedback\n\n' +
            'Ready to start learning?',
            { modal: true },
            'Start Learning',
            'Browse Lessons',
            'View Progress',
            'Cancel'
        ).then(async selection => {
            if (selection === 'Start Learning') {
                await learnInstance.startLearning('rust');
            } else if (selection === 'Browse Lessons') {
                await learnInstance.browseLessons('rust');
            } else if (selection === 'View Progress') {
                try {
                    const stats = await learnInstance.getStats('rust');
                    vscode.window.showInformationMessage(
                        `📊 Your Rust Learning Progress\n\n` +
                        `Lessons Completed: ${stats.lessonsCompleted}\n` +
                        `Exercises Completed: ${stats.exercisesCompleted}\n` +
                        `Current Streak: ${stats.currentStreak} days\n` +
                        `Study Time: ${stats.totalStudyTime} minutes\n` +
                        `Achievements: ${stats.achievements}`,
                        { modal: true },
                        'Continue Learning',
                        'Got it!'
                    ).then(choice => {
                        if (choice === 'Continue Learning') {
                            learnInstance.startLearning('rust');
                        }
                    });
                } catch (error) {
                    console.error('Error getting rust progress stats:', error);
                    vscode.window.showErrorMessage(`Error loading progress: ${error.message}`, { modal: true }, 'OK');
                }
            }
        });
    });

    context.subscriptions.push(learnRustCommand);

    // Register Browse Lessons command
    const browseLessonsCommand = vscode.commands.registerCommand('tsiheader.browseLessons', async () => {
        // Lazy load the Learn module
        const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);
        
        // For now, only Ruby is supported
        await learnInstance.browseLessons('ruby');
    });
    
    context.subscriptions.push(browseLessonsCommand);

    // Register View Learn Progress command
    const viewLearnProgressCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgress', async () => {
        // Lazy load the Learn module
        const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);
        
        try {
            const stats = await learnInstance.getStats('ruby');
            vscode.window.showInformationMessage(
                `📊 Your Ruby Learning Progress\n\n` +
                `Lessons Completed: ${stats.lessonsCompleted}\n` +
                `Exercises Completed: ${stats.exercisesCompleted}\n` +
                `Current Streak: ${stats.currentStreak} days\n` +
                `Study Time: ${stats.totalStudyTime} minutes\n` +
                `Achievements: ${stats.achievements}`,
                { modal: true },
                'Continue Learning',
                'Browse Lessons',
                'Got it!'
            ).then(async choice => {
                if (choice === 'Continue Learning') {
                    await learnInstance.startLearning('ruby');
                } else if (choice === 'Browse Lessons') {
                    await learnInstance.browseLessons('ruby');
                }
            });
        } catch (error) {
            console.error('Error getting progress stats:', error);
            vscode.window.showErrorMessage(
                `Error loading progress: ${error.message}`,
                { modal: true },
                'OK'
            );
        }
    });
    
    context.subscriptions.push(viewLearnProgressCommand);

    // Register Browse Lessons Rust command
    const browseLessonsRustCommand = vscode.commands.registerCommand('tsiheader.browseLessonsRust', async () => {
        const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);
        await learnInstance.browseLessons('rust');
    });
    context.subscriptions.push(browseLessonsRustCommand);

    // Register View Learn Progress Rust command
    const viewLearnProgressRustCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressRust', async () => {
        const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);
        try {
            const stats = await learnInstance.getStats('rust');
            vscode.window.showInformationMessage(
                `📊 Your Rust Learning Progress\n\n` +
                `Lessons Completed: ${stats.lessonsCompleted}\n` +
                `Exercises Completed: ${stats.exercisesCompleted}\n` +
                `Current Streak: ${stats.currentStreak} days\n` +
                `Study Time: ${stats.totalStudyTime} minutes\n` +
                `Achievements: ${stats.achievements}`,
                { modal: true },
                'Continue Learning',
                'Browse Lessons',
                'Got it!'
            ).then(async choice => {
                if (choice === 'Continue Learning') {
                    await learnInstance.startLearning('rust');
                } else if (choice === 'Browse Lessons') {
                    await learnInstance.browseLessons('rust');
                }
            });
        } catch (error) {
            console.error('Error getting rust progress stats:', error);
            vscode.window.showErrorMessage(`Error loading progress: ${error.message}`, { modal: true }, 'OK');
        }
    });
    context.subscriptions.push(viewLearnProgressRustCommand);

    // Register Learn exercise test command
    const runExerciseTestsCommand = vscode.commands.registerCommand('tsiheader.runExerciseTests', async (runtimeLanguageArg, exerciseMetaArg) => {
        let normalizedRuntimeArg = runtimeLanguageArg;
        let normalizedExerciseMeta = exerciseMetaArg;

        if (runtimeLanguageArg && typeof runtimeLanguageArg === 'object' && !Array.isArray(runtimeLanguageArg)) {
            normalizedRuntimeArg = runtimeLanguageArg.language || runtimeLanguageArg.lang || undefined;
            normalizedExerciseMeta = runtimeLanguageArg.exerciseMetadata || runtimeLanguageArg.metadata || exerciseMetaArg;
        }

        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showWarningMessage(
                'No Exercise File Open\n\nPlease open your exercise file before running tests.',
                { modal: true },
                'Got it!'
            );
            return;
        }

        const filePath = editor.document.fileName;
        const fileName = path.basename(filePath);
        
        // Check if this is a learn exercise file
        if (!filePath.includes('learn_exercises')) {
            vscode.window.showWarningMessage(
                'Not a Learn Exercise\n\nThis command only works with Learn exercise files.\nExercise files are located in the learn_exercises/ folder.',
                { modal: true },
                'Got it!'
            );
            return;
        }

        // Lazy load the Learn module
        const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);

        // Determine language from file path
        const pathParts = filePath.split(path.sep);
        const learnExercisesIndex = pathParts.indexOf('learn_exercises');
        let runtimeLanguage = normalizedRuntimeArg || pathParts[learnExercisesIndex + 1];

        try {
            const fs = require('fs');
            let exerciseMetadata = normalizedExerciseMeta || learnInstance.getExerciseMetadata(filePath);

            if (!runtimeLanguage && exerciseMetadata?.variantLanguage) {
                runtimeLanguage = exerciseMetadata.variantLanguage;
            }

            let baseExerciseId;
            let curriculumLanguage = exerciseMetadata?.curriculumLanguage || pathParts[learnExercisesIndex - 1] || runtimeLanguage;
            let variantId;

            let exerciseJsonPath;
            let exerciseData;

            if (exerciseMetadata) {
                baseExerciseId = exerciseMetadata.baseExerciseId || exerciseMetadata.baseExerciseFile || fileName.replace(/\.[^/.]+$/, '');
                variantId = exerciseMetadata.variantId || fileName.replace(/\.[^/.]+$/, '');
                exerciseJsonPath = path.join(__dirname, '..', '..', 'learn', 'curriculum', curriculumLanguage, 'exercises', `${exerciseMetadata.baseExerciseFile || baseExerciseId}.json`);
            } else {
                const exerciseId = fileName.replace(/\.(rb|py|js|cpp|c)$/, '');
                baseExerciseId = exerciseId;
                variantId = exerciseId;
                exerciseJsonPath = path.join(__dirname, '..', '..', 'learn', 'curriculum', runtimeLanguage, 'exercises', `${exerciseId}.json`);
                curriculumLanguage = runtimeLanguage;
            }

            if (!fs.existsSync(exerciseJsonPath)) {
                await vscode.window.showErrorMessage(
                    `❌ Exercise Not Found\n\nCouldn't find exercise definition: ${path.basename(exerciseJsonPath)}`,
                    { modal: true },
                    'Got it!'
                );
                return;
            }

            exerciseData = JSON.parse(fs.readFileSync(exerciseJsonPath, 'utf8'));

            let variant = null;
            if (Array.isArray(exerciseData.variants) && exerciseData.variants.length > 0) {
                variant = exerciseData.variants.find(v => v.id === variantId) || exerciseData.variants[0];
                if (!runtimeLanguage && variant?.language) {
                    runtimeLanguage = variant.language;
                }
            }

            const executionLanguage = (variant?.language || exerciseMetadata?.variantLanguage || runtimeLanguage || curriculumLanguage || 'cpp').toLowerCase();
            const runnerTests = variant?.tests || exerciseData.tests || [];
            const exerciseTitle = variant?.title || exerciseData.title || exerciseMetadata?.title || baseExerciseId;
            const exerciseDescription = variant?.description || exerciseData.description || '';
            const exerciseHints = variant?.hints || exerciseData.hints || [];
            const exerciseDifficulty = variant?.difficulty || exerciseMetadata?.difficulty || exerciseData.difficulty || 'beginner';
            const progressId = variant?.id || exerciseData.id || baseExerciseId;
            const normalizeLessonId = (value) => {
                if (!value) {
                    return null;
                }
                let normalized = value.toString();
                normalized = normalized.replace(/_exercise$/, '');
                normalized = normalized.replace(/_solution$/, '');
                normalized = normalized.replace(/_variant$/, '');
                return normalized;
            };

            const baseExerciseForLesson = exerciseMetadata?.baseExerciseFile || baseExerciseId;
            const lessonId = normalizeLessonId(
                exerciseMetadata?.lessonId ||
                baseExerciseForLesson ||
                progressId ||
                variant?.id
            );

            const exercise = {
                id: progressId,
                baseExerciseId,
                baseExerciseFile: exerciseMetadata?.baseExerciseFile || baseExerciseId,
                variantId: variant?.id,
                title: exerciseTitle,
                description: exerciseDescription,
                language: executionLanguage,
                curriculumLanguage,
                tests: runnerTests,
                hints: exerciseHints,
                difficulty: exerciseDifficulty,
                progressId,
                hasVariants: Array.isArray(exerciseData.variants) && exerciseData.variants.length > 0,
                lessonId
            };

            // Run with progress indicator
            await vscode.window.withProgress({
                location: vscode.ProgressLocation.Notification,
                title: "🔄 Running Tests...",
                cancellable: false
            }, async (progress) => {
                progress.report({ message: "Testing your solution..." });
                
                try {
                    // Run the exercise - this will show modal dialogs
                    const result = await learnInstance.runExercise(curriculumLanguage, exercise);
                    console.log('Exercise result:', result);
                    return result;
                } catch (error) {
                    console.error('Exercise run error:', error);
                    throw error;
                }
            });
            
        } catch (error) {
            console.error('Test execution error:', error);
            await vscode.window.showErrorMessage(
                `❌ Test Execution Failed\n\n${error.message}\n\n${error.stack || ''}`,
                { modal: true },
                'Got it!'
            );
        }
    });

    context.subscriptions.push(runExerciseTestsCommand);

    // C Learning Commands
    const learnCCommand = vscode.commands.registerCommand('tsiheader.learnC', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            vscode.window.showInformationMessage(
                '⚙️ Start C Learning Journey?\n\n' +
                'Launch an interactive C curriculum featuring:\n' +
                '• Core concepts of memory, pointers, and data structures\n' +
                '• Hands-on exercises with automated feedback\n' +
                '• Progress tracking, streaks, and achievements\n\n' +
                'Ready to begin?',
                { modal: true },
                'Start Learning',
                'Browse Lessons',
                'View Progress',
                'Cancel'
            ).then(async selection => {
                if (selection === 'Start Learning') {
                    await learnInstance.startLearning('c');
                } else if (selection === 'Browse Lessons') {
                    await learnInstance.browseLessons('c');
                } else if (selection === 'View Progress') {
                    try {
                        const stats = await learnInstance.getStats('c');
                        vscode.window.showInformationMessage(
                            `📊 Your C Learning Progress\n\n` +
                            `Lessons Completed: ${stats.lessonsCompleted}\n` +
                            `Exercises Completed: ${stats.exercisesCompleted}\n` +
                            `Current Streak: ${stats.currentStreak} days\n` +
                            `Study Time: ${stats.totalStudyTime} minutes\n` +
                            `Achievements: ${stats.achievements}`,
                            { modal: true },
                            'Continue Learning',
                            'Got it!'
                        ).then(choice => {
                            if (choice === 'Continue Learning') {
                                learnInstance.startLearning('c');
                            }
                        });
                    } catch (progressError) {
                        vscode.window.showErrorMessage(`Error loading progress: ${progressError.message}`, { modal: true }, 'OK');
                    }
                }
            });
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting C learning: ${error.message}`);
        }
    });

    context.subscriptions.push(learnCCommand);

    const browseLessonsCCommand = vscode.commands.registerCommand('tsiheader.browseLessonsC', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            await learnInstance.browseLessons('c');
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing C lessons: ${error.message}`);
        }
    });

    context.subscriptions.push(browseLessonsCCommand);

    const viewLearnProgressCCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressC', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            const stats = await learnInstance.getStats('c');

            const message = `📊 C Learning Progress\n\n`
                + `Lessons Completed: ${stats.lessonsCompleted}\n`
                + `Exercises Completed: ${stats.exercisesCompleted}\n`
                + `Current Streak: ${stats.currentStreak} days\n`
                + `Total Study Time: ${stats.totalStudyTime} minutes\n`
                + `Achievements: ${stats.achievements}\n\n`
                + `Keep up the great work! 🚀`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Continue Learning',
                'View All Lessons'
            );

            if (action === 'Continue Learning') {
                await learnInstance.startLearning('c');
            } else if (action === 'View All Lessons') {
                await learnInstance.browseLessons('c');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing C progress: ${error.message}`);
        }
    });

    context.subscriptions.push(viewLearnProgressCCommand);

    const runExerciseTestsCCommand = vscode.commands.registerCommand('tsiheader.runExerciseTestsC', async () => {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor) {
            vscode.window.showWarningMessage('Please open a C exercise file first.');
            return;
        }

        const filePath = activeEditor.document.fileName;
        const isCFile = filePath.endsWith('.c');
        if (!filePath.includes('learn_exercises') || !isCFile) {
            vscode.window.showWarningMessage('Please open a C exercise file (.c) from the learn_exercises directory.');
            return;
        }

        await vscode.commands.executeCommand('tsiheader.runExerciseTests', { language: 'c' });
    });

    context.subscriptions.push(runExerciseTestsCCommand);

    // C++ Learning Commands
    const learnCppCommand = vscode.commands.registerCommand('tsiheader.learnCpp', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            vscode.window.showInformationMessage(
                '⚙️ Start C++ Learning Journey?\n\n' +
                'Launch an interactive C++ curriculum featuring:\n' +
                '• 8 modules covering fundamentals to advanced topics\n' +
                '• Hands-on exercises with automated feedback\n' +
                '• Progress tracking, streaks, and achievements\n\n' +
                'Ready to begin?',
                { modal: true },
                'Start Learning',
                'Browse Lessons',
                'View Progress',
                'Cancel'
            ).then(async selection => {
                if (selection === 'Start Learning') {
                    await learnInstance.startLearning('cpp');
                } else if (selection === 'Browse Lessons') {
                    await learnInstance.browseLessons('cpp');
                } else if (selection === 'View Progress') {
                    try {
                        const stats = await learnInstance.getStats('cpp');
                        vscode.window.showInformationMessage(
                            `📊 Your C++ Learning Progress\n\n` +
                            `Lessons Completed: ${stats.lessonsCompleted}\n` +
                            `Exercises Completed: ${stats.exercisesCompleted}\n` +
                            `Current Streak: ${stats.currentStreak} days\n` +
                            `Study Time: ${stats.totalStudyTime} minutes\n` +
                            `Achievements: ${stats.achievements}`,
                            { modal: true },
                            'Continue Learning',
                            'Got it!'
                        ).then(choice => {
                            if (choice === 'Continue Learning') {
                                learnInstance.startLearning('cpp');
                            }
                        });
                    } catch (progressError) {
                        vscode.window.showErrorMessage(`Error loading progress: ${progressError.message}`, { modal: true }, 'OK');
                    }
                }
            });
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting C++ learning: ${error.message}`);
        }
    });

    context.subscriptions.push(learnCppCommand);

    const browseLessonsCppCommand = vscode.commands.registerCommand('tsiheader.browseLessonsCpp', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            await learnInstance.browseLessons('cpp');
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing C++ lessons: ${error.message}`);
        }
    });

    context.subscriptions.push(browseLessonsCppCommand);

    const viewLearnProgressCppCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressCpp', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            const stats = await learnInstance.getStats('cpp');

            const message = `📊 C++ Learning Progress\n\n`
                + `Lessons Completed: ${stats.lessonsCompleted}\n`
                + `Exercises Completed: ${stats.exercisesCompleted}\n`
                + `Current Streak: ${stats.currentStreak} days\n`
                + `Total Study Time: ${stats.totalStudyTime} minutes\n`
                + `Achievements: ${stats.achievements}\n\n`
                + `Keep up the great work! 🚀`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Continue Learning',
                'View All Lessons'
            );

            if (action === 'Continue Learning') {
                await learnInstance.startLearning('cpp');
            } else if (action === 'View All Lessons') {
                await learnInstance.browseLessons('cpp');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing C++ progress: ${error.message}`);
        }
    });

    context.subscriptions.push(viewLearnProgressCppCommand);

    const runExerciseTestsCppCommand = vscode.commands.registerCommand('tsiheader.runExerciseTestsCpp', async () => {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor) {
            vscode.window.showWarningMessage('Please open a C++ exercise file first.');
            return;
        }

        const filePath = activeEditor.document.fileName;
        const isCppFile = filePath.endsWith('.cpp');
        if (!filePath.includes('learn_exercises') || !isCppFile) {
            vscode.window.showWarningMessage('Please open a C++ exercise file (.cpp) from the learn_exercises directory.');
            return;
        }

        await vscode.commands.executeCommand('tsiheader.runExerciseTests');
    });

    context.subscriptions.push(runExerciseTestsCppCommand);

    const learnCppDsaCommand = vscode.commands.registerCommand('tsiheader.learnCppDsa', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            const message = '🚀 C++ DSA Roadmap\n\n' +
                'Dive into a dedicated Data Structures & Algorithms journey built for modern C++:\n' +
                '• Foundations, arrays, linked lists, and more\n' +
                '• Sorting, trees, graphs, flows, and optimization\n' +
                '• Every lesson ships with rich visuals, 300+ lines of code, and a 10-question quiz\n\n' +
                'Choose how you want to begin.';

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Start Roadmap',
                'Browse Lessons',
                'View Progress'
            );

            if (action === 'Start Roadmap') {
                await learnInstance.startLearning('dsa_cpp');
            } else if (action === 'Browse Lessons') {
                await learnInstance.browseLessons('dsa_cpp');
            } else if (action === 'View Progress') {
                await vscode.commands.executeCommand('tsiheader.viewLearnProgressCppDsa');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting C++ DSA roadmap: ${error.message}`);
        }
    });

    context.subscriptions.push(learnCppDsaCommand);

    const browseLessonsCppDsaCommand = vscode.commands.registerCommand('tsiheader.browseLessonsCppDsa', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            await learnInstance.browseLessons('dsa_cpp');
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing C++ DSA lessons: ${error.message}`);
        }
    });

    context.subscriptions.push(browseLessonsCppDsaCommand);

    const viewLearnProgressCppDsaCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressCppDsa', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            const curriculum = await learnInstance.learnManager.loadCurriculum('dsa_cpp');
            const modules = Array.isArray(curriculum.modules) ? curriculum.modules : [];
            const allLessons = modules.flatMap(module => module.lessons || []);

            if (allLessons.length === 0) {
                vscode.window.showWarningMessage('No lessons were found in the C++ DSA roadmap.');
                return;
            }

            const progress = await learnInstance.progressTracker.getProgress('dsa_cpp');
            const normalize = id => learnInstance.progressTracker.normalizeLessonId(id);
            const completedLessons = new Set((progress.completed || [])
                .map(id => normalize(id))
                .filter(Boolean));

            const totalLessons = allLessons.length;
            const completedCount = allLessons.reduce((count, lesson) => {
                const normalizedId = normalize(lesson.id);
                return normalizedId && completedLessons.has(normalizedId) ? count + 1 : count;
            }, 0);

            const remainingCount = totalLessons - completedCount;
            const completionRate = totalLessons === 0 ? 0 : Math.round((completedCount / totalLessons) * 100);

            const nextLesson = allLessons.find(lesson => {
                const normalizedId = normalize(lesson.id);
                return !(normalizedId && completedLessons.has(normalizedId));
            }) || null;

            const messageLines = [
                '📊 C++ DSA Roadmap Progress',
                '',
                `Modules: ${modules.length}`,
                `Lessons Completed: ${completedCount}/${totalLessons} (${completionRate}%)`,
                `Remaining Lessons: ${remainingCount}`
            ];

            if (nextLesson) {
                messageLines.push(`Next Recommended Lesson: ${nextLesson.title || nextLesson.id}`);
            } else {
                messageLines.push('🎉 You have completed every lesson in the roadmap!');
            }

            const stats = await learnInstance.getStats('dsa_cpp');
            messageLines.push('', `Exercises Completed: ${stats.exercisesCompleted}`);
            messageLines.push(`Current Streak: ${stats.currentStreak} days`);
            messageLines.push(`Total Study Time: ${stats.totalStudyTime} minutes`);

            const message = messageLines.join('\n');

            const buttons = [];
            if (nextLesson) {
                buttons.push('Start Next Lesson');
            }
            buttons.push('Browse Lessons');

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                ...buttons
            );

            if (action === 'Start Next Lesson') {
                await learnInstance.startLearning('dsa_cpp');
            } else if (action === 'Browse Lessons') {
                await learnInstance.browseLessons('dsa_cpp');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing C++ DSA progress: ${error.message}`);
        }
    });

    context.subscriptions.push(viewLearnProgressCppDsaCommand);

    const learnGitCommand = vscode.commands.registerCommand('tsiheader.learnGit', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            // Lazy load the Git Book manager with progress tracker
            const GitBookManager = require(path.join(__dirname, '..', '..', 'learn', 'git_manager.js'));
            const gitManager = new GitBookManager(vscode, learnInstance.progressTracker);

            const message = '📖 Pro Git Book - Complete Git Reference\n\n' +
                'Master Git with the official comprehensive guide:\n' +
                '• Getting Started: Installation, setup, and basics\n' +
                '• Git Fundamentals: Repository management, commits, history\n' +
                '• Branching & Merging: Workflows, rebasing, conflict resolution\n' +
                '• Git Server: Hosting, collaboration, and remote management\n' +
                '• Advanced Topics: Internals, customization, and automation\n\n' +
                'Lessons are fetched live from git-scm.com/book.\n\n' +
                'Ready to master Git?';

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Start Getting Started',
                'Browse All Chapters',
                'View Progress'
            );

            if (action === 'Start Getting Started') {
                // Load curriculum and start with first lesson in Getting Started chapter
                const fs = require('fs');
                const curriculumPath = path.join(__dirname, '..', '..', 'learn', 'curriculum', 'git', 'curriculum.json');
                const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));
                const gettingStartedChapter = curriculum.chapters.find(chapter => chapter.id === 'getting-started');
                if (gettingStartedChapter && gettingStartedChapter.lessons && gettingStartedChapter.lessons.length > 0) {
                    const firstLesson = gettingStartedChapter.lessons[0];
                    await gitManager.openLesson(firstLesson, context);
                } else {
                    vscode.window.showErrorMessage('No lessons found in Getting Started chapter');
                }
            } else if (action === 'Browse All Chapters') {
                await vscode.commands.executeCommand('tsiheader.browseLessonsGit');
            } else if (action === 'View Progress') {
                await vscode.commands.executeCommand('tsiheader.viewLearnProgressGit');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting Git Book: ${error.message}`);
        }
    });

    context.subscriptions.push(learnGitCommand);

    const browseLessonsGitCommand = vscode.commands.registerCommand('tsiheader.browseLessonsGit', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            // Lazy load the Git Book manager with progress tracker
            const GitBookManager = require(path.join(__dirname, '..', '..', 'learn', 'git_manager.js'));
            const gitManager = new GitBookManager(vscode, learnInstance.progressTracker);

            const fs = require('fs');
            const curriculumPath = path.join(__dirname, '..', '..', 'learn', 'curriculum', 'git', 'curriculum.json');
            const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));

            // Get progress to show completion status
            const progress = await gitManager.getProgressStats();
            const completedLessons = new Set(progress.completed || []);

            // Create quick pick items for all lessons
            const items = [];
            curriculum.chapters.forEach(chapter => {
                // Add chapter separator
                items.push({
                    label: `📚 ${chapter.title}`,
                    kind: vscode.QuickPickItemKind.Separator
                });

                // Add lessons within the chapter
                if (chapter.lessons && chapter.lessons.length > 0) {
                    chapter.lessons.forEach(lesson => {
                        const isCompleted = completedLessons.has(lesson.id);
                        const icon = isCompleted ? '✅' : '○';
                        items.push({
                            label: `    ${icon} ${lesson.title}`,
                            description: `${lesson.estimatedHours}h`,
                            detail: isCompleted ? 'Completed' : 'Not started',
                            lesson: lesson
                        });
                    });
                }
            });

            const selected = await vscode.window.showQuickPick(items, {
                placeHolder: 'Select a lesson from Pro Git Book',
                matchOnDescription: true
            });

            if (selected && selected.lesson) {
                await gitManager.openLesson(selected.lesson, context);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing Git lessons: ${error.message}`);
        }
    });

    context.subscriptions.push(browseLessonsGitCommand);

    const viewLearnProgressGitCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressGit', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            // Lazy load the Git Book manager with progress tracker
            const GitBookManager = require(path.join(__dirname, '..', '..', 'learn', 'git_manager.js'));
            const gitManager = new GitBookManager(vscode, learnInstance.progressTracker);

            // Get actual progress statistics
            const stats = await gitManager.getProgressStats();

            // Load curriculum to get total lesson count
            const fs = require('fs');
            const curriculumPath = path.join(__dirname, '..', '..', 'learn', 'curriculum', 'git', 'curriculum.json');
            const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));

            // Count total lessons in curriculum
            let totalLessons = 0;
            curriculum.chapters.forEach(chapter => {
                if (chapter.lessons) {
                    totalLessons += chapter.lessons.length;
                }
            });

            const completionRate = totalLessons > 0 ? Math.round((stats.lessonsCompleted / totalLessons) * 100) : 0;

            const message = `📊 Pro Git Book Progress\n\n` +
                `📚 Lessons Completed: ${stats.lessonsCompleted}/${totalLessons} (${completionRate}%)\n` +
                `🔥 Current Streak: ${stats.currentStreak} days\n` +
                `⏱️ Total Study Time: ${stats.totalStudyTime} minutes\n` +
                `🏆 Achievements: ${stats.achievements}\n` +
                `📅 Last Study Date: ${stats.lastStudyDate}\n\n` +
                `Keep up the great work mastering Git! 🚀`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Continue Learning',
                'Browse Chapters',
                'Got it!'
            );

            if (action === 'Continue Learning') {
                // Start with Getting Started if no progress, otherwise continue from current progress
                if (stats.lessonsCompleted === 0) {
                    const gettingStartedChapter = curriculum.chapters.find(chapter => chapter.id === 'getting-started');
                    if (gettingStartedChapter && gettingStartedChapter.lessons && gettingStartedChapter.lessons.length > 0) {
                        const firstLesson = gettingStartedChapter.lessons[0];
                        await gitManager.openLesson(firstLesson, context);
                    }
                } else {
                    // For now, just open browse lessons - could be enhanced to find next lesson
                    await vscode.commands.executeCommand('tsiheader.browseLessonsGit');
                }
            } else if (action === 'Browse Chapters') {
                await vscode.commands.executeCommand('tsiheader.browseLessonsGit');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing Git progress: ${error.message}`);
        }
    });

    context.subscriptions.push(viewLearnProgressGitCommand);

    // Git Book Cache Management Commands
    const clearGitCacheCommand = vscode.commands.registerCommand('tsiheader.clearGitCache', async () => {
        try {
            // Lazy load the Git Book manager
            const GitBookManager = require(path.join(__dirname, '..', '..', 'learn', 'git_manager.js'));
            const gitManager = new GitBookManager(vscode);

            const confirmed = await vscode.window.showWarningMessage(
                'Clear Git Book Cache?\n\n' +
                'This will delete all cached lessons. You will need to re-download lessons from git-scm.com next time you access them.\n\n' +
                'Cached lessons allow offline access when you don\'t have internet connection.',
                { modal: true },
                'Clear Cache',
                'Cancel'
            );

            if (confirmed === 'Clear Cache') {
                const success = await gitManager.clearLessonCache();
                if (success) {
                    vscode.window.showInformationMessage('✅ Git Book cache cleared successfully!');
                } else {
                    vscode.window.showErrorMessage('❌ Failed to clear Git Book cache');
                }
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error clearing cache: ${error.message}`);
        }
    });

    context.subscriptions.push(clearGitCacheCommand);

    const viewGitCacheStatsCommand = vscode.commands.registerCommand('tsiheader.viewGitCacheStats', async () => {
        try {
            // Lazy load the Git Book manager
            const GitBookManager = require(path.join(__dirname, '..', '..', 'learn', 'git_manager.js'));
            const gitManager = new GitBookManager(vscode);

            const stats = await gitManager.getCacheStats();

            const formatSize = (bytes) => {
                if (bytes === 0) return '0 B';
                const k = 1024;
                const sizes = ['B', 'KB', 'MB', 'GB'];
                const i = Math.floor(Math.log(bytes) / Math.log(k));
                return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
            };

            const formatDate = (dateStr) => {
                if (!dateStr) return 'Never';
                return new Date(dateStr).toLocaleDateString();
            };

            const message = `📊 Git Book Cache Statistics\n\n` +
                `📚 Cached Lessons: ${stats.totalLessons}\n` +
                `💾 Cache Size: ${formatSize(stats.totalSize)}\n` +
                `📅 Oldest Cache: ${formatDate(stats.oldestCache)}\n` +
                `🆕 Newest Cache: ${formatDate(stats.newestCache)}\n\n` +
                `Cache helps provide offline access to lessons you've previously viewed.`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Clear Cache',
                'Got it!'
            );

            if (action === 'Clear Cache') {
                await vscode.commands.executeCommand('tsiheader.clearGitCache');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing cache stats: ${error.message}`);
        }
    });

    context.subscriptions.push(viewGitCacheStatsCommand);

    // Odin Project Learning Commands
    const learnOdinCommand = vscode.commands.registerCommand('tsiheader.learnOdin', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            
            // Lazy load the Odin Project manager with progress tracker
            const OdinProjectManager = require(path.join(__dirname, '..', '..', 'top', 'odin_manager.js'));
            const odinManager = new OdinProjectManager(vscode, learnInstance.progressTracker);

            const message = '🚀 The Odin Project - Full Stack JavaScript\n\n' +
                'Embark on a comprehensive journey to become a full-stack developer:\n' +
                '• Foundations: HTML, CSS, JavaScript, Git\n' +
                '• Full Stack JavaScript: Node.js, Express, React, MongoDB\n' +
                '• Ruby on Rails: Complete web application development\n' +
                '• 300+ hours of interactive curriculum\n\n' +
                'Lessons are fetched live from The Odin Project website.\n\n' +
                'Ready to start your coding journey?';

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Start Foundations',
                'Browse All Lessons',
                'View Progress'
            );

            if (action === 'Start Foundations') {
                // Load curriculum and start with first lesson in Foundations course
                const fs = require('fs');
                const curriculumPath = path.join(__dirname, '..', '..', 'top', 'curriculum.json');
                const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));
                const foundationsPath = curriculum.paths.find(path => path.id === 'foundations');
                if (foundationsPath && foundationsPath.courses.length > 0) {
                    const foundationsCourse = foundationsPath.courses[0];
                    if (foundationsCourse.lessons && foundationsCourse.lessons.length > 0) {
                        const firstLesson = foundationsCourse.lessons[0];
                        await odinManager.openLesson(firstLesson, context);
                    } else {
                        vscode.window.showErrorMessage('No lessons found in Foundations course');
                    }
                } else {
                    vscode.window.showErrorMessage('Foundations path not found in curriculum');
                }
            } else if (action === 'Browse All Lessons') {
                await vscode.commands.executeCommand('tsiheader.browseLessonsOdin');
            } else if (action === 'View Progress') {
                await vscode.commands.executeCommand('tsiheader.viewLearnProgressOdin');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting Odin Project: ${error.message}`);
        }
    });

    context.subscriptions.push(learnOdinCommand);

    const browseLessonsOdinCommand = vscode.commands.registerCommand('tsiheader.browseLessonsOdin', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            
            // Lazy load the Odin Project manager with progress tracker
            const OdinProjectManager = require(path.join(__dirname, '..', '..', 'top', 'odin_manager.js'));
            const odinManager = new OdinProjectManager(vscode, learnInstance.progressTracker);
            
            const fs = require('fs');
            const curriculumPath = path.join(__dirname, '..', '..', 'top', 'curriculum.json');
            const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));

            // Get progress to show completion status
            const progress = await odinManager.getProgressStats();
            const completedLessons = new Set(progress.completed || []);

            // Create quick pick items for all lessons
            const items = [];
            curriculum.paths.forEach(path => {
                // Add path separator
                items.push({
                    label: `📚 ${path.title}`,
                    kind: vscode.QuickPickItemKind.Separator
                });

                path.courses.forEach(course => {
                    // Add course as separator
                    items.push({
                        label: `  📖 ${course.title}`,
                        kind: vscode.QuickPickItemKind.Separator
                    });

                    // Add lessons within the course
                    if (course.lessons && course.lessons.length > 0) {
                        course.lessons.forEach(lesson => {
                            const isCompleted = completedLessons.has(lesson.id);
                            const icon = isCompleted ? '✅' : '○';
                            items.push({
                                label: `    ${icon} ${lesson.title}`,
                                description: `${lesson.estimatedHours}h`,
                                detail: isCompleted ? 'Completed' : 'Not started',
                                lesson: lesson
                            });
                        });
                    }
                });
            });

            const selected = await vscode.window.showQuickPick(items, {
                placeHolder: 'Select a lesson from The Odin Project',
                matchOnDescription: true
            });

            if (selected && selected.lesson) {
                await odinManager.openLesson(selected.lesson, context);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing Odin lessons: ${error.message}`);
        }
    });

    context.subscriptions.push(browseLessonsOdinCommand);

    const viewLearnProgressOdinCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressOdin', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            
            // Lazy load the Odin Project manager with progress tracker
            const OdinProjectManager = require(path.join(__dirname, '..', '..', 'top', 'odin_manager.js'));
            const odinManager = new OdinProjectManager(vscode, learnInstance.progressTracker);
            
            // Get actual progress statistics
            const stats = await odinManager.getProgressStats();
            
            // Load curriculum to get total lesson count
            const fs = require('fs');
            const curriculumPath = path.join(__dirname, '..', '..', 'top', 'curriculum.json');
            const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));
            
            // Count total lessons in curriculum
            let totalLessons = 0;
            curriculum.paths.forEach(path => {
                path.courses.forEach(course => {
                    if (course.lessons) {
                        totalLessons += course.lessons.length;
                    }
                });
            });
            
            const completionRate = totalLessons > 0 ? Math.round((stats.lessonsCompleted / totalLessons) * 100) : 0;
            
            const message = `📊 The Odin Project Progress\n\n` +
                `📚 Lessons Completed: ${stats.lessonsCompleted}/${totalLessons} (${completionRate}%)\n` +
                `🔥 Current Streak: ${stats.currentStreak} days\n` +
                `⏱️ Total Study Time: ${stats.totalStudyTime} minutes\n` +
                `🏆 Achievements: ${stats.achievements}\n` +
                `📅 Last Study Date: ${stats.lastStudyDate}\n\n` +
                `Keep up the great work on your full-stack development journey! 🚀`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Continue Learning',
                'Browse Lessons',
                'Got it!'
            );

            if (action === 'Continue Learning') {
                // Start with foundations if no progress, otherwise continue from current progress
                if (stats.lessonsCompleted === 0) {
                    const foundationsPath = curriculum.paths.find(path => path.id === 'foundations');
                    if (foundationsPath && foundationsPath.courses.length > 0) {
                        const foundationsCourse = foundationsPath.courses[0];
                        if (foundationsCourse.lessons && foundationsCourse.lessons.length > 0) {
                            const firstLesson = foundationsCourse.lessons[0];
                            await odinManager.openLesson(firstLesson, context);
                        }
                    }
                } else {
                    // For now, just open browse lessons - could be enhanced to find next lesson
                    await vscode.commands.executeCommand('tsiheader.browseLessonsOdin');
                }
            } else if (action === 'Browse Lessons') {
                await vscode.commands.executeCommand('tsiheader.browseLessonsOdin');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing Odin progress: ${error.message}`);
        }
    });

    context.subscriptions.push(viewLearnProgressOdinCommand);

    // Odin Project Cache Management Commands
    const clearOdinCacheCommand = vscode.commands.registerCommand('tsiheader.clearOdinCache', async () => {
        try {
            // Lazy load the Odin Project manager
            const OdinProjectManager = require(path.join(__dirname, '..', '..', 'top', 'odin_manager.js'));
            const odinManager = new OdinProjectManager(vscode);

            const confirmed = await vscode.window.showWarningMessage(
                'Clear Odin Project Cache?\n\n' +
                'This will delete all cached lessons. You will need to re-download lessons from The Odin Project website next time you access them.\n\n' +
                'Cached lessons allow offline access when you don\'t have internet connection.',
                { modal: true },
                'Clear Cache',
                'Cancel'
            );

            if (confirmed === 'Clear Cache') {
                const success = await odinManager.clearLessonCache();
                if (success) {
                    vscode.window.showInformationMessage('✅ Odin Project cache cleared successfully!');
                } else {
                    vscode.window.showErrorMessage('❌ Failed to clear Odin Project cache');
                }
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error clearing cache: ${error.message}`);
        }
    });

    context.subscriptions.push(clearOdinCacheCommand);

    const viewOdinCacheStatsCommand = vscode.commands.registerCommand('tsiheader.viewOdinCacheStats', async () => {
        try {
            // Lazy load the Odin Project manager
            const OdinProjectManager = require(path.join(__dirname, '..', '..', 'top', 'odin_manager.js'));
            const odinManager = new OdinProjectManager(vscode);

            const stats = await odinManager.getCacheStats();

            const formatSize = (bytes) => {
                if (bytes === 0) return '0 B';
                const k = 1024;
                const sizes = ['B', 'KB', 'MB', 'GB'];
                const i = Math.floor(Math.log(bytes) / Math.log(k));
                return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
            };

            const formatDate = (dateStr) => {
                if (!dateStr) return 'Never';
                return new Date(dateStr).toLocaleDateString();
            };

            const message = `📊 Odin Project Cache Statistics\n\n` +
                `📚 Cached Lessons: ${stats.totalLessons}\n` +
                `💾 Cache Size: ${formatSize(stats.totalSize)}\n` +
                `📅 Oldest Cache: ${formatDate(stats.oldestCache)}\n` +
                `🆕 Newest Cache: ${formatDate(stats.newestCache)}\n\n` +
                `Cache helps provide offline access to lessons you've previously viewed.`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Clear Cache',
                'Got it!'
            );

            if (action === 'Clear Cache') {
                await vscode.commands.executeCommand('tsiheader.clearOdinCache');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing cache stats: ${error.message}`);
        }
    });

    context.subscriptions.push(viewOdinCacheStatsCommand);

    // Mathematics Learning Commands
    const learnMathematicsCommand = vscode.commands.registerCommand('tsiheader.learnMathematics', async () => {
        try {
            // Lazy load the Mathematics manager
            const MathematicsManager = require(path.join(__dirname, '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);

            const workbooks = await mathManager.getWorkbooks();
            
            vscode.window.showInformationMessage(
                '🔢 Start Higher Mathematics Learning?\n\n' +
                'Explore advanced mathematics with HELM workbooks:\n' +
                '• Matrices fundamentals and operations\n' +
                '• Matrix solution of equations\n' +
                '• Interactive exercises and quizzes\n' +
                '• PDF workbooks with detailed explanations\n\n' +
                'Ready to begin your mathematics journey?',
                { modal: true },
                'Browse Workbooks',
                'Cancel'
            ).then(async selection => {
                if (selection === 'Browse Workbooks') {
                    // Show available workbooks
                    const workbookItems = workbooks.map(workbook => ({
                        label: `📚 ${workbook.title}`,
                        description: 'Open PDF workbook',
                        workbook: workbook
                    }));
                    
                    const selected = await vscode.window.showQuickPick(workbookItems, {
                        placeHolder: 'Select a mathematics workbook'
                    });
                    
                    if (selected) {
                        await mathManager.openWorkbook(selected.workbook);
                    }
                }
            });
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting mathematics learning: ${error.message}`);
        }
    });

    context.subscriptions.push(learnMathematicsCommand);

    const browseMathematicsWorkbooksCommand = vscode.commands.registerCommand('tsiheader.browseMathematicsWorkbooks', async () => {
        try {
            const MathematicsManager = require(path.join(__dirname, '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);
            
            const workbooks = await mathManager.getWorkbooks();
            const workbookItems = workbooks.map(workbook => ({
                label: `📚 ${workbook.title}`,
                description: 'Open PDF workbook',
                workbook: workbook
            }));
            
            const selected = await vscode.window.showQuickPick(workbookItems, {
                placeHolder: 'Select a mathematics workbook to open'
            });
            
            if (selected) {
                await mathManager.openWorkbook(selected.workbook);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing mathematics workbooks: ${error.message}`);
        }
    });

    context.subscriptions.push(browseMathematicsWorkbooksCommand);

    const takeMathematicsQuizCommand = vscode.commands.registerCommand('tsiheader.takeMathematicsQuiz', async () => {
        try {
            const MathematicsManager = require(path.join(__dirname, '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);
            
            const quizzes = await mathManager.getQuizzes();
            const quizItems = quizzes.map(quiz => ({
                label: `🧠 ${quiz.title}`,
                description: 'Test your knowledge',
                quiz: quiz
            }));
            
            const selected = await vscode.window.showQuickPick(quizItems, {
                placeHolder: 'Select a mathematics quiz'
            });
            
            if (selected) {
                // Load and display the quiz in webview
                const quizData = await mathManager.loadQuiz(selected.quiz.id);
                await mathManager.openQuiz(quizData);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error loading mathematics quiz: ${error.message}`);
        }
    });

    context.subscriptions.push(takeMathematicsQuizCommand);

    // Git quizzes - list and open interactive quizzes defined under learn/curriculum/git/exercises
    const takeGitQuizCommand = vscode.commands.registerCommand('tsiheader.takeGitQuiz', async () => {
        try {
            const fs = require('fs');
            const path = require('path');

            const quizzesDir = path.join(__dirname, '..', '..', 'learn', 'curriculum', 'git', 'exercises');

            // Read available quiz files
            let files = [];
            try {
                files = fs.readdirSync(quizzesDir).filter(f => f.endsWith('_quiz.json'));
            } catch (e) {
                // Directory missing or unreadable
                files = [];
            }

            if (files.length === 0) {
                vscode.window.showInformationMessage('No Git quizzes are available right now.');
                return;
            }

            const quizItems = files.map(filename => {
                const id = filename.replace('_quiz.json', '');
                const title = id.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase());
                return {
                    label: `🧠 ${title}`,
                    description: 'Test your Git knowledge',
                    filename,
                    id
                };
            });

            const selected = await vscode.window.showQuickPick(quizItems, { placeHolder: 'Select a Git quiz' });

            if (!selected) return;

            // Load the selected quiz JSON
            const quizPath = path.join(quizzesDir, selected.filename);
            const raw = fs.readFileSync(quizPath, 'utf8');
            const quizData = JSON.parse(raw);

            // Reuse MathematicsManager's openQuiz UI since it provides a solid quiz viewer
            const MathematicsManager = require(path.join(__dirname, '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);

            // Open the quiz using the quiz viewer
            await mathManager.openQuiz(quizData);

        } catch (error) {
            vscode.window.showErrorMessage(`Error loading Git quiz: ${error.message}`);
        }
    });

    context.subscriptions.push(takeGitQuizCommand);

    const viewMathematicsCacheStatsCommand = vscode.commands.registerCommand('tsiheader.viewMathematicsCacheStats', async () => {
        try {
            const MathematicsManager = require(path.join(__dirname, '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);
            
            const stats = await mathManager.getCacheStatistics();
            
            const message = `📊 Mathematics Cache Statistics\n\n` +
                `Workbooks Cached: ${stats.workbooks}\n` +
                `Cache Size: ${stats.size}\n` +
                `Oldest Cache: ${stats.oldest || 'None'}\n` +
                `Newest Cache: ${stats.newest || 'None'}\n\n` +
                `Cache helps reduce download times and enables offline access.`;
            
            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Clear Cache',
                'Close'
            );
            
            if (action === 'Clear Cache') {
                const confirm = await vscode.window.showWarningMessage(
                    'Clear all cached mathematics workbooks?',
                    { modal: true },
                    'Yes, Clear Cache',
                    'Cancel'
                );
                
                if (confirm === 'Yes, Clear Cache') {
                    const cleared = await mathManager.clearCache();
                    if (cleared) {
                        vscode.window.showInformationMessage('Mathematics cache cleared successfully!');
                    } else {
                        vscode.window.showErrorMessage('Failed to clear mathematics cache.');
                    }
                }
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing cache stats: ${error.message}`);
        }
    });

    context.subscriptions.push(viewMathematicsCacheStatsCommand);

    const clearMathematicsCacheCommand = vscode.commands.registerCommand('tsiheader.clearMathematicsCache', async () => {
        try {
            const MathematicsManager = require(path.join(__dirname, '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);
            
            const confirm = await vscode.window.showWarningMessage(
                'Clear all cached mathematics workbooks?\n\nThis will remove all downloaded PDFs and require re-downloading them.',
                { modal: true },
                'Yes, Clear Cache',
                'Cancel'
            );
            
            if (confirm === 'Yes, Clear Cache') {
                const cleared = await mathManager.clearCache();
                if (cleared) {
                    vscode.window.showInformationMessage('Mathematics cache cleared successfully!');
                } else {
                    vscode.window.showErrorMessage('Failed to clear mathematics cache.');
                }
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error clearing cache: ${error.message}`);
        }
    });

    context.subscriptions.push(clearMathematicsCacheCommand);

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