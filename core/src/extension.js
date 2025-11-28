/*****************************************************************************/
/*                                                                           */
/*  extension.js                                         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Oct 19 2025 15:36 st93642                      TT    SSSSSSS II */
/*  Updated: Nov 28 2025 01:41 st93642                                       */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

const vscode = require('vscode');

const { ActivationManager } = require('./activation/activationManager');

const { createTSIProject } = require('../generators/project/projectCreator');
const { createLanguageSpecificFiles } = require('../generators/project/projectcreators/index');
const { createBuildFiles } = require('../generators/project/buildSystemGenerator');
const { createDocumentationFiles } = require('../generators/project/documentationGenerator');
const { createGitIgnoreFile } = require('../generators/project/gitIgnoreGenerator');

const commands = require('./commands');

function activate(context) {
    const activationManager = new ActivationManager(context);
    
    console.log('TSI Header: Activating extension');
    activationManager.registerViews();
    
    activationManager.initializeModules().catch(error => {
        console.error('Error during module initialization:', error);
    });
    
    const calendarManager = activationManager.getCalendarManager();
    const chatManager = activationManager.getChatManager();
    const core = activationManager.getCore();
    const studyModeExtension = activationManager.getStudyModeExtension();
    const tsiCommandsProvider = activationManager.getTSICommandsProvider();
    const tsiProjectsProvider = activationManager.getTSIProjectsProvider();
    
    const deps = {
        vscode,
        core,
        createTSIProject,
        createLanguageSpecificFiles,
        createBuildFiles,
        createDocumentationFiles,
        createGitIgnoreFile,
        tsiCommandsProvider,
        tsiProjectsProvider,
        calendarManager,
        chatManager,
        studyModeExtension
    };
    
    commands.registerAll(context, deps);
    
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
            
            const tomorrow = new Date();
            tomorrow.setDate(tomorrow.getDate() + 1);
            tomorrow.setHours(9, 0, 0, 0);
            
            const testEvent = {
                id: 'test-notification-' + Date.now(),
                title: 'Test Notification Event',
                type: 'custom',
                start: tomorrow.toISOString(),
                description: 'This is a test notification to verify your email settings are working correctly.',
                category: 'Other'
            };
            
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
}

function deactivate() {}

module.exports = {
    activate,
    deactivate
};
