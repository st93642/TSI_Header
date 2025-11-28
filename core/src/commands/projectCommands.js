const { createLanguageSpecificProject } = require('../projects/projectHelpers');

function register(context, deps) {
    const { vscode, core, createTSIProject, createLanguageSpecificFiles, createBuildFiles, createDocumentationFiles, createGitIgnoreFile, tsiCommandsProvider, tsiProjectsProvider } = deps;
    
    const projectDeps = {
        vscode,
        core,
        createLanguageSpecificFiles,
        createBuildFiles,
        createDocumentationFiles,
        createGitIgnoreFile
    };
    
    const createTSIProjectCommand = vscode.commands.registerCommand('tsiheader.createTSIProject', createTSIProject);
    context.subscriptions.push(createTSIProjectCommand);
    
    const createCProjectCommand = vscode.commands.registerCommand('tsiheader.createCProject', (uri) => 
        createLanguageSpecificProject(projectDeps, 'c', uri));
    const createCppProjectCommand = vscode.commands.registerCommand('tsiheader.createCppProject', (uri) => 
        createLanguageSpecificProject(projectDeps, 'cpp', uri));
    const createPythonProjectCommand = vscode.commands.registerCommand('tsiheader.createPythonProject', (uri) => 
        createLanguageSpecificProject(projectDeps, 'python', uri));
    const createJavaProjectCommand = vscode.commands.registerCommand('tsiheader.createJavaProject', (uri) => 
        createLanguageSpecificProject(projectDeps, 'java', uri));
    const createRustProjectCommand = vscode.commands.registerCommand('tsiheader.createRustProject', (uri) => 
        createLanguageSpecificProject(projectDeps, 'rust', uri));
    const createRubyProjectCommand = vscode.commands.registerCommand('tsiheader.createRubyProject', (uri) => 
        createLanguageSpecificProject(projectDeps, 'ruby', uri));
    const createPhpProjectCommand = vscode.commands.registerCommand('tsiheader.createPhpProject', (uri) => 
        createLanguageSpecificProject(projectDeps, 'php', uri));
    const createHtmlProjectCommand = vscode.commands.registerCommand('tsiheader.createHtmlProject', (uri) => 
        createLanguageSpecificProject(projectDeps, 'html', uri));

    context.subscriptions.push(createCProjectCommand);
    context.subscriptions.push(createCppProjectCommand);
    context.subscriptions.push(createPythonProjectCommand);
    context.subscriptions.push(createJavaProjectCommand);
    context.subscriptions.push(createRustProjectCommand);
    context.subscriptions.push(createRubyProjectCommand);
    context.subscriptions.push(createPhpProjectCommand);
    context.subscriptions.push(createHtmlProjectCommand);

    if (tsiCommandsProvider) {
        const refreshCommandsCommand = vscode.commands.registerCommand('tsiheader.refreshCommands', () => {
            tsiCommandsProvider.refresh();
        });
        context.subscriptions.push(refreshCommandsCommand);
    }

    if (tsiProjectsProvider) {
        const refreshProjectsCommand = vscode.commands.registerCommand('tsiheader.refreshProjects', () => {
            tsiProjectsProvider.refresh();
        });
        context.subscriptions.push(refreshProjectsCommand);
    }
}

module.exports = { register };
