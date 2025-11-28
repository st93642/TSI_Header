const { TextEncoder } = require('util');

async function selectWorkspaceLocation(vscode) {
    if (vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 0) {
        return vscode.workspace.workspaceFolders[0].uri;
    }

    const uris = await vscode.window.showOpenDialog({
        canSelectFiles: false,
        canSelectFolders: true,
        canSelectMany: false,
        openLabel: 'Select Project Location'
    });

    return uris && uris.length > 0 ? uris[0] : undefined;
}

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

async function createMainSourceFile(deps, language, projectName, projectUri) {
    const { vscode, core } = deps;
    const extension = getFileExtension(language);
    let fileName = `main.${extension}`;
    let fileUri;
    
    if (language === 'java') {
        fileName = 'Main.java';
        fileUri = vscode.Uri.joinPath(projectUri, 'src', 'main', 'java', fileName);
    } else if (language === 'php') {
        fileName = 'index.php';
        fileUri = vscode.Uri.joinPath(projectUri, 'public', fileName);
    } else {
        fileUri = vscode.Uri.joinPath(projectUri, 'src', fileName);
    }
    
    const headerContent = await core.generateTSIHeaderContent(fileName, vscode);
    const codeResult = core.codeGenerator.generateCodeBase(language, fileName);
    const codeContent = codeResult.success ? codeResult.content : '';
    
    const fullContent = `${headerContent}\n${codeContent}`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(fullContent));
}

async function createHeaderFile(deps, language, projectName, projectUri) {
    const { vscode, core } = deps;
    const extension = language === 'c' ? 'h' : 'hpp';
    const fileName = `${projectName}.${extension}`;
    const fileUri = vscode.Uri.joinPath(projectUri, 'include', fileName);
    
    const headerContent = await core.generateTSIHeaderContent(fileName, vscode);
    
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

async function createProjectStructure(deps, language, projectName, workspaceUri) {
    const { vscode, createLanguageSpecificFiles, createBuildFiles, createDocumentationFiles, createGitIgnoreFile } = deps;
    const projectUri = vscode.Uri.joinPath(workspaceUri, projectName);
    
    const directories = getDirectoryStructure(language);
    for (const dir of directories) {
        const dirUri = vscode.Uri.joinPath(projectUri, dir);
        await vscode.workspace.fs.createDirectory(dirUri);
    }
    
    await createMainSourceFile(deps, language, projectName, projectUri);
    
    if (language === 'c' || language === 'cpp') {
        await createHeaderFile(deps, language, projectName, projectUri);
    }
    
    await createLanguageSpecificFiles(language, projectName, projectUri, vscode);
    
    await createBuildFiles(language, projectName, projectUri);
    
    await createDocumentationFiles(language, projectName, projectUri, vscode);
    
    await createGitIgnoreFile(language, projectUri, vscode);
}

async function createLanguageSpecificProject(deps, language, uri) {
    const { vscode } = deps;
    const selectLocation = deps.selectWorkspaceLocation || selectWorkspaceLocation;
    
    try {
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

        const workspaceUri = uri || await selectLocation(vscode);
        if (!workspaceUri) return;

        await createProjectStructure(deps, language, projectName, workspaceUri);
        
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

module.exports = {
    selectWorkspaceLocation,
    getDirectoryStructure,
    getFileExtension,
    createMainSourceFile,
    createHeaderFile,
    createProjectStructure,
    createLanguageSpecificProject
};
