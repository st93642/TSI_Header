/**
 * TSI Project Creator
 * Creates complete project structures with TSI headers and professional setup
 */

let vscode;
try {
    vscode = require('vscode');
} catch (e) {
    // vscode not available (running outside VS Code)
    vscode = null;
}
const path = require('path');
const { generateCodeBase } = require('../codeBaseGenerators');
const { createBuildFiles } = require('./buildSystemGenerator');
const { createDocumentationFiles } = require('./documentationGenerator');
const { createGitIgnoreFile } = require('./gitIgnoreGenerator');
const { createLanguageSpecificFiles } = require('./projectcreators/index');
const { generateTSIHeaderContent } = require('./headerUtils');

/**
 * Main function to create TSI project
 * Called from the VS Code command
 */
async function createTSIProject(uri) {
    if (!vscode) {
        throw new Error('VS Code API not available');
    }

    try {
        // Step 1: Show language selection
        const language = await showLanguageQuickPick();
        if (!language) return;

        // Step 2: Get project name
        const projectName = await showProjectNameInput();
        if (!projectName) return;

        // Step 3: Determine workspace location
        const workspaceUri = uri || await selectWorkspaceLocation();
        if (!workspaceUri) return;

        // Step 4: Create project structure
        await createProjectStructure(language, projectName, workspaceUri);
        
        // Step 5: Show success message and open project
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

/**
 * Show language selection QuickPick
 */
async function showLanguageQuickPick() {
    const languages = [
        {
            label: '$(symbol-class) C Project',
            description: 'Traditional C programming with GCC',
            detail: 'Creates: main.c, Makefile, headers, documentation',
            value: 'c'
        },
        {
            label: '$(symbol-class) C++ Project', 
            description: 'Modern C++ with object-oriented design',
            detail: 'Creates: main.cpp, classes, Makefile, documentation',
            value: 'cpp'
        },
        {
            label: '$(code) Python Project',
            description: 'Python scripting and development',
            detail: 'Creates: main.py, classes, requirements.txt, documentation',
            value: 'python'
        },
        {
            label: '$(tools) Java Project',
            description: 'Object-oriented Java development',
            detail: 'Creates: Main.java, classes, pom.xml, documentation',
            value: 'java'
        },
        {
            label: '$(package) Rust Project',
            description: 'Systems programming with memory safety',
            detail: 'Creates: main.rs, lib.rs, Cargo.toml, documentation',
            value: 'rust'
        },
        {
            label: '$(ruby) Ruby Project',
            description: 'Dynamic programming with elegant syntax',
            detail: 'Creates: main.rb, classes, Gemfile, documentation',
            value: 'ruby'
        },
        {
            label: '$(symbol-namespace) PHP Project',
            description: 'Server-side web development',
            detail: 'Creates: index.php, classes, composer.json, documentation',
            value: 'php'
        }
    ];

    const selected = await vscode.window.showQuickPick(languages, {
        placeHolder: 'Select programming language for your TSI project',
        matchOnDescription: true,
        matchOnDetail: true,
        canPickMany: false
    });

    return selected ? selected.value : undefined;
}

/**
 * Show project name input box
 */
async function showProjectNameInput() {
    return await vscode.window.showInputBox({
        prompt: 'Enter project name',
        placeHolder: 'my-tsi-project',
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
}

/**
 * Select workspace location
 */
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

/**
 * Create complete project structure
 */
async function createProjectStructure(language, projectName, workspaceUri) {
    const projectUri = vscode.Uri.joinPath(workspaceUri, projectName);
    
    // Create base directories
    const directories = getDirectoryStructure(language);
    for (const dir of directories) {
        const dirUri = vscode.Uri.joinPath(projectUri, dir);
        await vscode.workspace.fs.createDirectory(dirUri);
    }
    
    // Generate main source file
    await createMainSourceFile(language, projectName, projectUri);
    
    // Create header file (for C/C++)
    if (language === 'c' || language === 'cpp') {
        await createHeaderFile(language, projectName, projectUri);
    }
    
    // Create language-specific project files
    await createLanguageSpecificFiles(language, projectName, projectUri, vscode);
    
    // Create build system files
    await createBuildFiles(language, projectName, projectUri);
    
    // Create documentation
    await createDocumentationFiles(language, projectName, projectUri);
    
    // Create .gitignore
    await createGitIgnoreFile(language, projectUri);
}

/**
 * Get directory structure for language
 */
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
    }
    
    return commonDirs;
}

/**
 * Create main source file with TSI header and code
 */
async function createMainSourceFile(language, projectName, projectUri) {
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
    
    // Generate TSI header (we'll call the Ruby CLI)
    const headerContent = await generateTSIHeaderContent(fileName, vscode);
    
    // Generate code base using existing API
    const codeResult = generateCodeBase(language, fileName);
    const codeContent = codeResult.success ? codeResult.content : '';
    
    // Combine header and code
    const fullContent = headerContent + '\n' + codeContent;
    
    // Write to file
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(fullContent));
}

/**
 * Create header file for C/C++
 */
async function createHeaderFile(language, projectName, projectUri) {
    const extension = language === 'c' ? 'h' : 'hpp';
    const fileName = `${projectName}.${extension}`;
    const fileUri = vscode.Uri.joinPath(projectUri, 'include', fileName);
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent(fileName, vscode);
    
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


function getFileExtension(language) {
    const extensions = {
        'c': 'c',
        'cpp': 'cpp',
        'python': 'py',
        'java': 'java',
        'rust': 'rs',
        'ruby': 'rb',
        'php': 'php'
    };
    return extensions[language] || 'txt';
}

module.exports = {
    createTSIProject
};