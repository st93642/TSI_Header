/**
 * Project Creators Index
 * Unified interface for language-specific project file creation
 */

// Import language-specific project creators
const { createCFiles } = require('./cProjectCreator');
const { createCppFiles } = require('./cppProjectCreator');
const { createPythonFiles } = require('./pythonProjectCreator');
const { createJavaFiles } = require('./javaProjectCreator');
const { createRustFiles } = require('./rustProjectCreator');
const { createRubyFiles } = require('./rubyProjectCreator');

/**
 * Create language-specific project files
 * @param {string} language - The programming language
 * @param {string} projectName - Name of the project
 * @param {vscode.Uri} projectUri - URI of the project directory
 * @param {object} vscode - VS Code API object
 */
async function createLanguageSpecificFiles(language, projectName, projectUri, vscode) {
    switch (language.toLowerCase()) {
        case 'c':
            return await createCFiles(vscode, projectName, projectUri);

        case 'cpp':
        case 'c++':
            return await createCppFiles(vscode, projectName, projectUri);

        case 'python':
            return await createPythonFiles(vscode, projectName, projectUri);

        case 'java':
            return await createJavaFiles(vscode, projectName, projectUri);

        case 'rust':
            return await createRustFiles(vscode, projectName, projectUri);

        case 'ruby':
            return await createRubyFiles(vscode, projectName, projectUri);

        default:
            throw new Error(`Unsupported language: ${language}`);
    }
}

module.exports = {
    createLanguageSpecificFiles,
    createCFiles,
    createCppFiles,
    createPythonFiles,
    createJavaFiles,
    createRustFiles,
    createRubyFiles
};