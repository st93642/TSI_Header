/**
 * TSI Header Core Module Interface
 *
 * This module provides a clean interface to the core TSI Header functionalities
 * for use by the main extension and future feature modules.
 */

const path = require('path');

// Core functionality modules
const { generateClass } = require('./generators/classGenerators');
const { generateCodeBase } = require('./generators/codeBaseGenerators');
const { hasSubstantialContent, findHeaderEndLine } = require('./utils/contentAnalyzer');
const { createTSIProject } = require('./generators/project/projectCreator');
const { createLanguageSpecificFiles } = require('./generators/project/projectcreators/index');

// Header operations interface
class TSIHeaderManager {
    constructor(extensionPath) {
        this.extensionPath = extensionPath;
        this.cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
    }

    /**
     * Generate a TSI header for a file
     * @param {string} languageId - VS Code language ID
     * @param {string} fileName - File name
     * @param {Object} env - Environment variables for configuration
     * @returns {Promise<Object>} - {success: boolean, header?: string, message?: string}
     */
    async generateHeader(languageId, fileName, env = {}) {
        try {
            const { execSync } = require('child_process');
            const command = `ruby "${this.cliPath}" insert "${languageId}" "${fileName}"`;
            const result = execSync(command, {
                encoding: 'utf8',
                cwd: this.extensionPath,
                env: { ...process.env, ...env }
            });
            return JSON.parse(result);
        } catch (error) {
            return { success: false, message: error.message };
        }
    }

    /**
     * Update an existing TSI header
     * @param {string} languageId - VS Code language ID
     * @param {string} fileName - File name
     * @param {Object} env - Environment variables for configuration
     * @returns {Promise<Object>} - {success: boolean, message?: string}
     */
    async updateHeader(languageId, fileName, env = {}) {
        try {
            const { execSync } = require('child_process');
            const command = `ruby "${this.cliPath}" update "${languageId}" "${fileName}"`;
            const result = execSync(command, {
                encoding: 'utf8',
                cwd: this.extensionPath,
                env: { ...process.env, ...env }
            });
            return JSON.parse(result);
        } catch (error) {
            return { success: false, message: error.message };
        }
    }

    /**
     * Generate TSI header content for a file (creates temporary file and uses CLI)
     * @param {string} fileName - File name
     * @param {Object} vscode - VS Code API object
     * @returns {Promise<string>} - Generated header content
     */
    async generateTSIHeaderContent(fileName, vscode) {
        try {
            const fs = require('fs');
            const os = require('os');
            const path = require('path');
            const { execSync } = require('child_process');

            // Create a temporary file to generate header for
            const tempDir = os.tmpdir();
            const tempFile = path.join(tempDir, fileName);

            // Write a dummy file content
            fs.writeFileSync(tempFile, '// Temporary file for header generation\n');

            // Get user configuration
            const config = vscode.workspace.getConfiguration('tsiheader');
            const username = config.get('username');
            const email = config.get('email');
            const enableCustomHeader = config.get('customHeader.enableCustomHeader', false);
            const institutionName = config.get('customHeader.institutionName', 'Transport and Telecommunication Institute - Riga, Latvia');
            const institutionUrl = config.get('customHeader.institutionUrl', 'https://tsi.lv');

            // Set environment variables
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

            // Detect language from file extension
            const ext = path.extname(fileName).toLowerCase();
            let language = 'plaintext';
            if (ext === '.c') language = 'c';
            else if (ext === '.cpp' || ext === '.cxx' || ext === '.cc') language = 'cpp';
            else if (ext === '.h') language = 'c';
            else if (ext === '.hpp' || ext === '.hxx') language = 'cpp';
            else if (ext === '.java') language = 'java';
            else if (ext === '.rs') language = 'rust';
            else if (ext === '.rb') language = 'ruby';
            else if (ext === '.py') language = 'python';
            else if (ext === '.php') language = 'php';
            else if (ext === '.html') language = 'html';
            else if (ext === '.css') language = 'css';
            else if (ext === '.js') language = 'javascript';

            // Execute Ruby CLI
            const command = `ruby "${this.cliPath}" insert "${language}" "${tempFile}"`;
            const result = execSync(command, { encoding: 'utf8', env: env });
            const response = JSON.parse(result);

            // Clean up temp file
            try {
                fs.unlinkSync(tempFile);
            } catch (e) {
                // Ignore cleanup errors
            }

            if (response.success) {
                return response.header;
            } else {
                throw new Error(`Ruby CLI failed: ${response.message}`);
            }

        } catch (error) {
            console.error('Failed to generate TSI header via Ruby CLI:', error);
            // Fallback to simplified header
            return this.generateFallbackHeader(fileName, vscode);
        }
    }

    /**
     * Fallback header generation if Ruby CLI is not available
     * @param {string} fileName - Name of the file
     * @param {Object} vscode - VS Code API object
     * @returns {string} The fallback header content
     */
    generateFallbackHeader(fileName, vscode) {
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

        // Get user info from settings
        const config = vscode.workspace.getConfiguration('tsiheader');
        const username = config.get('username') || 'TSI Student';
        const email = config.get('email') || 'student@tsi.lv';

        const dateTime = `${dateStr} ${timeStr}`;

        return `/*****************************************************************************/
/*                                                                           */
/*  ${fileName.padEnd(50, ' ')}         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: ${email.padEnd(50, ' ')} TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: ${dateTime} ${username.padEnd(30, ' ')} TT    SSSSSSS II */
/*  Updated: ${dateTime} ${username.padEnd(30, ' ')}                          */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/`;
    }
}

// Code generation interface
class TSICodeGenerator {
    constructor(extensionPath) {
        this.extensionPath = extensionPath;
    }

    /**
     * Generate a class template
     * @param {string} languageId - VS Code language ID
     * @param {string} className - Name of the class
     * @param {string} fileName - File name
     * @param {Object} env - Environment variables
     * @returns {Object} - {success: boolean, content?: string, files?: Array, message?: string}
     */
    generateClass(languageId, className, fileName, env = {}) {
        const cliPath = path.join(this.extensionPath, 'core', 'lib', 'tsi_header_cli.rb');
        return generateClass(languageId, className, fileName, this.extensionPath, cliPath, env);
    }

    /**
     * Generate code base structure
     * @param {string} languageId - VS Code language ID
     * @param {string} fileName - File name
     * @returns {Object} - {success: boolean, content?: string, message?: string}
     */
    generateCodeBase(languageId, fileName) {
        return generateCodeBase(languageId, fileName);
    }
}

// Project creation interface
class TSIProjectManager {
    constructor(extensionPath) {
        this.extensionPath = extensionPath;
    }

    /**
     * Create a TSI project structure
     * @param {string} language - Programming language
     * @param {string} projectName - Project name
     * @param {Object} workspaceUri - VS Code workspace URI
     * @param {Object} vscode - VS Code API object
     * @returns {Promise<Object>} - {success: boolean, message?: string}
     */
    async createProject(language, projectName, workspaceUri, vscode) {
        try {
            const projectUri = vscode.Uri.joinPath(workspaceUri, projectName);

            // Create base directories
            const directories = this.getDirectoryStructure(language);
            for (const dir of directories) {
                const dirUri = vscode.Uri.joinPath(projectUri, dir);
                await vscode.workspace.fs.createDirectory(dirUri);
            }

            // Generate main source file
            await this.createMainSourceFile(language, projectName, projectUri, vscode);

            // Create header file (for C/C++)
            if (language === 'c' || language === 'cpp') {
                await this.createHeaderFile(language, projectName, projectUri, vscode);
            }

            // Create language-specific project files
            await createLanguageSpecificFiles(language, projectName, projectUri, vscode);

            return { success: true };
        } catch (error) {
            return { success: false, message: error.message };
        }
    }

    getDirectoryStructure(language) {
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

    async createMainSourceFile(language, projectName, projectUri, vscode) {
        const extension = this.getFileExtension(language);
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

        // Generate TSI header using Ruby CLI API
        const { generateTSIHeaderContent } = require('./generators/project/headerUtils');
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

    async createHeaderFile(language, projectName, projectUri, vscode) {
        const extension = language === 'c' ? 'h' : 'hpp';
        const fileName = `${projectName}.${extension}`;
        const fileUri = vscode.Uri.joinPath(projectUri, 'include', fileName);

        // Generate TSI header using Ruby CLI API
        const { generateTSIHeaderContent } = require('./generators/project/headerUtils');
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

    getFileExtension(language) {
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
}

// Utility functions interface
class TSIUtils {
    /**
     * Check if file content has substantial content beyond header
     * @param {string} content - File content
     * @returns {boolean}
     */
    hasSubstantialContent(content) {
        return hasSubstantialContent(content);
    }

    /**
     * Find the line where header ends
     * @param {string} content - File content
     * @returns {number} - Line number where header ends
     */
    findHeaderEndLine(content) {
        return findHeaderEndLine(content);
    }

    /**
     * Detect language from file extension (for unsupported VS Code languages)
     * @param {string} languageId - VS Code language ID
     * @param {string} fileName - File name
     * @returns {string} - Detected language ID
     */
    detectLanguageFromExtension(languageId, fileName) {
        let detectedLanguageId = languageId;
        const fileExtension = fileName.split('.').pop().toLowerCase();
        const fileNameParts = fileName.split('.');

        // Handle compound extensions (e.g., .html.erb, .js.erb)
        if (fileNameParts.length > 2) {
            const compoundExt = fileNameParts.slice(-2).join('.');
            if (compoundExt === 'html.erb' || compoundExt === 'js.erb' || compoundExt === 'css.erb') {
                detectedLanguageId = 'erb';
            }
        }

        // Handle single extensions - override VS Code's default detection for certain files
        if (fileExtension === 'erb' || fileExtension === 'rhtml') {
            detectedLanguageId = 'erb';
        } else if (fileExtension === 'vue') {
            detectedLanguageId = 'vue';
        } else if (fileExtension === 'ejs' || fileExtension === 'ect' || fileExtension === 'jst') {
            detectedLanguageId = 'ejs';
        } else if (fileExtension === 'toml') {
            detectedLanguageId = 'toml';
        } else if (fileExtension === 'h') {
            // .h files should be treated as C, not C++ (unless in C++ context)
            detectedLanguageId = 'c';
        }

        return detectedLanguageId;
    }

    /**
     * Get user configuration with fallbacks
     * @param {Object} vscode - VS Code API
     * @returns {Object} - {username, email, hasCredentials}
     */
    getUserConfig(vscode) {
        const config = vscode.workspace.getConfiguration('tsiheader');
        const username = config.get('username');
        const email = config.get('email');

        const hasUsername = username && username.trim() !== '';
        const hasEmail = email && email.trim() !== '';

        // Check git config as fallback
        let gitUsername = '';
        let gitEmail = '';
        try {
            const { execSync } = require('child_process');
            gitUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
        } catch (e) { /* ignore */ }
        try {
            const { execSync } = require('child_process');
            gitEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
        } catch (e) { /* ignore */ }

        const finalUsername = hasUsername ? username : gitUsername;
        const finalEmail = hasEmail ? email : gitEmail;

        return {
            username: finalUsername,
            email: finalEmail,
            hasCredentials: !!(finalUsername && finalEmail)
        };
    }
}

// Main interface class
class TSICore {
    constructor(extensionPath) {
        this.extensionPath = extensionPath;
        this.headerManager = new TSIHeaderManager(extensionPath);
        this.codeGenerator = new TSICodeGenerator(extensionPath);
        this.projectManager = new TSIProjectManager(extensionPath);
        this.utils = new TSIUtils();
    }

    // Convenience methods that delegate to sub-managers
    async generateHeader(languageId, fileName, env) {
        return this.headerManager.generateHeader(languageId, fileName, env);
    }

    async updateHeader(languageId, fileName, env) {
        return this.headerManager.updateHeader(languageId, fileName, env);
    }

    async removeHeader(languageId, fileName) {
        return this.headerManager.removeHeader(languageId, fileName);
    }

    async generateTSIHeaderContent(fileName, vscode) {
        return this.headerManager.generateTSIHeaderContent(fileName, vscode);
    }

    generateClass(languageId, className, fileName, env) {
        return this.codeGenerator.generateClass(languageId, className, fileName, env);
    }

    generateCodeBase(languageId, fileName) {
        return this.codeGenerator.generateCodeBase(languageId, fileName);
    }

    async createProject(language, projectName, workspaceUri, vscode) {
        return this.projectManager.createProject(language, projectName, workspaceUri, vscode);
    }

    // Utility methods
    hasSubstantialContent(content) {
        return this.utils.hasSubstantialContent(content);
    }

    findHeaderEndLine(content) {
        return this.utils.findHeaderEndLine(content);
    }

    detectLanguageFromExtension(languageId, fileName) {
        return this.utils.detectLanguageFromExtension(languageId, fileName);
    }

    getUserConfig(vscode) {
        return this.utils.getUserConfig(vscode);
    }
}

// Export the main interface and individual managers for flexibility
module.exports = {
    TSICore,
    TSIHeaderManager,
    TSICodeGenerator,
    TSIProjectManager,
    TSIUtils
};