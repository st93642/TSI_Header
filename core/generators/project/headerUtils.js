/**
 * TSI Header Generation Utilities
 * Shared utilities for generating TSI headers across the extension
 */

const path = require('path');
const fs = require('fs');
const { execSync } = require('child_process');

/**
 * Generate TSI header content using Ruby CLI
 * @param {string} fileName - Name of the file to generate header for
 * @param {object} vscode - VS Code API object
 * @returns {Promise<string>} The generated header content
 */
async function generateTSIHeaderContent(fileName, vscode) {
    try {
        // Create a temporary file to generate header for
        const os = require('os');
        const tempDir = os.tmpdir();
        const tempFile = path.join(tempDir, fileName);

        // Ensure the directory exists
        const tempFileDir = path.dirname(tempFile);
        if (!fs.existsSync(tempFileDir)) {
            fs.mkdirSync(tempFileDir, { recursive: true });
        }

        // Write a dummy file content
        fs.writeFileSync(tempFile, '// Temporary file for header generation\n');

        // Get extension path - we need to find where the Ruby CLI is
        // In VS Code extension context, we need to get the extension path
        let extensionPath;
        try {
            // Try to get from VS Code context
            extensionPath = vscode.extensions.getExtension('st93642.uni-header').extensionPath;
        } catch (e) {
            // Fallback to current working directory
            extensionPath = process.cwd();
        }

        const cliPath = path.join(extensionPath, 'core', 'lib', 'tsi_header_cli.rb');

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
        else if (ext === '.ts') language = 'typescript';
        else if (ext === '.json') language = 'json';
        else if (ext === '.md') language = 'markdown';
        else if (ext === '.yml' || ext === '.yaml') language = 'yaml';
        else if (ext === '.xml') language = 'xml';
        else if (ext === '.sh') language = 'shellscript';
        else if (ext === '.sql') language = 'sql';

        // Execute Ruby CLI
        const command = `ruby "${cliPath}" insert "${language}" "${tempFile}"`;
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
        // Fallback to simplified header if Ruby CLI fails
        return generateFallbackHeader(fileName, vscode);
    }
}

/**
 * Fallback header generation if Ruby CLI is not available
 * @param {string} fileName - Name of the file
 * @param {object} vscode - VS Code API object
 * @returns {string} The fallback header content
 */
function generateFallbackHeader(fileName, vscode) {
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

module.exports = {
    generateTSIHeaderContent
};