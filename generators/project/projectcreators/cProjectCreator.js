/**
 * C Project Creator
 * Creates C-specific project files and structure
 */

const { generateTSIHeaderContent } = require('../headerUtils');

/**
 * Create C-specific files
 */
async function createCFiles(vscode, projectName, projectUri) {
    // C projects typically don't need additional class files like C++
    // The main.c file is sufficient for basic C projects
    // Additional C files can be added manually as needed
}

module.exports = {
    createCFiles
};