/**
 * Content Analyzer Module
 * Contains utilities for analyzing file content
 */

/**
 * Analyzes if a file has substantial content beyond just the header
 * @param {string} content - The file content
 * @returns {boolean} True if file has substantial content
 */
function hasSubstantialContent(content) {
    const lines = content.split('\n');

    // Skip header (usually first 12-15 lines) and check for real content
    for (let i = 12; i < lines.length; i++) {
        const line = lines[i].trim();
        // Ignore empty lines, comments, and basic boilerplate
        if (line && !line.startsWith('//') && !line.startsWith('/*') &&
            !line.startsWith('*') && !line.startsWith('import') &&
            !line.startsWith('using') && !line.startsWith('#include') &&
            !line.startsWith('public class') && !line.startsWith('def ') &&
            !line.startsWith('class ') && !line.includes('main(') &&
            !line.startsWith('package ') && !line.startsWith('<?php')) {
            return true;
        }
    }

    return false;
}

/**
 * Finds the end of the header in a file
 * @param {string} content - The file content
 * @returns {number} The line number where header ends
 */
function findHeaderEndLine(content) {
    const lines = content.split('\n');

    // Look for the end of the TSI header (usually ends with bottom border)
    for (let i = 0; i < Math.min(20, lines.length); i++) {
        if (lines[i].includes('*****************************************************************************/') ||
            lines[i].includes('******************************************************************************/')) {
            return i + 1; // Return line after header ends
        }
    }

    // Fallback: assume header ends by line 15
    return Math.min(15, lines.length);
}

module.exports = {
    hasSubstantialContent,
    findHeaderEndLine
};