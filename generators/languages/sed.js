/**
 * SED Code Base Generator
 * Generates boilerplate SED code
 */

function generateSedCodeBase() {
    return `\n#!/usr/bin/sed -f\n# Basic SED script\n# Note: SED is a stream editor for filtering and transforming text\n\n# Print a greeting message (insert at beginning)\n1i\\\nHello, World!\\\nThis is a basic SED script.\n\n# Example transformations (commented out)\n# s/old/new/g        # Replace 'old' with 'new' globally\n# /pattern/d         # Delete lines containing 'pattern'\n# p                  # Print current line\n# q                  # Quit after first line\n\n# Example: Convert text to uppercase\n# y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/\n\n# Example: Add line numbers\n# =\n# N\n# s/\\n/: /\n`;
}

module.exports = {
    generateSedCodeBase
};