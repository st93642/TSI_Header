/**
 * Git Language Code Base Generator
 * Generates Git configuration code (e.g., .gitignore, .gitattributes)
 */

/**
 * Generates Git code base
 * @returns {string} Git code base template
 */
function generateGitCodeBase() {
    return `# Git Configuration Files
# This file contains example Git configuration patterns

# .gitignore example
# Compiled binaries and build outputs
*.exe
*.dll
*.so
*.dylib
*.o
*.obj

# Build directories
build/
dist/
out/
target/

# IDE and editor files
.vscode/
.idea/
*.swp
*.swo
*~

# OS generated files
.DS_Store
Thumbs.db

# Logs and temporary files
*.log
*.tmp
*.temp

# .gitattributes example
# Set the default behavior, in case people don't have core.autocrlf set
* text=auto

# Explicitly declare text files you want to always be normalized and converted
# to native line endings on checkout
*.c text
*.h text

# Declare files that will always have CRLF line endings on checkout
*.sln text eol=crlf

# Denote all files that are truly binary and should not be modified
*.png binary
*.jpg binary
*.gif binary
*.ico binary
*.mov binary
*.mp4 binary
*.mp3 binary
*.flv binary
*.fla binary
*.swf binary
*.gz binary
*.zip binary
*.7z binary
*.ttf binary
*.eot binary
*.woff binary
*.pyc binary
*.pdf binary
`;
}

module.exports = {
    generateGitCodeBase
};