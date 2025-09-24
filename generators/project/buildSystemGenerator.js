/**
 * Build System Generator
 * Creates Makefiles and build configurations for different languages
 */

let vscode;
try {
    vscode = require('vscode');
} catch (e) {
    // vscode not available (running outside VS Code)
    vscode = null;
}

/**
 * Create build system files for the project
 */
async function createBuildFiles(language, projectName, projectUri) {
    if (!vscode) {
        throw new Error('VS Code API not available');
    }

    if (language === 'c' || language === 'cpp') {
        await createMakefile(language, projectName, projectUri);
    }

    // Future: Add CMake, VS Code tasks.json, etc.
}

/**
 * Create Makefile for C/C++ projects
 */
async function createMakefile(language, projectName, projectUri) {
    const makefileContent = await generateMakefileContent(language, projectName);
    const makefileUri = vscode.Uri.joinPath(projectUri, 'Makefile');
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(makefileUri, encoder.encode(makefileContent));
}

/**
 * Generate Makefile content based on language and project name
 */
async function generateMakefileContent(language, projectName) {
    try {
        // Generate proper TSI header using Ruby CLI
        const { execSync } = require('child_process');
        const path = require('path');
        const os = require('os');
        const fs = require('fs');
        
        // Create a temporary Makefile to generate header for
        const tempDir = os.tmpdir();
        const tempFile = path.join(tempDir, 'Makefile');
        
        // Write a dummy file content
        fs.writeFileSync(tempFile, '# Temporary Makefile for header generation\n');
        
        // Get extension path
        let extensionPath;
        try {
            extensionPath = vscode.extensions.getExtension('st93642.tsi-header').extensionPath;
        } catch (e) {
            extensionPath = process.cwd();
        }
        
        const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
        
        // Get user configuration
        const config = vscode.workspace.getConfiguration('tsiheader');
        const username = config.get('username');
        const email = config.get('email');
        
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
        
        // Execute Ruby CLI for Makefile (shell script format)
        const command = `ruby "${cliPath}" insert "shellscript" "${tempFile}"`;
        const result = execSync(command, { encoding: 'utf8', env: env });
        const response = JSON.parse(result);
        
        // Clean up temp file
        try {
            fs.unlinkSync(tempFile);
        } catch (e) {
            // Ignore cleanup errors
        }
        
        if (response.success) {
            const tsiHeader = response.header;
            if (language === 'c') {
                return generateCMakefile(projectName, tsiHeader);
            } else if (language === 'cpp') {
                return generateCppMakefile(projectName, tsiHeader);
            }
        }
        
    } catch (error) {
        console.error('Failed to generate TSI header for Makefile:', error);
    }
    
    // Fallback to simple header if Ruby CLI fails
    const now = new Date();
    const dateStr = now.toLocaleDateString('en-US', { 
        month: 'short', 
        day: '2-digit', 
        year: 'numeric' 
    }).replace(',', '');
    
    const config = vscode.workspace.getConfiguration('tsiheader');
    const username = config.get('username') || 'TSI Student';
    const email = config.get('email') || 'student@tsi.lv';
    
    const fallbackHeader = `# TSI Header Makefile - ${projectName}
# Transport and Telecommunication Institute
# By: ${email}
# Created: ${dateStr}`;
    
    if (language === 'c') {
        return generateCMakefile(projectName, fallbackHeader);
    } else if (language === 'cpp') {
        return generateCppMakefile(projectName, fallbackHeader);
    }
    
    return '';
}

/**
 * Generate C Makefile
 */
function generateCMakefile(projectName, tsiHeader) {
    return `${tsiHeader}

# TSI Header C Project Makefile
# Project: ${projectName}

# Compiler and flags
CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -Iinclude -g
LDFLAGS = 

# Directories
SRCDIR = src
INCDIR = include
BUILDDIR = build
DOCSDIR = docs

# Source and object files
SOURCES = \$(wildcard \$(SRCDIR)/*.c)
OBJECTS = \$(SOURCES:\$(SRCDIR)/%.c=\$(BUILDDIR)/%.o)
TARGET = \$(BUILDDIR)/${projectName}

# Default target
.PHONY: all
all: \$(TARGET)

# Link object files to create executable
\$(TARGET): \$(OBJECTS) | \$(BUILDDIR)
	\$(CC) \$(OBJECTS) -o \$(TARGET) \$(LDFLAGS)
	@echo "✅ Built \$(TARGET) successfully!"

# Compile source files to object files
\$(BUILDDIR)/%.o: \$(SRCDIR)/%.c | \$(BUILDDIR)
	\$(CC) \$(CFLAGS) -c $< -o $@

# Create build directory
\$(BUILDDIR):
	mkdir -p \$(BUILDDIR)

# Clean build artifacts
.PHONY: clean
clean:
	rm -rf \$(BUILDDIR)
	@echo "🧹 Cleaned build artifacts"

# Run the program
.PHONY: run
run: \$(TARGET)
	@echo "🚀 Running \$(TARGET)..."
	@./\$(TARGET)

# Debug build
.PHONY: debug
debug: CFLAGS += -DDEBUG -O0
debug: \$(TARGET)

# Release build
.PHONY: release
release: CFLAGS += -DNDEBUG -O2
release: \$(TARGET)

# Install program
.PHONY: install
install: \$(TARGET)
	cp \$(TARGET) /usr/local/bin/
	@echo "📦 Installed \$(TARGET) to /usr/local/bin/"

# Show help
.PHONY: help
help:
	@echo "TSI Header C Project - Available targets:"
	@echo "  all       - Build the project (default)"
	@echo "  clean     - Remove build artifacts"
	@echo "  run       - Build and run the project"
	@echo "  debug     - Build with debug flags"
	@echo "  release   - Build with release optimizations"
	@echo "  install   - Install to system"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Project: ${projectName}"
	@echo "Compiler: \$(CC) \$(CFLAGS)"
`;
}

/**
 * Generate C++ Makefile
 */
function generateCppMakefile(projectName, tsiHeader) {
    return `${tsiHeader}

# TSI Header C++ Project Makefile
# Project: ${projectName}

# Compiler and flags
CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++17 -Iinclude -g
LDFLAGS = 

# Directories
SRCDIR = src
INCDIR = include
BUILDDIR = build
DOCSDIR = docs

# Source and object files
SOURCES = \$(wildcard \$(SRCDIR)/*.cpp)
OBJECTS = \$(SOURCES:\$(SRCDIR)/%.cpp=\$(BUILDDIR)/%.o)
TARGET = \$(BUILDDIR)/${projectName}

# Default target
.PHONY: all
all: \$(TARGET)

# Link object files to create executable
\$(TARGET): \$(OBJECTS) | \$(BUILDDIR)
	\$(CXX) \$(OBJECTS) -o \$(TARGET) \$(LDFLAGS)
	@echo "✅ Built \$(TARGET) successfully!"

# Compile source files to object files
\$(BUILDDIR)/%.o: \$(SRCDIR)/%.cpp | \$(BUILDDIR)
	\$(CXX) \$(CXXFLAGS) -c $< -o $@

# Create build directory
\$(BUILDDIR):
	mkdir -p \$(BUILDDIR)

# Clean build artifacts
.PHONY: clean
clean:
	rm -rf \$(BUILDDIR)
	@echo "🧹 Cleaned build artifacts"

# Run the program
.PHONY: run
run: \$(TARGET)
	@echo "🚀 Running \$(TARGET)..."
	@./\$(TARGET)

# Debug build
.PHONY: debug
debug: CXXFLAGS += -DDEBUG -O0
debug: \$(TARGET)

# Release build
.PHONY: release
release: CXXFLAGS += -DNDEBUG -O2
release: \$(TARGET)

# Install program
.PHONY: install
install: \$(TARGET)
	cp \$(TARGET) /usr/local/bin/
	@echo "📦 Installed \$(TARGET) to /usr/local/bin/"

# Show help
.PHONY: help
help:
	@echo "TSI Header C++ Project - Available targets:"
	@echo "  all       - Build the project (default)"
	@echo "  clean     - Remove build artifacts"
	@echo "  run       - Build and run the project"
	@echo "  debug     - Build with debug flags"
	@echo "  release   - Build with release optimizations"
	@echo "  install   - Install to system"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Project: ${projectName}"
	@echo "Compiler: \$(CXX) \$(CXXFLAGS)"
`;
}

module.exports = {
    createBuildFiles
};