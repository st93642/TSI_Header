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
    } else if (language === 'python') {
        await createPythonMakefile(projectName, projectUri);
    } else if (language === 'java') {
        await createJavaBuildFiles(projectName, projectUri);
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

/**
 * Create Makefile for Python projects
 */
async function createPythonMakefile(projectName, projectUri) {
    const makefileContent = await generatePythonMakefileContent(projectName);
    const makefileUri = vscode.Uri.joinPath(projectUri, 'Makefile');
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(makefileUri, encoder.encode(makefileContent));
}

/**
 * Generate Python Makefile content
 */
async function generatePythonMakefileContent(projectName) {
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
        
        // Execute Ruby CLI
        const command = `ruby "${cliPath}" insert "makefile" "${tempFile}"`;
        const result = execSync(command, { encoding: 'utf8', env: env });
        const response = JSON.parse(result);
        
        // Clean up temp file
        try {
            fs.unlinkSync(tempFile);
        } catch (e) {
            // Ignore cleanup errors
        }
        
        if (response.success) {
            return `${response.header}
# Makefile for ${projectName} Python project

# Python settings
PYTHON := python3
PIP := pip3
VENV := venv
VENV_ACTIVATE := \$(VENV)/bin/activate

# Project settings
PACKAGE_NAME := ${projectName}
MAIN_FILE := src/main.py

.PHONY: help install run test clean venv format lint docs build

# Default target
help: ## Show this help message
	@echo "${projectName} - Python Project Makefile"
	@echo ""
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' \$(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\\n", \$\$1, \$\$2}'

venv: ## Create virtual environment
	@echo "Creating virtual environment..."
	@\$(PYTHON) -m venv \$(VENV)
	@echo "Virtual environment created. Run 'source \$(VENV_ACTIVATE)' to activate."

install: venv ## Install dependencies
	@echo "Installing dependencies..."
	@source \$(VENV_ACTIVATE) && \$(PIP) install -r requirements.txt
	@echo "Dependencies installed."

install-dev: install ## Install development dependencies
	@echo "Installing development dependencies..."
	@source \$(VENV_ACTIVATE) && \$(PIP) install -e .[dev]
	@echo "Development dependencies installed."

run: ## Run the main application
	@echo "Running ${projectName}..."
	@source \$(VENV_ACTIVATE) && \$(PYTHON) \$(MAIN_FILE)

test: ## Run tests
	@echo "Running tests..."
	@source \$(VENV_ACTIVATE) && python -m pytest tests/ -v

test-cov: ## Run tests with coverage
	@echo "Running tests with coverage..."
	@source \$(VENV_ACTIVATE) && python -m pytest tests/ --cov=src --cov-report=html

format: ## Format code with black
	@echo "Formatting code..."
	@source \$(VENV_ACTIVATE) && black src/ tests/

lint: ## Lint code with flake8
	@echo "Linting code..."
	@source \$(VENV_ACTIVATE) && flake8 src/ tests/

type-check: ## Type check with mypy
	@echo "Type checking..."
	@source \$(VENV_ACTIVATE) && mypy src/

docs: ## Generate documentation
	@echo "Generating documentation..."
	@source \$(VENV_ACTIVATE) && sphinx-build -b html docs/ docs/_build/html

build: ## Build the package
	@echo "Building package..."
	@source \$(VENV_ACTIVATE) && python setup.py sdist bdist_wheel

clean: ## Clean build artifacts
	@echo "Cleaning..."
	@rm -rf build/
	@rm -rf dist/
	@rm -rf *.egg-info/
	@rm -rf .coverage
	@rm -rf htmlcov/
	@find . -type d -name __pycache__ -exec rm -rf {} +
	@find . -type f -name "*.pyc" -delete
	@find . -type f -name "*.pyo" -delete

clean-venv: ## Remove virtual environment
	@echo "Removing virtual environment..."
	@rm -rf \$(VENV)

# Development workflow
dev: install-dev format lint test ## Run full development workflow

# CI/CD targets
ci: format lint test-cov ## Run CI checks

# Utility targets
freeze: ## Freeze current dependencies to requirements.txt
	@echo "Freezing dependencies..."
	@source \$(VENV_ACTIVATE) && \$(PIP) freeze > requirements.txt

update-deps: ## Update all dependencies
	@echo "Updating dependencies..."
	@source \$(VENV_ACTIVATE) && \$(PIP) install --upgrade -r requirements.txt

# Project info
info: ## Show project information
	@echo "Project: ${projectName}"
	@echo "Python: \$(\$(PYTHON) --version)"
	@echo "Virtual Environment: \$(VENV)"
	@echo "Main File: \$(MAIN_FILE)"
`;
        } else {
            throw new Error(`Ruby CLI failed: ${response.message}`);
        }
        
    } catch (error) {
        console.error('Failed to generate Python Makefile via Ruby CLI:', error);
        // Fallback to simplified Makefile if Ruby CLI fails
        return generateFallbackPythonMakefile(projectName);
    }
}

/**
 * Fallback Python Makefile generation
 */
function generateFallbackPythonMakefile(projectName) {
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
/*  Makefile                                              TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: ${email.padEnd(50, ' ')} TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: ${dateTime} ${username.padEnd(30, ' ')} TT    SSSSSSS II */
/*  Updated: ${dateTime} ${username.padEnd(30, ' ')}                          */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

# Makefile for ${projectName} Python project

# Python settings
PYTHON := python3
PIP := pip3
VENV := venv
VENV_ACTIVATE := \$(VENV)/bin/activate

# Project settings
PACKAGE_NAME := ${projectName}
MAIN_FILE := src/main.py

.PHONY: help install run test clean venv format lint docs build

# Default target
help: ## Show this help message
	@echo "${projectName} - Python Project Makefile"
	@echo ""
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' \$(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\\n", \$\$1, \$\$2}'

venv: ## Create virtual environment
	@echo "Creating virtual environment..."
	@\$(PYTHON) -m venv \$(VENV)
	@echo "Virtual environment created. Run 'source \$(VENV_ACTIVATE)' to activate."

install: venv ## Install dependencies
	@echo "Installing dependencies..."
	@source \$(VENV_ACTIVATE) && \$(PIP) install -r requirements.txt
	@echo "Dependencies installed."

run: ## Run the main application
	@echo "Running ${projectName}..."
	@source \$(VENV_ACTIVATE) && \$(PYTHON) \$(MAIN_FILE)

test: ## Run tests
	@echo "Running tests..."
	@source \$(VENV_ACTIVATE) && python -m pytest tests/ -v

clean: ## Clean build artifacts
	@echo "Cleaning..."
	@rm -rf build/
	@rm -rf dist/
	@rm -rf *.egg-info/
	@find . -type d -name __pycache__ -exec rm -rf {} +
	@find . -type f -name "*.pyc" -delete

# Project info
info: ## Show project information
	@echo "Project: ${projectName}"
	@echo "Python: \$(\$(PYTHON) --version)"
	@echo "Virtual Environment: \$(VENV)"
	@echo "Main File: \$(MAIN_FILE)"
`;
}

/**
 * Create Java build files (Maven pom.xml and Gradle build.gradle)
 */
async function createJavaBuildFiles(projectName, projectUri) {
    await createMavenPom(projectName, projectUri);
    await createGradleBuild(projectName, projectUri);
}

/**
 * Create Maven pom.xml for Java projects
 */
async function createMavenPom(projectName, projectUri) {
    const pomContent = await generateMavenPomContent(projectName);
    const pomUri = vscode.Uri.joinPath(projectUri, 'pom.xml');
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(pomUri, encoder.encode(pomContent));
}

/**
 * Generate Maven pom.xml content
 */
async function generateMavenPomContent(projectName) {
    try {
        // Generate proper TSI header using Ruby CLI
        const { execSync } = require('child_process');
        const path = require('path');
        const os = require('os');
        const fs = require('fs');
        
        // Create a temporary XML file to generate header for
        const tempDir = os.tmpdir();
        const tempFile = path.join(tempDir, 'pom.xml');
        
        // Write a dummy XML content
        fs.writeFileSync(tempFile, '<?xml version="1.0" encoding="UTF-8"?>\n<!-- Temporary pom.xml for header generation -->\n');
        
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
        
        // Execute Ruby CLI for XML
        const command = `ruby "${cliPath}" insert "xml" "${tempFile}"`;
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
            return generateMavenPomXml(projectName, tsiHeader);
        }
        
    } catch (error) {
        console.error('Failed to generate TSI header for Maven pom.xml:', error);
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
    
    const fallbackHeader = `<!-- TSI Header Maven pom.xml - ${projectName}
     Transport and Telecommunication Institute
     By: ${email}
     Created: ${dateStr} -->`;
    
    return generateMavenPomXml(projectName, fallbackHeader);
}

/**
 * Generate Maven pom.xml structure
 */
function generateMavenPomXml(projectName, tsiHeader) {
    return `<?xml version="1.0" encoding="UTF-8"?>
${tsiHeader}
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>lv.tsi</groupId>
    <artifactId>${projectName}</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>

    <name>${projectName}</name>
    <description>TSI Header Java Project - ${projectName}</description>

    <properties>
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <junit.version>5.9.2</junit.version>
    </properties>

    <dependencies>
        <!-- JUnit 5 for testing -->
        <dependency>
            <groupId>org.junit.jupiter</groupId>
            <artifactId>junit-jupiter</artifactId>
            <version>\${junit.version}</version>
            <scope>test</scope>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <!-- Maven Compiler Plugin -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.11.0</version>
                <configuration>
                    <source>17</source>
                    <target>17</target>
                </configuration>
            </plugin>

            <!-- Maven Surefire Plugin for running tests -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-surefire-plugin</artifactId>
                <version>3.0.0</version>
            </plugin>

            <!-- Maven JAR Plugin -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-jar-plugin</artifactId>
                <version>3.3.0</version>
                <configuration>
                    <archive>
                        <manifest>
                            <mainClass>lv.tsi.${projectName}.Main</mainClass>
                        </manifest>
                    </archive>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>`;
}

/**
 * Create Gradle build.gradle for Java projects
 */
async function createGradleBuild(projectName, projectUri) {
    const gradleContent = await generateGradleBuildContent(projectName);
    const gradleUri = vscode.Uri.joinPath(projectUri, 'build.gradle');
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(gradleUri, encoder.encode(gradleContent));
}

/**
 * Generate Gradle build.gradle content
 */
async function generateGradleBuildContent(projectName) {
    try {
        // Generate proper TSI header using Ruby CLI
        const { execSync } = require('child_process');
        const path = require('path');
        const os = require('os');
        const fs = require('fs');
        
        // Create a temporary Gradle file to generate header for
        const tempDir = os.tmpdir();
        const tempFile = path.join(tempDir, 'build.gradle');
        
        // Write a dummy Gradle content
        fs.writeFileSync(tempFile, '// Temporary build.gradle for header generation\n');
        
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
        
        // Execute Ruby CLI for Gradle (use shell script format)
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
            return generateGradleBuildScript(projectName, tsiHeader);
        }
        
    } catch (error) {
        console.error('Failed to generate TSI header for Gradle build.gradle:', error);
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
    
    const fallbackHeader = `// TSI Header Gradle build.gradle - ${projectName}
// Transport and Telecommunication Institute
// By: ${email}
// Created: ${dateStr}`;
    
    return generateGradleBuildScript(projectName, fallbackHeader);
}

/**
 * Generate Gradle build.gradle structure
 */
function generateGradleBuildScript(projectName, tsiHeader) {
    return `${tsiHeader}

plugins {
    id 'java'
    id 'application'
}

group = 'lv.tsi'
version = '1.0.0'
sourceCompatibility = '17'

repositories {
    mavenCentral()
}

dependencies {
    // JUnit 5 for testing
    testImplementation 'org.junit.jupiter:junit-jupiter:5.9.2'
    
    // Additional dependencies can be added here
}

application {
    mainClassName = 'lv.tsi.${projectName}.Main'
}

test {
    useJUnitPlatform()
}

jar {
    manifest {
        attributes 'Main-Class': 'lv.tsi.${projectName}.Main'
    }
}

task fatJar(type: Jar) {
    manifest.from jar.manifest
    classifier = 'all'
    from {
        configurations.runtimeClasspath.collect { it.isDirectory() ? it : zipTree(it) }
    } {
        exclude "META-INF/*.SF"
        exclude "META-INF/*.DSA"
        exclude "META-INF/*.RSA"
    }
    with jar
}

task runJar(dependsOn: jar) {
    doLast {
        javaexec {
            main = '-jar'
            args jar.archiveFile.get()
        }
    }
}`;
}

module.exports = {
    createBuildFiles
};