/**
 * Build System Generator
 * Creates Makefiles and build configurations for different languages
 */

// Import build system generators
const { generateCMakefile } = require('./buildsystems/c');
const { generateCppMakefile } = require('./buildsystems/cppBuildSystem');
const { generatePythonMakefileContent, generateFallbackPythonMakefile } = require('./buildsystems/pythonBuildSystem');
const { generateMavenPomContent, generateGradleBuildContent } = require('./buildsystems/javaBuildSystem');

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
 * Create Makefile for Python projects
 */
async function createPythonMakefile(projectName, projectUri) {
    const makefileContent = await generatePythonMakefileContentWithHeader(projectName);
    const makefileUri = vscode.Uri.joinPath(projectUri, 'Makefile');
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(makefileUri, encoder.encode(makefileContent));
}

/**
 * Generate Python Makefile content
 */
async function generatePythonMakefileContentWithHeader(projectName) {
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
            return generatePythonMakefileContent(projectName, response.header);
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
    const pomContent = await generateMavenPomContentWithHeader(projectName);
    const pomUri = vscode.Uri.joinPath(projectUri, 'pom.xml');
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(pomUri, encoder.encode(pomContent));
}

/**
 * Generate Maven pom.xml content
 */
async function generateMavenPomContentWithHeader(projectName) {
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
            return generateMavenPomContent(projectName, response.header);
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
    
    return generateMavenPomContent(projectName, fallbackHeader);
}

/**
 * Create Gradle build.gradle for Java projects
 */
async function createGradleBuild(projectName, projectUri) {
    const gradleContent = await generateGradleBuildContentWithHeader(projectName);
    const gradleUri = vscode.Uri.joinPath(projectUri, 'build.gradle');
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(gradleUri, encoder.encode(gradleContent));
}

/**
 * Generate Gradle build.gradle content
 */
async function generateGradleBuildContentWithHeader(projectName) {
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
            return generateGradleBuildContent(projectName, tsiHeader);
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

module.exports = {
    createBuildFiles
};