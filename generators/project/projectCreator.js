/**
 * TSI Project Creator
 * Creates complete project structures with TSI headers and professional setup
 */

let vscode;
try {
    vscode = require('vscode');
} catch (e) {
    // vscode not available (running outside VS Code)
    vscode = null;
}
const path = require('path');
const { generateCodeBase } = require('../codeBaseGenerators');
const { createBuildFiles } = require('./buildSystemGenerator');
const { createDocumentationFiles } = require('./documentationGenerator');
const { createGitIgnoreFile } = require('./gitIgnoreGenerator');

/**
 * Main function to create TSI project
 * Called from the VS Code command
 */
async function createTSIProject(uri) {
    if (!vscode) {
        throw new Error('VS Code API not available');
    }

    try {
        // Step 1: Show language selection
        const language = await showLanguageQuickPick();
        if (!language) return;

        // Step 2: Get project name
        const projectName = await showProjectNameInput();
        if (!projectName) return;

        // Step 3: Determine workspace location
        const workspaceUri = uri || await selectWorkspaceLocation();
        if (!workspaceUri) return;

        // Step 4: Create project structure
        await createProjectStructure(language, projectName, workspaceUri);
        
        // Step 5: Show success message and open project
        vscode.window.showInformationMessage(
            `TSI ${language.toUpperCase()} project "${projectName}" created successfully!`,
            'Open Project'
        ).then(selection => {
            if (selection === 'Open Project') {
                const projectUri = vscode.Uri.joinPath(workspaceUri, projectName);
                vscode.commands.executeCommand('vscode.openFolder', projectUri);
            }
        });
        
    } catch (error) {
        console.error('TSI Project Creator Error:', error);
        vscode.window.showErrorMessage(`Failed to create TSI project: ${error.message}`);
    }
}

/**
 * Show language selection QuickPick
 */
async function showLanguageQuickPick() {
    const languages = [
        {
            label: '$(symbol-class) C Project',
            description: 'Traditional C programming with GCC',
            detail: 'Creates: main.c, Makefile, headers, documentation',
            value: 'c'
        },
        {
            label: '$(symbol-class) C++ Project', 
            description: 'Modern C++ with object-oriented design',
            detail: 'Creates: main.cpp, classes, Makefile, documentation',
            value: 'cpp'
        }
    ];

    const selected = await vscode.window.showQuickPick(languages, {
        placeHolder: 'Select programming language for your TSI project',
        matchOnDescription: true,
        matchOnDetail: true,
        canPickMany: false
    });

    return selected ? selected.value : undefined;
}

/**
 * Show project name input box
 */
async function showProjectNameInput() {
    return await vscode.window.showInputBox({
        prompt: 'Enter project name',
        placeHolder: 'my-tsi-project',
        validateInput: (value) => {
            if (!value || value.trim().length === 0) {
                return 'Project name cannot be empty';
            }
            if (!/^[a-zA-Z0-9_-]+$/.test(value)) {
                return 'Project name can only contain letters, numbers, hyphens, and underscores';
            }
            return null;
        }
    });
}

/**
 * Select workspace location
 */
async function selectWorkspaceLocation() {
    // If we have workspace folders, use the first one
    if (vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 0) {
        return vscode.workspace.workspaceFolders[0].uri;
    }

    // Otherwise, show folder picker
    const uris = await vscode.window.showOpenDialog({
        canSelectFiles: false,
        canSelectFolders: true,
        canSelectMany: false,
        openLabel: 'Select Project Location'
    });

    return uris && uris.length > 0 ? uris[0] : undefined;
}

/**
 * Create complete project structure
 */
async function createProjectStructure(language, projectName, workspaceUri) {
    const projectUri = vscode.Uri.joinPath(workspaceUri, projectName);
    
    // Create base directories
    const directories = getDirectoryStructure(language);
    for (const dir of directories) {
        const dirUri = vscode.Uri.joinPath(projectUri, dir);
        await vscode.workspace.fs.createDirectory(dirUri);
    }
    
    // Generate main source file
    await createMainSourceFile(language, projectName, projectUri);
    
    // Create header file (for C/C++)
    if (language === 'c' || language === 'cpp') {
        await createHeaderFile(language, projectName, projectUri);
    }
    
    // Create base class files for C++
    if (language === 'cpp') {
        await createBaseClassFiles(projectName, projectUri);
    }
    
    // Create build system files
    await createBuildFiles(language, projectName, projectUri);
    
    // Create documentation
    await createDocumentationFiles(language, projectName, projectUri);
    
    // Create .gitignore
    await createGitIgnoreFile(language, projectUri);
}

/**
 * Get directory structure for language
 */
function getDirectoryStructure(language) {
    const commonDirs = ['src', 'docs', 'build'];
    
    if (language === 'c' || language === 'cpp') {
        return [...commonDirs, 'include'];
    }
    
    return commonDirs;
}

/**
 * Create main source file with TSI header and code
 */
async function createMainSourceFile(language, projectName, projectUri) {
    const extension = getFileExtension(language);
    const fileName = `main.${extension}`;
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', fileName);
    
    // Generate TSI header (we'll call the Ruby CLI)
    const headerContent = await generateTSIHeaderContent(fileName);
    
    // Generate code base using existing API
    const codeResult = generateCodeBase(language, fileName);
    const codeContent = codeResult.success ? codeResult.content : '';
    
    // Combine header and code
    const fullContent = headerContent + '\n' + codeContent;
    
    // Write to file
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(fullContent));
}

/**
 * Create header file for C/C++
 */
async function createHeaderFile(language, projectName, projectUri) {
    const extension = language === 'c' ? 'h' : 'hpp';
    const fileName = `${projectName}.${extension}`;
    const fileUri = vscode.Uri.joinPath(projectUri, 'include', fileName);
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent(fileName);
    
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

/**
 * Create BaseClass files for C++ projects
 */
async function createBaseClassFiles(projectName, projectUri) {
    await createBaseClassHeader(projectName, projectUri);
    await createBaseClassSource(projectName, projectUri);
}

/**
 * Create BaseClass.hpp file
 */
async function createBaseClassHeader(projectName, projectUri) {
    const fileName = 'BaseClass.hpp';
    const fileUri = vscode.Uri.joinPath(projectUri, 'include', fileName);
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent(fileName);
    
    const content = `${headerContent}

#ifndef BASECLASS_HPP
#define BASECLASS_HPP

#include <string>
#include <iostream>

/**
 * BaseClass - Foundation class for ${projectName}
 * 
 * This class provides basic functionality and serves as a base
 * for other classes in the ${projectName} project.
 * 
 * Features:
 * - Basic object initialization
 * - String-based naming system
 * - Virtual destructor for inheritance
 * - Display functionality
 */
class BaseClass {
private:
    std::string name;
    int id;
    static int nextId;

protected:
    // Protected members accessible by derived classes
    void setId(int newId);

public:
    // Constructors
    BaseClass();
    explicit BaseClass(const std::string& name);
    BaseClass(const std::string& name, int id);
    
    // Copy constructor
    BaseClass(const BaseClass& other);
    
    // Assignment operator
    BaseClass& operator=(const BaseClass& other);
    
    // Virtual destructor for proper inheritance
    virtual ~BaseClass();
    
    // Getters
    const std::string& getName() const;
    int getId() const;
    static int getNextId();
    
    // Setters
    void setName(const std::string& newName);
    
    // Virtual methods (can be overridden)
    virtual void display() const;
    virtual std::string toString() const;
    
    // Utility methods
    void initialize();
    bool isValid() const;
    
    // Operator overloading
    bool operator==(const BaseClass& other) const;
    bool operator!=(const BaseClass& other) const;
    
    // Friend function for output stream
    friend std::ostream& operator<<(std::ostream& os, const BaseClass& obj);
};

#endif // BASECLASS_HPP
`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create BaseClass.cpp file
 */
async function createBaseClassSource(projectName, projectUri) {
    const fileName = 'BaseClass.cpp';
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', fileName);
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent(fileName);
    
    const content = `${headerContent}

#include "BaseClass.hpp"
#include <sstream>
#include <iomanip>

// Static member initialization
int BaseClass::nextId = 1;

/**
 * Default constructor
 * Initializes BaseClass with default values
 */
BaseClass::BaseClass() : name("DefaultObject"), id(nextId++) {
    initialize();
}

/**
 * Constructor with name
 * @param name The name for this object
 */
BaseClass::BaseClass(const std::string& name) : name(name), id(nextId++) {
    initialize();
}

/**
 * Constructor with name and id
 * @param name The name for this object
 * @param id The ID for this object
 */
BaseClass::BaseClass(const std::string& name, int id) : name(name), id(id) {
    if (id >= nextId) {
        nextId = id + 1;
    }
    initialize();
}

/**
 * Copy constructor
 * @param other The BaseClass object to copy from
 */
BaseClass::BaseClass(const BaseClass& other) : name(other.name), id(nextId++) {
    initialize();
}

/**
 * Assignment operator
 * @param other The BaseClass object to assign from
 * @return Reference to this object
 */
BaseClass& BaseClass::operator=(const BaseClass& other) {
    if (this != &other) {
        name = other.name;
        // Note: ID is not copied in assignment, each object keeps its unique ID
    }
    return *this;
}

/**
 * Virtual destructor
 * Ensures proper cleanup for derived classes
 */
BaseClass::~BaseClass() {
    // Cleanup code if needed
}

/**
 * Get the object name
 * @return Constant reference to the name string
 */
const std::string& BaseClass::getName() const {
    return name;
}

/**
 * Get the object ID
 * @return The object ID
 */
int BaseClass::getId() const {
    return id;
}

/**
 * Get the next available ID
 * @return The next ID that will be assigned
 */
int BaseClass::getNextId() {
    return nextId;
}

/**
 * Set the object name
 * @param newName The new name for this object
 */
void BaseClass::setName(const std::string& newName) {
    if (!newName.empty()) {
        name = newName;
    }
}

/**
 * Set the object ID (protected method)
 * @param newId The new ID for this object
 */
void BaseClass::setId(int newId) {
    id = newId;
}

/**
 * Display object information
 * Virtual method that can be overridden by derived classes
 */
void BaseClass::display() const {
    std::cout << "BaseClass Object:" << std::endl;
    std::cout << "  Name: " << name << std::endl;
    std::cout << "  ID: " << id << std::endl;
}

/**
 * Convert object to string representation
 * @return String representation of the object
 */
std::string BaseClass::toString() const {
    std::stringstream ss;
    ss << "BaseClass(name=\"" << name << "\", id=" << id << ")";
    return ss.str();
}

/**
 * Initialize the object
 * Called by constructors to set up common initialization
 */
void BaseClass::initialize() {
    // Common initialization logic
    if (name.empty()) {
        name = "UnnamedObject";
    }
}

/**
 * Check if the object is in a valid state
 * @return true if the object is valid, false otherwise
 */
bool BaseClass::isValid() const {
    return !name.empty() && id > 0;
}

/**
 * Equality operator
 * @param other The BaseClass object to compare with
 * @return true if objects are equal, false otherwise
 */
bool BaseClass::operator==(const BaseClass& other) const {
    return name == other.name && id == other.id;
}

/**
 * Inequality operator
 * @param other The BaseClass object to compare with
 * @return true if objects are not equal, false otherwise
 */
bool BaseClass::operator!=(const BaseClass& other) const {
    return !(*this == other);
}

/**
 * Output stream operator
 * @param os The output stream
 * @param obj The BaseClass object to output
 * @return Reference to the output stream
 */
std::ostream& operator<<(std::ostream& os, const BaseClass& obj) {
    os << obj.toString();
    return os;
}
`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Get file extension for language
 */
function getFileExtension(language) {
    const extensions = {
        'c': 'c',
        'cpp': 'cpp'
    };
    return extensions[language] || 'txt';
}

/**
 * Generate TSI header content using Ruby CLI
 */
async function generateTSIHeaderContent(fileName) {
    try {
        // Use the Ruby CLI to generate proper TSI headers
        const { execSync } = require('child_process');
        const path = require('path');
        
        // Create a temporary file to generate header for
        const os = require('os');
        const fs = require('fs');
        const tempDir = os.tmpdir();
        const tempFile = path.join(tempDir, fileName);
        
        // Write a dummy file content
        fs.writeFileSync(tempFile, '// Temporary file for header generation\n');
        
        // Get extension path - we need to find where the Ruby CLI is
        // In VS Code extension context, we need to get the extension path
        let extensionPath;
        try {
            // Try to get from VS Code context
            extensionPath = vscode.extensions.getExtension('st93642.tsi-header').extensionPath;
        } catch (e) {
            // Fallback to current working directory
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
        
        // Detect language from file extension
        const ext = path.extname(fileName).toLowerCase();
        let language = 'plaintext';
        if (ext === '.c') language = 'c';
        else if (ext === '.cpp' || ext === '.cxx' || ext === '.cc') language = 'cpp';
        else if (ext === '.h') language = 'c';
        else if (ext === '.hpp' || ext === '.hxx') language = 'cpp';
        
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
        return generateFallbackHeader(fileName);
    }
}

/**
 * Fallback header generation if Ruby CLI is not available
 */
function generateFallbackHeader(fileName) {
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
    createTSIProject
};