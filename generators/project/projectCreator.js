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
        },
        {
            label: '$(code) Python Project',
            description: 'Python scripting and development',
            detail: 'Creates: main.py, classes, requirements.txt, documentation',
            value: 'python'
        },
        {
            label: '$(tools) Java Project',
            description: 'Object-oriented Java development',
            detail: 'Creates: Main.java, classes, pom.xml, documentation',
            value: 'java'
        },
        {
            label: '$(package) Rust Project',
            description: 'Systems programming with memory safety',
            detail: 'Creates: main.rs, lib.rs, Cargo.toml, documentation',
            value: 'rust'
        },
        {
            label: '$(ruby) Ruby Project',
            description: 'Dynamic programming with elegant syntax',
            detail: 'Creates: main.rb, classes, Gemfile, documentation',
            value: 'ruby'
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
    
    // Create Python-specific files
    if (language === 'python') {
        await createPythonFiles(projectName, projectUri);
    }
    
    // Create Java-specific files
    if (language === 'java') {
        await createJavaFiles(projectName, projectUri);
    }
    
    // Create Rust-specific files
    if (language === 'rust') {
        await createRustFiles(projectName, projectUri);
    }
    
    // Create Ruby-specific files
    if (language === 'ruby') {
        await createRubyFiles(projectName, projectUri);
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
    }
    
    return commonDirs;
}

/**
 * Create main source file with TSI header and code
 */
async function createMainSourceFile(language, projectName, projectUri) {
    const extension = getFileExtension(language);
    let fileName = `main.${extension}`;
    let fileUri;
    
    if (language === 'java') {
        // For Java, create Main.java in the proper package structure
        fileName = 'Main.java';
        fileUri = vscode.Uri.joinPath(projectUri, 'src', 'main', 'java', fileName);
    } else {
        fileUri = vscode.Uri.joinPath(projectUri, 'src', fileName);
    }
    
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
 * Create Python-specific files
 */
async function createPythonFiles(projectName, projectUri) {
    await createRequirementsTxt(projectName, projectUri);
    await createSetupPy(projectName, projectUri);
    await createBaseClassPy(projectName, projectUri);
}

/**
 * Create requirements.txt file
 */
async function createRequirementsTxt(projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'requirements.txt');
    
    const content = `# ${projectName} requirements
# Add your project dependencies here
# Example:
# requests>=2.25.1
# numpy>=1.21.0
# pandas>=1.3.0
`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create setup.py file
 */
async function createSetupPy(projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'setup.py');
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('setup.py');
    
    const content = `${headerContent}

"""
Setup script for ${projectName}
"""

from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="${projectName}",
    version="0.1.0",
    author="TSI Student",
    author_email="student@tsi.lv",
    description="A ${projectName} project created with TSI Header",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/yourusername/${projectName}",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
    ],
    python_requires=">=3.8",
    install_requires=[
        # Add your dependencies here
    ],
    extras_require={
        "dev": [
            "pytest>=6.0",
            "pytest-cov>=2.0",
            "black>=21.0",
            "flake8>=3.9",
            "mypy>=0.910",
        ],
    },
)
`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create BaseClass.py file for Python projects
 */
async function createBaseClassPy(projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', 'base_class.py');
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('base_class.py');
    
    const content = `${headerContent}

"""
BaseClass - Foundation class for ${projectName}

This class provides basic functionality and serves as a base
for other classes in the ${projectName} project.

Features:
- Basic object initialization
- String-based naming system
- Unique ID generation
- Display functionality
- JSON serialization support
"""

import json
from typing import Dict, Any, Optional
from datetime import datetime


class BaseClass:
    """Base class providing common functionality for ${projectName} objects."""
    
    # Class variable for ID generation
    _next_id = 1
    
    def __init__(self, name: str = "DefaultObject", obj_id: Optional[int] = None):
        """
        Initialize BaseClass object.
        
        Args:
            name: Name for this object
            obj_id: Optional specific ID, auto-generated if not provided
        """
        self._name = name if name else "UnnamedObject"
        self._id = obj_id if obj_id is not None else BaseClass._next_id
        if obj_id is None or obj_id >= BaseClass._next_id:
            BaseClass._next_id = self._id + 1
        
        self._created_at = datetime.now()
        self._initialize()
    
    def _initialize(self) -> None:
        """Initialize common object properties."""
        pass
    
    @property
    def name(self) -> str:
        """Get the object name."""
        return self._name
    
    @name.setter
    def name(self, new_name: str) -> None:
        """Set the object name."""
        if new_name and isinstance(new_name, str):
            self._name = new_name
    
    @property
    def id(self) -> int:
        """Get the object ID."""
        return self._id
    
    @classmethod
    def get_next_id(cls) -> int:
        """Get the next available ID."""
        return cls._next_id
    
    def display(self) -> None:
        """Display object information."""
        print(f"{self.__class__.__name__} Object:")
        print(f"  Name: {self._name}")
        print(f"  ID: {self._id}")
        print(f"  Created: {self._created_at.strftime('%Y-%m-%d %H:%M:%S')}")
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert object to dictionary for serialization."""
        return {
            'name': self._name,
            'id': self._id,
            'class': self.__class__.__name__,
            'created_at': self._created_at.isoformat()
        }
    
    def to_json(self) -> str:
        """Convert object to JSON string."""
        return json.dumps(self.to_dict(), indent=2, default=str)
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'BaseClass':
        """Create object from dictionary."""
        return cls(
            name=data.get('name', 'DefaultObject'),
            obj_id=data.get('id')
        )
    
    @classmethod
    def from_json(cls, json_str: str) -> 'BaseClass':
        """Create object from JSON string."""
        data = json.loads(json_str)
        return cls.from_dict(data)
    
    def __str__(self) -> str:
        """String representation of the object."""
        return f"{self.__class__.__name__}(name=\"{self._name}\", id={self._id})"
    
    def __repr__(self) -> str:
        """Detailed string representation."""
        return self.__str__()
    
    def __eq__(self, other: object) -> bool:
        """Check equality with another object."""
        if not isinstance(other, BaseClass):
            return False
        return self._name == other._name and self._id == other._id
    
    def __hash__(self) -> int:
        """Hash function for use in sets/dicts."""
        return hash((self._name, self._id))


# Example usage
if __name__ == "__main__":
    # Create some example objects
    obj1 = BaseClass("Example Object 1")
    obj2 = BaseClass("Example Object 2")
    
    print("Created objects:")
    obj1.display()
    print()
    obj2.display()
    print()
    
    print("JSON representation:")
    print(obj1.to_json())
    print()
    
    print("String representations:")
    print(f"str(): {str(obj1)}")
    print(f"repr(): {repr(obj1)}")
`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create Java-specific files
 */
async function createJavaFiles(projectName, projectUri) {
    await createBaseClassJava(projectName, projectUri);
}

/**
 * Create BaseClass.java file for Java projects
 */
async function createBaseClassJava(projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', 'main', 'java', 'BaseClass.java');
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('BaseClass.java');
    
    const content = `${headerContent}

/**
 * BaseClass - Foundation class for ${projectName}
 * 
 * This class provides basic functionality and serves as a base
 * for other classes in the ${projectName} project.
 * 
 * Features:
 * - Basic object initialization
 * - String-based naming system
 * - Unique ID generation
 * - Display functionality
 * - JSON serialization support
 */
public class BaseClass {
    // Static counter for ID generation
    private static int nextId = 1;
    
    // Instance variables
    private String name;
    private int id;
    private java.time.LocalDateTime createdAt;
    
    /**
     * Default constructor
     */
    public BaseClass() {
        this("DefaultObject");
    }
    
    /**
     * Constructor with name
     * @param name The name for this object
     */
    public BaseClass(String name) {
        this.name = name != null ? name : "UnnamedObject";
        this.id = nextId++;
        this.createdAt = java.time.LocalDateTime.now();
        initialize();
    }
    
    /**
     * Constructor with name and ID
     * @param name The name for this object
     * @param id The specific ID for this object
     */
    public BaseClass(String name, int id) {
        this.name = name != null ? name : "UnnamedObject";
        this.id = id;
        if (id >= nextId) {
            nextId = id + 1;
        }
        this.createdAt = java.time.LocalDateTime.now();
        initialize();
    }
    
    /**
     * Initialize common object properties
     */
    protected void initialize() {
        // Override in subclasses for custom initialization
    }
    
    /**
     * Get the object name
     * @return The object name
     */
    public String getName() {
        return name;
    }
    
    /**
     * Set the object name
     * @param name The new name for this object
     */
    public void setName(String name) {
        if (name != null && !name.trim().isEmpty()) {
            this.name = name;
        }
    }
    
    /**
     * Get the object ID
     * @return The object ID
     */
    public int getId() {
        return id;
    }
    
    /**
     * Get the next available ID
     * @return The next available ID
     */
    public static int getNextId() {
        return nextId;
    }
    
    /**
     * Get the creation timestamp
     * @return The creation timestamp
     */
    public java.time.LocalDateTime getCreatedAt() {
        return createdAt;
    }
    
    /**
     * Display object information
     */
    public void display() {
        System.out.println(this.getClass().getSimpleName() + " Object:");
        System.out.println("  Name: " + name);
        System.out.println("  ID: " + id);
        System.out.println("  Created: " + createdAt.format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
    }
    
    /**
     * Convert object to JSON string
     * @return JSON representation of the object
     */
    public String toJson() {
        return String.format(
            "{\\"name\\":\\"%s\\", \\"id\\":%d, \\"class\\":\\"%s\\", \\"createdAt\\":\\"%s\\"}",
            name.replace("\\"", "\\\\\\""),
            id,
            this.getClass().getSimpleName(),
            createdAt.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        );
    }
    
    /**
     * Check if object is valid
     * @return true if object is valid
     */
    public boolean isValid() {
        return name != null && !name.trim().isEmpty() && id > 0;
    }
    
    /**
     * String representation of the object
     * @return String representation
     */
    @Override
    public String toString() {
        return String.format("%s(name=\\"%s\\", id=%d)",
            this.getClass().getSimpleName(), name, id);
    }
    
    /**
     * Check equality with another object
     * @param obj The object to compare with
     * @return true if objects are equal
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        
        BaseClass that = (BaseClass) obj;
        return id == that.id && name.equals(that.name);
    }
    
    /**
     * Generate hash code for the object
     * @return Hash code
     */
    @Override
    public int hashCode() {
        return java.util.Objects.hash(name, id);
    }
    
    // Example usage
    public static void main(String[] args) {
        // Create some example objects
        BaseClass obj1 = new BaseClass("Example Object 1");
        BaseClass obj2 = new BaseClass("Example Object 2");
        
        System.out.println("Created objects:");
        obj1.display();
        System.out.println();
        obj2.display();
        System.out.println();
        
        System.out.println("JSON representation:");
        System.out.println(obj1.toJson());
        System.out.println();
        
        System.out.println("String representations:");
        System.out.println("toString(): " + obj1.toString());
        System.out.println("equals(): " + obj1.equals(obj2));
    }
}`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create Rust-specific files
 */
async function createRustFiles(projectName, projectUri) {
    await createCargoToml(projectName, projectUri);
    await createLibRs(projectName, projectUri);
    await createBaseStructRs(projectName, projectUri);
}

/**
 * Create Cargo.toml file for Rust projects
 */
async function createCargoToml(projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'Cargo.toml');
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('Cargo.toml');
    
    const content = `${headerContent}

[package]
name = "${projectName}"
version = "0.1.0"
edition = "2021"
authors = ["TSI Student <student@tsi.lv>"]
description = "A ${projectName} project created with TSI Header"
license = "MIT"
repository = "https://github.com/yourusername/${projectName}"

[dependencies]
# Add your dependencies here
# Example:
# serde = { version = "1.0", features = ["derive"] }
# tokio = { version = "1.0", features = ["full"] }

[dev-dependencies]
# Add your dev dependencies here
# Example:
# rstest = "0.15"
`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create lib.rs file for Rust projects
 */
async function createLibRs(projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', 'lib.rs');
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('lib.rs');
    
    const content = `${headerContent}

//! # ${projectName}
//!
//! A library for ${projectName} functionality.
//!
//! This library provides the core functionality for the ${projectName} project,
//! including base structures and common utilities.

pub mod base_struct;

/// Re-export commonly used items
pub use base_struct::BaseStruct;

/// Library version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Get library information
pub fn get_info() -> String {
    format!("{} v{}", env!("CARGO_PKG_NAME"), VERSION)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_info() {
        let info = get_info();
        assert!(info.contains("${projectName}"));
        assert!(info.contains("0.1.0"));
    }
}
`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create BaseStruct.rs file for Rust projects
 */
async function createBaseStructRs(projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', 'base_struct.rs');
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('base_struct.rs');
    
    const content = `${headerContent}

//! BaseStruct - Foundation struct for ${projectName}
//!
//! This struct provides basic functionality and serves as a base
//! for other structs in the ${projectName} project.
//!
//! Features:
//! - Basic object initialization
//! - String-based naming system
//! - Unique ID generation
//! - Display functionality
//! - JSON serialization support (with serde)

use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};

/// BaseStruct - Foundation struct for ${projectName}
///
/// This struct provides common functionality that can be embedded
/// or inherited by other structs in the project.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseStruct {
    name: String,
    id: u64,
    created_at: SystemTime,
}

impl BaseStruct {
    /// Static counter for ID generation
    static mut NEXT_ID: u64 = 1;
    
    /// Create a new BaseStruct with default values
    pub fn new() -> Self {
        Self::with_name("DefaultObject".to_string())
    }
    
    /// Create a new BaseStruct with a specific name
    pub fn with_name(name: String) -> Self {
        let id = Self::get_next_id();
        let created_at = SystemTime::now();
        
        Self {
            name: if name.is_empty() { "UnnamedObject".to_string() } else { name },
            id,
            created_at,
        }
    }
    
    /// Create a new BaseStruct with name and specific ID
    pub fn with_name_and_id(name: String, id: u64) -> Self {
        let created_at = SystemTime::now();
        
        Self {
            name: if name.is_empty() { "UnnamedObject".to_string() } else { name },
            id,
            created_at,
        }
    }
    
    /// Get the next available ID (thread-unsafe for simplicity)
    fn get_next_id() -> u64 {
        unsafe {
            let id = Self::NEXT_ID;
            Self::NEXT_ID += 1;
            id
        }
    }
    
    /// Get the object name
    pub fn name(&self) -> &str {
        &self.name
    }
    
    /// Set the object name
    pub fn set_name(&mut self, name: String) {
        if !name.is_empty() {
            self.name = name;
        }
    }
    
    /// Get the object ID
    pub fn id(&self) -> u64 {
        self.id
    }
    
    /// Get the creation timestamp
    pub fn created_at(&self) -> SystemTime {
        self.created_at
    }
    
    /// Get the next available ID (for external use)
    pub fn next_id() -> u64 {
        Self::get_next_id()
    }
    
    /// Display object information
    pub fn display(&self) {
        println!("{} Object:", std::any::type_name::<Self>());
        println!("  Name: {}", self.name);
        println!("  ID: {}", self.id);
        
        if let Ok(duration) = self.created_at.duration_since(UNIX_EPOCH) {
            println!("  Created: {} seconds since epoch", duration.as_secs());
        }
    }
    
    /// Check if the object is valid
    pub fn is_valid(&self) -> bool {
        !self.name.is_empty() && self.id > 0
    }
    
    /// Convert to a simple string representation
    pub fn to_string(&self) -> String {
        format!("{} {{ name: \"{}\", id: {} }}", 
                std::any::type_name::<Self>(), self.name, self.id)
    }
    
    /// Get creation timestamp as seconds since epoch
    pub fn created_at_secs(&self) -> u64 {
        self.created_at
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs()
    }
}

impl Default for BaseStruct {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for BaseStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[cfg(feature = "serde")]
mod serde_support {
    use super::*;
    use serde::{Deserialize, Serialize};
    
    #[derive(Serialize, Deserialize)]
    struct BaseStructData {
        name: String,
        id: u64,
        created_at: u64,
    }
    
    impl BaseStruct {
        /// Convert to JSON string (requires serde feature)
        pub fn to_json(&self) -> Result<String, serde_json::Error> {
            let data = BaseStructData {
                name: self.name.clone(),
                id: self.id,
                created_at: self.created_at_secs(),
            };
            serde_json::to_string_pretty(&data)
        }
        
        /// Create from JSON string (requires serde feature)
        pub fn from_json(json_str: &str) -> Result<Self, serde_json::Error> {
            let data: BaseStructData = serde_json::from_str(json_str)?;
            Ok(Self::with_name_and_id(data.name, data.id))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_new() {
        let obj = BaseStruct::new();
        assert_eq!(obj.name(), "DefaultObject");
        assert!(obj.id() > 0);
        assert!(obj.is_valid());
    }
    
    #[test]
    fn test_with_name() {
        let obj = BaseStruct::with_name("Test Object".to_string());
        assert_eq!(obj.name(), "Test Object");
        assert!(obj.id() > 0);
        assert!(obj.is_valid());
    }
    
    #[test]
    fn test_with_name_and_id() {
        let obj = BaseStruct::with_name_and_id("Test Object".to_string(), 42);
        assert_eq!(obj.name(), "Test Object");
        assert_eq!(obj.id(), 42);
        assert!(obj.is_valid());
    }
    
    #[test]
    fn test_set_name() {
        let mut obj = BaseStruct::new();
        obj.set_name("New Name".to_string());
        assert_eq!(obj.name(), "New Name");
    }
    
    #[test]
    fn test_display() {
        let obj = BaseStruct::with_name("Test".to_string());
        // Just ensure it doesn't panic
        obj.display();
    }
    
    #[test]
    fn test_to_string() {
        let obj = BaseStruct::with_name_and_id("Test".to_string(), 123);
        let s = obj.to_string();
        assert!(s.contains("Test"));
        assert!(s.contains("123"));
    }
    
    #[test]
    fn test_equality() {
        let obj1 = BaseStruct::with_name_and_id("Test".to_string(), 123);
        let obj2 = BaseStruct::with_name_and_id("Test".to_string(), 123);
        let obj3 = BaseStruct::with_name_and_id("Different".to_string(), 123);
        
        assert_eq!(obj1, obj2);
        assert_ne!(obj1, obj3);
    }
    
    #[cfg(feature = "serde")]
    #[test]
    fn test_json_serialization() {
        let obj = BaseStruct::with_name_and_id("Test Object".to_string(), 42);
        
        let json = obj.to_json().unwrap();
        let deserialized = BaseStruct::from_json(&json).unwrap();
        
        assert_eq!(obj, deserialized);
    }
}

// Example usage
#[cfg(not(test))]
fn main() {
    // Create some example objects
    let obj1 = BaseStruct::with_name("Example Object 1".to_string());
    let obj2 = BaseStruct::with_name("Example Object 2".to_string());
    
    println!("Created objects:");
    obj1.display();
    println!();
    obj2.display();
    println!();
    
    println!("String representations:");
    println!("to_string(): {}", obj1.to_string());
    println!("Display: {}", obj1);
    println!("Debug: {:?}", obj1);
    
    println!();
    println!("Equality test: {}", obj1 == obj2);
}`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create Ruby-specific files
 */
async function createRubyFiles(projectName, projectUri) {
    await createGemfile(projectName, projectUri);
    await createRakefile(projectName, projectUri);
    await createBaseClassRb(projectName, projectUri);
}

/**
 * Create Gemfile for Ruby projects
 */
async function createGemfile(projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'Gemfile');
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('Gemfile');
    
    const content = `${headerContent}

source 'https://rubygems.org'

# Specify your gem's dependencies in ${projectName}.gemspec
gemspec

# Development dependencies
group :development do
  gem 'bundler', '~> 2.0'
  gem 'rake', '>= 12.3.3'
  gem 'rspec', '~> 3.0'
  gem 'rubocop', '>= 1.0', '< 2.0'
  gem 'yard', '~> 0.9'
end

# Test dependencies
group :test do
  gem 'simplecov', '~> 0.21'
  gem 'webmock', '~> 3.14'
end
`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create Rakefile for Ruby projects
 */
async function createRakefile(projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'Rakefile');
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('Rakefile');
    
    const content = `${headerContent}

require 'bundler/gem_tasks'
require 'rspec/core/rake_task'
require 'rubocop/rake_task'

# Default task
task default: [:spec, :rubocop]

# RSpec task
RSpec::Core::RakeTask.new(:spec) do |task|
  task.pattern = 'spec/**/*_spec.rb'
  task.rspec_opts = '--format documentation --color'
end

# RuboCop task
RuboCop::RakeTask.new

# Test task
task test: :spec

# Documentation task
require 'yard'
YARD::Rake::YardocTask.new do |t|
  t.files = ['lib/**/*.rb']
  t.options = ['--readme', 'README.md']
end

# Clean task
task :clean do
  FileUtils.rm_rf('pkg')
  FileUtils.rm_rf('coverage')
  FileUtils.rm_rf('.yardoc')
end

desc 'Run all checks'
task ci: [:spec, :rubocop, :yard]

desc 'Release gem'
task release: [:ci, :build] do
  # Add release logic here
end
`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create BaseClass.rb file for Ruby projects
 */
async function createBaseClassRb(projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'lib', 'base_class.rb');
    
    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('base_class.rb');
    
    const content = `${headerContent}

# BaseClass - Foundation class for ${projectName}
#
# This class provides basic functionality and serves as a base
# for other classes in the ${projectName} project.
#
# Features:
# - Basic object initialization
# - String-based naming system
# - Unique ID generation
# - Display functionality
# - JSON serialization support
# - Method chaining support
# - Validation and error handling

require 'json'
require 'time'

class BaseClass
  # Class variable for ID generation
  @@next_id = 1
  
  # Accessors
  attr_reader :name, :id, :created_at
  attr_accessor :description
  
  # Constructor
  def initialize(name = 'DefaultObject', id = nil)
    @name = name.to_s.empty? ? 'UnnamedObject' : name.to_s
    @id = id || self.class.generate_next_id
    @created_at = Time.now
    @description = ''
    
    # Initialize the object
    initialize_object
  end
  
  # Class method to generate next ID
  def self.generate_next_id
    id = @@next_id
    @@next_id += 1
    id
  end
  
  # Class method to get next available ID
  def self.next_id
    @@next_id
  end
  
  # Instance methods
  
  # Set the object name with validation
  def name=(new_name)
    @name = new_name.to_s.empty? ? 'UnnamedObject' : new_name.to_s
  end
  
  # Display object information
  def display
    puts "\#{self.class.name} Object:"
    puts "  Name: \#{@name}"
    puts "  ID: \#{@id}"
    puts "  Description: \#{@description.empty? ? '(none)' : @description}"
    puts "  Created: \#{@created_at.strftime('%Y-%m-%d %H:%M:%S')}"
  end
  
  # Convert to hash for serialization
  def to_hash
    {
      'name' => @name,
      'id' => @id,
      'class' => self.class.name,
      'description' => @description,
      'created_at' => @created_at.iso8601
    }
  end
  
  # Convert to JSON string
  def to_json(*args)
    to_hash.to_json(*args)
  end
  
  # Create object from hash
  def self.from_hash(hash)
    obj = new(hash['name'], hash['id'])
    obj.description = hash['description'] || ''
    obj.instance_variable_set(:@created_at, Time.parse(hash['created_at'])) if hash['created_at']
    obj
  end
  
  # Create object from JSON string
  def self.from_json(json_string)
    hash = JSON.parse(json_string)
    from_hash(hash)
  end
  
  # Check if object is valid
  def valid?
    !@name.empty? && @id.is_a?(Integer) && @id > 0
  end
  
  # Get object information as string
  def info
    "\#{self.class.name}(name=\"\#{@name}\", id=\#{@id})"
  end
  
  # String representation
  def to_s
    info
  end
  
  # Inspect representation
  def inspect
    "\#<\#{self.class.name} \#{info}>"
  end
  
  # Equality comparison
  def ==(other)
    return false unless other.is_a?(BaseClass)
    @name == other.name && @id == other.id
  end
  
  # Hash for use in Hash and Set
  def hash
    [@name, @id].hash
  end
  
  # Eql? for Hash key comparison
  def eql?(other)
    self == other
  end
  
  # Method chaining support
  def with_name(name)
    self.name = name
    self
  end
  
  def with_description(description)
    self.description = description
    self
  end
  
  # Clone with modifications
  def clone_with(**attributes)
    cloned = self.clone
    attributes.each do |key, value|
      cloned.send("\#{key}=", value) if cloned.respond_to?("\#{key}=")
    end
    cloned
  end
  
  protected
  
  # Initialize hook for subclasses
  def initialize_object
    # Override in subclasses for custom initialization
  end
  
  private
  
  # Private helper methods
  def validate_name(name)
    name.to_s.strip.empty? ? 'UnnamedObject' : name.to_s.strip
  end
end

# Example usage (only when run directly)
if __FILE__ == $PROGRAM_NAME
  # Create some example objects
  obj1 = BaseClass.new('Example Object 1')
  obj2 = BaseClass.new('Example Object 2')
  
  puts 'Created objects:'
  obj1.display
  puts
  obj2.display
  puts
  
  puts 'JSON representation:'
  puts obj1.to_json
  puts
  
  puts 'String representations:'
  puts "to_s(): \#{obj1.to_s}"
  puts "inspect(): \#{obj1.inspect}"
  puts "equal?: \#{obj1 == obj2}"
  
  # Demonstrate method chaining
  puts
  puts 'Method chaining:'
  obj3 = BaseClass.new.with_name('Chained Object').with_description('Created with chaining')
  obj3.display
end`;
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Get file extension for language
 */
function getFileExtension(language) {
    const extensions = {
        'c': 'c',
        'cpp': 'cpp',
        'python': 'py',
        'java': 'java',
        'rust': 'rs',
        'ruby': 'rb'
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
        else if (ext === '.java') language = 'java';
        else if (ext === '.rs') language = 'rust';
        else if (ext === '.rb') language = 'ruby';
        
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