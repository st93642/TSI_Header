/**
 * Class Generators Module
 * Contains language-specific class generation logic
 */

/**
 * Generates class code for the specified language
 * @param {string} languageId - The language identifier
 * @param {string} className - The name of the class to generate
 * @param {string} fileName - The file name (for extension detection in plaintext)
 * @param {string} extensionPath - The extension path (for CLI access)
 * @param {string} cliPath - The CLI path
 * @param {object} env - Environment variables
 * @returns {object} Result object with success, content/files properties
 */
function generateClass(languageId, className, fileName, extensionPath, cliPath, env) {
    switch (languageId) {
        case 'java':
            return { success: true, content: generateJavaClass(className) };
        case 'cpp':
        case 'c++':
            return generateCppClass(className, fileName, extensionPath, cliPath, env);
        case 'csharp':
            return { success: true, content: generateCSharpClass(className) };
        case 'python':
            return { success: true, content: generatePythonClass(className) };
        case 'javascript':
            return { success: true, content: generateJavaScriptClass(className) };
        case 'kotlin':
            return { success: true, content: generateKotlinClass(className) };
        case 'php':
            return { success: true, content: generatePhpClass(className) };
        case 'plaintext':
            // Handle plaintext files (may be Kotlin .kt files)
            const fileExtension = fileName.split('.').pop().toLowerCase();
            if (fileExtension === 'kt') {
                return { success: true, content: generateKotlinClass(className) };
            } else {
                return { success: false, message: `Class generation not supported for plaintext files with extension: ${fileExtension}` };
            }
        default:
            return { success: false, message: `Class generation not supported for language: ${languageId}` };
    }
}

/**
 * Generates Java class code
 */
function generateJavaClass(className) {
    return `public class ${className} {
    // Class attributes
    private String name;
    private int id;

    // Default constructor
    public ${className}() {
        this.name = "";
        this.id = 0;
    }

    // Parameterized constructor
    public ${className}(String name, int id) {
        this.name = name;
        this.id = id;
    }

    // Getter for name
    public String getName() {
        return name;
    }

    // Setter for name
    public void setName(String name) {
        this.name = name;
    }

    // Getter for id
    public int getId() {
        return id;
    }

    // Setter for id
    public void setId(int id) {
        this.id = id;
    }

    // Override toString method
    @Override
    public String toString() {
        return "${className}{" +
                "name='" + name + '\\'' +
                ", id=" + id +
                '}';
    }

    // Override equals method
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        ${className} that = (${className}) obj;
        return id == that.id && name.equals(that.name);
    }

    // Override hashCode method
    @Override
    public int hashCode() {
        return name.hashCode() * 31 + id;
    }
}`;
}

/**
 * Generates C++ class code (creates separate .hpp and .cpp files)
 */
function generateCppClass(className, fileName, extensionPath, cliPath, env) {
    const vscode = require('vscode');
    const path = require('path');
    const { execSync } = require('child_process');

    const currentDir = path.dirname(fileName);
    const headerFileName = `${className}.hpp`;
    const implFileName = `${className}.cpp`;
    const headerFilePath = path.join(currentDir, headerFileName);
    const implFilePath = path.join(currentDir, implFileName);

    // Check if files already exist
    const fs = require('fs');
    if (fs.existsSync(headerFilePath)) {
        return { success: false, message: `File '${headerFileName}' already exists. Please choose a different class name or delete the existing file.` };
    }
    if (fs.existsSync(implFilePath)) {
        return { success: false, message: `File '${implFileName}' already exists. Please choose a different class name or delete the existing file.` };
    }

    try {
        // Generate header file content
        const headerCommand = `ruby "${cliPath}" insert "${'cpp'}" "${headerFilePath}"`;
        const headerResult = execSync(headerCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
        const headerResponse = JSON.parse(headerResult);
        
        if (!headerResponse.success) {
            return { success: false, message: `Failed to generate header for ${headerFileName}: ${headerResponse.message}` };
        }

        const headerGuard = `${className.toUpperCase()}_HPP`;
        let headerContent = headerResponse.header;
        headerContent += `\n#ifndef ${headerGuard}\n#define ${headerGuard}\n\n#include <string>\n#include <iostream>\n\nclass ${className} {\nprivate:\n    std::string name;\n    int id;\n\npublic:\n    // Default constructor\n    ${className}();\n\n    // Parameterized constructor\n    ${className}(const std::string& name, int id);\n\n    // Copy constructor\n    ${className}(const ${className}& other);\n\n    // Destructor\n    ~${className}();\n\n    // Assignment operator\n    ${className}& operator=(const ${className}& other);\n\n    // Getters\n    std::string getName() const;\n    int getId() const;\n\n    // Setters\n    void setName(const std::string& name);\n    void setId(int id);\n\n    // Utility methods\n    void display() const;\n    bool equals(const ${className}& other) const;\n};\n\n#endif // ${headerGuard}\n`;

        // Generate implementation file content
        const implCommand = `ruby "${cliPath}" insert "${'cpp'}" "${implFilePath}"`;
        const implResult = execSync(implCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
        const implResponse = JSON.parse(implResult);
        
        if (!implResponse.success) {
            return { success: false, message: `Failed to generate header for ${implFileName}: ${implResponse.message}` };
        }

        let implContent = implResponse.header;
        implContent += `\n#include "${headerFileName}"\n\n// Default constructor\n${className}::${className}() : name(""), id(0) {}\n\n// Parameterized constructor\n${className}::${className}(const std::string& name, int id) : name(name), id(id) {}\n\n// Copy constructor\n${className}::${className}(const ${className}& other) : name(other.name), id(other.id) {}\n\n// Destructor\n${className}::~${className}() {}\n\n// Assignment operator\n${className}& ${className}::operator=(const ${className}& other) {\n    if (this != &other) {\n        name = other.name;\n        id = other.id;\n    }\n    return *this;\n}\n\n// Getters\nstd::string ${className}::getName() const {\n    return name;\n}\n\nint ${className}::getId() const {\n    return id;\n}\n\n// Setters\nvoid ${className}::setName(const std::string& name) {\n    this->name = name;\n}\n\nvoid ${className}::setId(int id) {\n    this->id = id;\n}\n\n// Utility methods\nvoid ${className}::display() const {\n    std::cout << "${className}{name='" << name << "', id=" << id << "}" << std::endl;\n}\n\nbool ${className}::equals(const ${className}& other) const {\n    return name == other.name && id == other.id;\n}\n`;

        // Write both files
        fs.writeFileSync(headerFilePath, headerContent);
        fs.writeFileSync(implFilePath, implContent);

        return { 
            success: true, 
            files: true, 
            message: `TSI Header: Created C++ class "${className}" (${headerFileName} + ${implFileName})` 
        };
    } catch (error) {
        return { success: false, message: `Failed to create C++ class files: ${error.message}` };
    }
}

/**
 * Generates C# class code
 */
function generateCSharpClass(className) {
    return `using System;

namespace ${className}Namespace
{
    public class ${className}
    {
        // Private fields
        private string name;
        private int id;

        // Default constructor
        public ${className}()
        {
            this.name = "";
            this.id = 0;
        }

        // Parameterized constructor
        public ${className}(string name, int id)
        {
            this.name = name;
            this.id = id;
        }

        // Copy constructor
        public ${className}(${className} other)
        {
            this.name = other.name;
            this.id = other.id;
        }

        // Properties
        public string Name
        {
            get { return name; }
            set { name = value; }
        }

        public int Id
        {
            get { return id; }
            set { id = value; }
        }

        // Override ToString method
        public override string ToString()
        {
            return $"${className}{{Name='{name}', Id={id}}}";
        }

        // Override Equals method
        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }
            ${className} other = (${className})obj;
            return name == other.name && id == other.id;
        }

        // Override GetHashCode method
        public override int GetHashCode()
        {
            return name.GetHashCode() * 31 + id;
        }

        // Display method
        public void Display()
        {
            Console.WriteLine(ToString());
        }
    }
}`;
}

/**
 * Generates Python class code
 */
function generatePythonClass(className) {
    return `class ${className}:
    """${className} class with basic functionality."""

    def __init__(self, name="", id_num=0):
        """Initialize ${className} instance.

        Args:
            name (str): The name attribute
            id_num (int): The ID attribute
        """
        self._name = name
        self._id = id_num

    @property
    def name(self):
        """Get the name attribute."""
        return self._name

    @name.setter
    def name(self, value):
        """Set the name attribute."""
        if not isinstance(value, str):
            raise TypeError("Name must be a string")
        self._name = value

    @property
    def id(self):
        """Get the ID attribute."""
        return self._id

    @id.setter
    def id(self, value):
        """Set the ID attribute."""
        if not isinstance(value, int):
            raise TypeError("ID must be an integer")
        self._id = value

    def __str__(self):
        """String representation of the object."""
        return f"${className}{{name='{self._name}', id={self._id}}}"

    def __repr__(self):
        """Official string representation of the object."""
        return f"${className}('{self._name}', {self._id})"

    def __eq__(self, other):
        """Check equality with another object."""
        if not isinstance(other, ${className}):
            return NotImplemented
        return self._name == other._name and self._id == other._id

    def __hash__(self):
        """Hash function for the object."""
        return hash((self._name, self._id))

    def display(self):
        """Display the object information."""
        print(f"${className}{{name='{self._name}', id={self._id}}}")


// Example usage
if __name__ == "__main__":
    # Create an instance
    obj = ${className}("Example", 123)
    obj.display()

    # Test property setters
    obj.name = "Updated Name"
    obj.id = 456
    obj.display()`;
}

/**
 * Generates JavaScript class code
 */
function generateJavaScriptClass(className) {
    return `/**
 * ${className} class with basic functionality
 */
class ${className} {
    /**
     * Create a ${className} instance
     * @param {string} name - The name attribute
     * @param {number} id - The ID attribute
     */
    constructor(name = "", id = 0) {
        this._name = name;
        this._id = id;
    }

    /**
     * Get the name attribute
     * @returns {string} The name
     */
    get name() {
        return this._name;
    }

    /**
     * Set the name attribute
     * @param {string} value - The new name
     */
    set name(value) {
        if (typeof value !== 'string') {
            throw new TypeError('Name must be a string');
        }
        this._name = value;
    }

    /**
     * Get the ID attribute
     * @returns {number} The ID
     */
    get id() {
        return this._id;
    }

    /**
     * Set the ID attribute
     * @param {number} value - The new ID
     */
    set id(value) {
        if (typeof value !== 'number') {
            throw new TypeError('ID must be a number');
        }
        this._id = value;
    }

    /**
     * String representation of the object
     * @returns {string} String representation
     */
    toString() {
        return \`${className}{name='\${this._name}', id=\${this._id}}\`;
    }

    /**
     * Check equality with another object
     * @param {*} other - Object to compare with
     * @returns {boolean} True if equal
     */
    equals(other) {
        if (!(other instanceof ${className})) {
            return false;
        }
        return this._name === other._name && this._id === other._id;
    }

    /**
     * Display the object information
     */
    display() {
        console.log(this.toString());
    }
}

// Example usage
// const obj = new ${className}("Example", 123);
// obj.display();

// Export for use as module
module.exports = { ${className} };`;
}

/**
 * Generates Kotlin class code
 */
function generateKotlinClass(className) {
    return `/**
 * ${className} class with basic functionality
 */
class ${className}(
    var name: String = "",
    var id: Int = 0
) {
    /**
     * Secondary constructor
     */
    constructor(name: String) : this(name, 0)

    /**
     * Copy constructor equivalent
     */
    constructor(other: ${className}) : this(other.name, other.id)

    /**
     * Override toString method
     */
    override fun toString(): String {
        return "${className}(name='$name', id=$id)"
    }

    /**
     * Override equals method
     */
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ${className}) return false
        return name == other.name && id == other.id
    }

    /**
     * Override hashCode method
     */
    override fun hashCode(): Int {
        return name.hashCode() * 31 + id
    }

    /**
     * Display method
     */
    fun display() {
        println(toString())
    }

    /**
     * Companion object for static methods
     */
    companion object {
        /**
         * Create instance with default values
         */
        fun create(): ${className} {
            return ${className}("Default", 0)
        }
    }
}

// Example usage
// val obj = ${className}("Example", 123)
// obj.display()

// Test companion object
// val defaultObj = ${className}.create()`;
}

/**
 * Generates PHP class code
 */
function generatePhpClass(className) {
    return `<?php

/**
 * ${className} class with basic functionality
 */
class ${className}
{
    /**
     * @var string
     */
    private $name;

    /**
     * @var int
     */
    private $id;

    /**
     * Constructor
     * @param string $name
     * @param int $id
     */
    public function __construct($name = "", $id = 0)
    {
        $this->name = $name;
        $this->id = $id;
    }

    /**
     * Get the name
     * @return string
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * Set the name
     * @param string $name
     */
    public function setName($name)
    {
        $this->name = $name;
    }

    /**
     * Get the ID
     * @return int
     */
    public function getId()
    {
        return $this->id;
    }

    /**
     * Set the ID
     * @param int $id
     */
    public function setId($id)
    {
        $this->id = $id;
    }

    /**
     * Convert to string
     * @return string
     */
    public function __toString()
    {
        return "${className}{name='" . $this->name . "', id=" . $this->id . "}";
    }

    /**
     * Check equality
     * @param mixed $other
     * @return bool
     */
    public function equals($other)
    {
        if (!$other instanceof ${className}) {
            return false;
        }
        return $this->name === $other->name && $this->id === $other->id;
    }

    /**
     * Display the object
     */
    public function display()
    {
        echo $this->__toString() . PHP_EOL;
    }

    /**
     * Create instance with default values
     * @return ${className}
     */
    public static function create()
    {
        return new self("Default", 0);
    }
}

// Example usage
// $obj = new ${className}("Example", 123);
// $obj->display();`;
}

module.exports = {
    generateClass
};