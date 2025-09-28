/**
 * C++ Project Creator
 * Creates C++-specific project files and structure
 */

const { generateTSIHeaderContent } = require('../headerUtils');

/**
 * Create C++-specific files
 */
async function createCppFiles(vscode, projectName, projectUri) {
    await createBaseClassHeader(vscode, projectName, projectUri);
    await createBaseClassSource(vscode, projectName, projectUri);
}

/**
 * Create BaseClass.hpp file for C++ projects
 */
async function createBaseClassHeader(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', 'BaseClass.hpp');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('BaseClass.hpp', vscode);

    const content = `${headerContent}

#ifndef BASECLASS_HPP
#define BASECLASS_HPP

#include <string>
#include <iostream>
#include <ctime>

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
 * - String conversion and comparison operators
 */
class BaseClass {
protected:
    std::string name;
    int id;
    std::time_t createdAt;

    // Static member for ID generation
    static int nextId;

    /**
     * Initialize common object properties
     */
    virtual void initialize();

public:
    /**
     * Default constructor
     */
    BaseClass();

    /**
     * Constructor with name
     */
    BaseClass(const std::string& name);

    /**
     * Constructor with name and ID
     */
    BaseClass(const std::string& name, int id);

    /**
     * Copy constructor
     */
    BaseClass(const BaseClass& other);

    /**
     * Assignment operator
     */
    BaseClass& operator=(const BaseClass& other);

    /**
     * Virtual destructor
     */
    virtual ~BaseClass();

    /**
     * Get the object name
     */
    const std::string& getName() const;

    /**
     * Get the object ID
     */
    int getId() const;

    /**
     * Get the next available ID
     */
    static int getNextId();

    /**
     * Set the object name
     */
    void setName(const std::string& newName);

    /**
     * Display object information
     */
    virtual void display() const;

    /**
     * Convert object to string
     */
    virtual std::string toString() const;

    /**
     * Check if object is valid
     */
    virtual bool isValid() const;

    /**
     * Get class name (for polymorphism)
     */
    virtual std::string getClassName() const;

    /**
     * Equality operator
     */
    bool operator==(const BaseClass& other) const;

    /**
     * Inequality operator
     */
    bool operator!=(const BaseClass& other) const;

    /**
     * Output stream operator
     */
    friend std::ostream& operator<<(std::ostream& os, const BaseClass& obj);
};

#endif // BASECLASS_HPP
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create BaseClass.cpp file for C++ projects
 */
async function createBaseClassSource(vscode, projectName, projectUri) {
    const fileName = 'BaseClass.cpp';
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', fileName);

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent(fileName, vscode);

    const content = `${headerContent}

#include "BaseClass.hpp"
#include <iostream>
#include <iomanip>
#include <ctime>

// Initialize static member
int BaseClass::nextId = 1;

/**
 * Default constructor
 */
BaseClass::BaseClass() : BaseClass("DefaultObject") {
}

/**
 * Constructor with name
 */
BaseClass::BaseClass(const std::string& name) : BaseClass(name, nextId++) {
}

/**
 * Constructor with name and ID
 */
BaseClass::BaseClass(const std::string& name, int id)
    : name(name.empty() ? "UnnamedObject" : name), id(id) {

    // Set nextId if this ID is higher
    if (id >= nextId) {
        nextId = id + 1;
    }

    // Initialize the object
    initialize();
}

/**
 * Copy constructor
 */
BaseClass::BaseClass(const BaseClass& other)
    : name(other.name), id(nextId++) {

    initialize();
}

/**
 * Assignment operator
 */
BaseClass& BaseClass::operator=(const BaseClass& other) {
    if (this != &other) {
        name = other.name;
        // ID remains the same for assignment
    }
    return *this;
}

/**
 * Virtual destructor
 */
BaseClass::~BaseClass() {
    // Cleanup if needed
}

/**
 * Get the object name
 */
const std::string& BaseClass::getName() const {
    return name;
}

/**
 * Get the object ID
 */
int BaseClass::getId() const {
    return id;
}

/**
 * Get the next available ID
 */
int BaseClass::getNextId() {
    return nextId;
}

/**
 * Set the object name
 */
void BaseClass::setName(const std::string& newName) {
    if (!newName.empty()) {
        name = newName;
    }
}

/**
 * Initialize common object properties
 */
void BaseClass::initialize() {
    // Override in derived classes for custom initialization
}

/**
 * Display object information
 */
void BaseClass::display() const {
    std::cout << this->getClassName() << " Object:" << std::endl;
    std::cout << "  Name: " << name << std::endl;
    std::cout << "  ID: " << id << std::endl;

    // Get current time for display
    std::time_t now = std::time(nullptr);
    std::tm* localTime = std::localtime(&now);
    std::cout << "  Created: " << std::put_time(localTime, "%Y-%m-%d %H:%M:%S") << std::endl;
}

/**
 * Convert object to string
 */
std::string BaseClass::toString() const {
    return getClassName() + "(name=\"" + name + "\", id=" + std::to_string(id) + ")";
}

/**
 * Check if object is valid
 */
bool BaseClass::isValid() const {
    return !name.empty() && id > 0;
}

/**
 * Equality operator
 */
bool BaseClass::operator==(const BaseClass& other) const {
    return name == other.name && id == other.id;
}

/**
 * Inequality operator
 */
bool BaseClass::operator!=(const BaseClass& other) const {
    return !(*this == other);
}

/**
 * Get class name (for polymorphism)
 */
std::string BaseClass::getClassName() const {
    return "BaseClass";
}

/**
 * Output stream operator
 */
std::ostream& operator<<(std::ostream& os, const BaseClass& obj) {
    os << obj.toString();
    return os;
}

// Example usage
int main() {
    // Create some example objects
    BaseClass obj1("Example Object 1");
    BaseClass obj2("Example Object 2");

    std::cout << "Created objects:" << std::endl;
    obj1.display();
    std::cout << std::endl;
    obj2.display();
    std::cout << std::endl;

    std::cout << "String representations:" << std::endl;
    std::cout << "toString(): " << obj1.toString() << std::endl;
    std::cout << "operator<<: " << obj1 << std::endl;
    std::cout << "Equality: " << (obj1 == obj2 ? "equal" : "not equal") << std::endl;

    return 0;
}`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

module.exports = {
    createCppFiles
};