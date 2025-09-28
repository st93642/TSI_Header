/**
 * Java Project Creator
 * Creates Java-specific project files and structure
 */

const { generateTSIHeaderContent } = require('../headerUtils');

/**
 * Create Java-specific files
 */
async function createJavaFiles(vscode, projectName, projectUri) {
    await createBaseClassJava(vscode, projectName, projectUri);
}

/**
 * Create BaseClass.java file for Java projects
 */
async function createBaseClassJava(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', 'main', 'java', 'BaseClass.java');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('BaseClass.java', vscode);

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

module.exports = {
    createJavaFiles
};