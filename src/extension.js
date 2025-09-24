/*****************************************************************************/
/*                                                                           */
/*  extension.js                                         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 23 2025 11:39 st93642                      TT    SSSSSSS II */
/*  Updated: Sep 24 2025 03:14 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

const vscode = require('vscode');
const { execSync } = require('child_process');
const path = require('path');

function activate(context) {
    console.log('TSI Header extension is now active!');

    // Helper function to show configuration instructions
    function showConfigurationInstructions(type) {
        const message = type === 'username' 
            ? '🔧 TSI Header Setup: Please configure your username to get started!\n\n' +
              '📝 Choose one option:\n' +
              '• VS Code Settings: Search "tsiheader.username"\n' +
              '• Git config: git config --global user.name "YourUsername"\n' +
              '• Environment: Set TSI_USERNAME variable'
            : '🔧 TSI Header Setup: Please configure your email to get started!\n\n' +
              '📝 Choose one option:\n' +
              '• VS Code Settings: Search "tsiheader.email"\n' +
              '• Git config: git config --global user.email "your.email@example.com"\n' +
              '• Environment: Set TSI_EMAIL variable';
        
        vscode.window.showInformationMessage(message, 'Open Settings', 'Git Config Help')
            .then(selection => {
                if (selection === 'Open Settings') {
                    vscode.commands.executeCommand('workbench.action.openSettings', `@ext:st93642.tsi-header`);
                } else if (selection === 'Git Config Help') {
                    vscode.env.openExternal(vscode.Uri.parse('https://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup'));
                }
            });
    }

    // Register insert header command
    const insertHeaderCommand = vscode.commands.registerCommand('tsiheader.insertHeader', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        try {
            const document = editor.document;
            const languageId = document.languageId;
            const fileName = document.fileName;
            
            // Get configuration
            const config = vscode.workspace.getConfiguration('tsiheader');
            const username = config.get('username');
            const email = config.get('email');
            
            // Check for credentials and show helpful setup instructions if missing
            const hasUsername = username && username.trim() !== '';
            const hasEmail = email && email.trim() !== '';
            
            // Check git config as fallback
            let gitUsername = '';
            let gitEmail = '';
            try {
                gitUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
            } catch (e) { /* ignore */ }
            try {
                gitEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
            } catch (e) { /* ignore */ }
            
            const hasAnyUsername = hasUsername || gitUsername;
            const hasAnyEmail = hasEmail || gitEmail;
            
            // Show configuration instructions if credentials are missing
            if (!hasAnyUsername) {
                showConfigurationInstructions('username');
                return;
            }
            if (!hasAnyEmail) {
                showConfigurationInstructions('email');
                return;
            }
            
            // Get Ruby CLI path
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
            
            // Set environment variables for configuration (only if they exist and are not empty)
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
            const command = `ruby "${cliPath}" insert "${languageId}" "${fileName}"`;
            console.log('Executing command:', command);
            const result = execSync(command, { encoding: 'utf8', cwd: extensionPath, env: env });
            console.log('CLI result:', result);
            const response = JSON.parse(result);
            
            if (response.success) {
                // Insert header at the beginning of the document
                const firstLine = document.lineAt(0);
                const insertPosition = new vscode.Position(0, 0);
                
                await editor.edit(editBuilder => {
                    editBuilder.insert(insertPosition, response.header + '\n');
                });
                
                vscode.window.showInformationMessage('TSI Header inserted successfully!');
            } else {
                vscode.window.showErrorMessage(`Failed to insert header: ${response.message}`);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error: ${error.message}`);
        }
    });

    // Register update header command
    const updateHeaderCommand = vscode.commands.registerCommand('tsiheader.updateHeader', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        try {
            const document = editor.document;
            const languageId = document.languageId;
            const fileName = document.fileName;
            
            // Get configuration
            const config = vscode.workspace.getConfiguration('tsiheader');
            const username = config.get('username');
            const email = config.get('email');
            
            // Check for credentials and show helpful setup instructions if missing
            const hasUsername = username && username.trim() !== '';
            const hasEmail = email && email.trim() !== '';
            
            // Check git config as fallback
            let gitUsername = '';
            let gitEmail = '';
            try {
                gitUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
            } catch (e) { /* ignore */ }
            try {
                gitEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
            } catch (e) { /* ignore */ }
            
            const hasAnyUsername = hasUsername || gitUsername;
            const hasAnyEmail = hasEmail || gitEmail;
            
            // Show configuration instructions if credentials are missing
            if (!hasAnyUsername) {
                showConfigurationInstructions('username');
                return;
            }
            if (!hasAnyEmail) {
                showConfigurationInstructions('email');
                return;
            }
            
            // Get Ruby CLI path
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
            
            // Set environment variables for configuration (only if they exist and are not empty)
            const env = {
                ...process.env
            };
            
            if (username && username.trim() !== '') {
                env.TSI_USERNAME = username;
            }
            if (email && email.trim() !== '') {
                env.TSI_EMAIL = email;
            }
            
            // Execute Ruby CLI for update
            const command = `ruby "${cliPath}" update "${languageId}" "${fileName}"`;
            console.log('Executing update command:', command);
            const result = execSync(command, { encoding: 'utf8', cwd: extensionPath, env: env });
            console.log('Update CLI result:', result);
            const response = JSON.parse(result);
            
            if (response.success) {
                vscode.window.showInformationMessage('TSI Header updated successfully!');
            } else {
                vscode.window.showErrorMessage(`Failed to update header: ${response.message}`);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error: ${error.message}`);
        }
    });

    // Auto-save functionality: Listen for file save events
    const onSaveListener = vscode.workspace.onDidSaveTextDocument(async (document) => {
        // Check if auto-update is enabled
        const config = vscode.workspace.getConfiguration('tsiheader');
        const autoUpdate = config.get('autoUpdate');
        
        if (!autoUpdate) {
            return; // Auto-update is disabled
        }

        // Check if file has a TSI header to update
        const text = document.getText();
        const lines = text.split('\n');
        
        // Look for TSI header pattern in first few lines
        let hasHeader = false;
        for (let i = 0; i < Math.min(15, lines.length); i++) {
            if (lines[i].includes('Transport and Telecommunication Institute')) {
                hasHeader = true;
                break;
            }
        }
        
        if (!hasHeader) {
            return; // No TSI header found, nothing to update
        }

        // Check for credentials (same logic as manual update)
        const username = config.get('username');
        const email = config.get('email');
        
        const hasUsername = username && username.trim() !== '';
        const hasEmail = email && email.trim() !== '';
        
        // Check git config as fallback
        let gitUsername = '';
        let gitEmail = '';
        try {
            gitUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
        } catch (e) { /* ignore */ }
        try {
            gitEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
        } catch (e) { /* ignore */ }
        
        const hasAnyUsername = hasUsername || gitUsername;
        const hasAnyEmail = hasEmail || gitEmail;
        
        if (!hasAnyUsername || !hasAnyEmail) {
            return; // Skip auto-update if credentials are missing
        }

        // Perform the auto-update
        try {
            const languageId = document.languageId;
            const fileName = document.fileName;
            
            // Get Ruby CLI path
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
            
            // Set environment variables for configuration
            const env = {
                ...process.env
            };
            
            if (hasUsername) {
                env.TSI_USERNAME = username;
            }
            if (hasEmail) {
                env.TSI_EMAIL = email;
            }
            
            // Execute Ruby CLI for auto-update
            const command = `ruby "${cliPath}" update "${languageId}" "${fileName}"`;
            const result = execSync(command, { encoding: 'utf8', cwd: extensionPath, env: env });
            const response = JSON.parse(result);
            
            if (response.success) {
                // Silent success for auto-update - no notification needed
                console.log('TSI Header auto-updated successfully');
            }
        } catch (error) {
            // Silent failure for auto-update - no error notifications for background operations
            console.log('TSI Header auto-update failed:', error.message);
        }
    });

    // Register add class command
    const addClassCommand = vscode.commands.registerCommand('tsiheader.addClass', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        const document = editor.document;
        const languageId = document.languageId;
        const fileName = document.fileName;

        // Get credentials for template
        const config = vscode.workspace.getConfiguration('tsiheader');
        const username = config.get('username');
        const email = config.get('email');

        let finalUsername = username || 'unknown';
        let finalEmail = email || 'unknown@students.tsi.lv';

        // Check git config as fallback
        try {
            if (!username) {
                finalUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
            }
            if (!email) {
                finalEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
            }
        } catch (e) {
            // Git config not available, use defaults
        }

        const extensionPath = context.extensionPath;
        const now = new Date();
        // Format date to match Ruby header generator format: "Sep 24 2025 02:32"
        const dateStr = now.toLocaleDateString('en-US', { month: 'short', day: '2-digit', year: 'numeric' }).replace(',', '') + 
                       ' ' + now.toTimeString().slice(0, 5);

        try {
            // Generate proper TSI header using Ruby CLI
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
            
            // Set environment variables for configuration
            const env = {
                ...process.env
            };
            
            if (finalUsername && finalUsername.trim() !== '') {
                env.TSI_USERNAME = finalUsername;
            }
            if (finalEmail && finalEmail.trim() !== '') {
                env.TSI_EMAIL = finalEmail;
            }
            
            // Generate header using Ruby CLI
            const headerCommand = `ruby "${cliPath}" insert "${languageId}" "${fileName}"`;
            const headerResult = execSync(headerCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
            const headerResponse = JSON.parse(headerResult);
            
            if (!headerResponse.success) {
                vscode.window.showErrorMessage(`Failed to generate header: ${headerResponse.message}`);
                return;
            }
            
            let fullContent = headerResponse.header;
            
            // Ask for class name
            const className = await vscode.window.showInputBox({
                prompt: 'Enter class name',
                placeHolder: 'MyClass'
            });

            if (!className) {
                return; // User cancelled
            }

            // Generate class code based on language
            if (languageId === 'java') {
                const packageName = await vscode.window.showInputBox({
                    prompt: 'Enter package name',
                    placeHolder: 'com.example'
                }) || 'com.example';
                
                fullContent += `\npackage ${packageName};\n\npublic class ${className} {\n    // Class attributes\n    private String name;\n    private int id;\n\n    // Default constructor\n    public ${className}() {\n        this.name = "";\n        this.id = 0;\n    }\n\n    // Parameterized constructor\n    public ${className}(String name, int id) {\n        this.name = name;\n        this.id = id;\n    }\n\n    // Getter for name\n    public String getName() {\n        return name;\n    }\n\n    // Setter for name\n    public void setName(String name) {\n        this.name = name;\n    }\n\n    // Getter for id\n    public int getId() {\n        return id;\n    }\n\n    // Setter for id\n    public void setId(int id) {\n        this.id = id;\n    }\n\n    // Override toString method\n    @Override\n    public String toString() {\n        return "${className}{" +\n                "name='" + name + '\\'' +\n                ", id=" + id +\n                '}';\n    }\n\n    // Override equals method\n    @Override\n    public boolean equals(Object obj) {\n        if (this == obj) return true;\n        if (obj == null || getClass() != obj.getClass()) return false;\n        ${className} that = (${className}) obj;\n        return id == that.id && name.equals(that.name);\n    }\n\n    // Override hashCode method\n    @Override\n    public int hashCode() {\n        return name.hashCode() * 31 + id;\n    }\n}\n`;
                
            } else if (languageId === 'cpp' || languageId === 'c++') {
                // For C++, create separate .hpp and .cpp files
                const headerGuard = `${className.toUpperCase()}_HPP`;
                const currentDir = path.dirname(fileName);
                const headerFileName = `${className}.hpp`;
                const implFileName = `${className}.cpp`;
                const headerFilePath = path.join(currentDir, headerFileName);
                const implFilePath = path.join(currentDir, implFileName);

                // Check if files already exist
                try {
                    await vscode.workspace.fs.stat(vscode.Uri.file(headerFilePath));
                    const overwrite = await vscode.window.showWarningMessage(
                        `File '${headerFileName}' already exists. Overwrite?`,
                        'Yes', 'No'
                    );
                    if (overwrite !== 'Yes') {
                        return;
                    }
                } catch (e) {
                    // File doesn't exist, continue
                }
                
                try {
                    await vscode.workspace.fs.stat(vscode.Uri.file(implFilePath));
                    const overwrite = await vscode.window.showWarningMessage(
                        `File '${implFileName}' already exists. Overwrite?`,
                        'Yes', 'No'
                    );
                    if (overwrite !== 'Yes') {
                        return;
                    }
                } catch (e) {
                    // File doesn't exist, continue
                }

                // Generate header file content
                const headerCommand = `ruby "${cliPath}" insert "${languageId}" "${headerFilePath}"`;
                const headerResult = execSync(headerCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
                const headerResponse = JSON.parse(headerResult);
                
                if (!headerResponse.success) {
                    vscode.window.showErrorMessage(`Failed to generate header for ${headerFileName}: ${headerResponse.message}`);
                    return;
                }

                let headerContent = headerResponse.header;
                headerContent += `\n#ifndef ${headerGuard}\n#define ${headerGuard}\n\n#include <string>\n#include <iostream>\n\nclass ${className} {\nprivate:\n    std::string name;\n    int id;\n\npublic:\n    // Default constructor\n    ${className}();\n\n    // Parameterized constructor\n    ${className}(const std::string& name, int id);\n\n    // Copy constructor\n    ${className}(const ${className}& other);\n\n    // Destructor\n    ~${className}();\n\n    // Assignment operator\n    ${className}& operator=(const ${className}& other);\n\n    // Getters\n    std::string getName() const;\n    int getId() const;\n\n    // Setters\n    void setName(const std::string& name);\n    void setId(int id);\n\n    // Utility methods\n    void display() const;\n    bool equals(const ${className}& other) const;\n};\n\n#endif // ${headerGuard}\n`;

                // Generate implementation file content
                const implCommand = `ruby "${cliPath}" insert "${languageId}" "${implFilePath}"`;
                const implResult = execSync(implCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
                const implResponse = JSON.parse(implResult);
                
                if (!implResponse.success) {
                    vscode.window.showErrorMessage(`Failed to generate header for ${implFileName}: ${implResponse.message}`);
                    return;
                }

                let implContent = implResponse.header;
                implContent += `\n#include "${headerFileName}"\n\n// Default constructor\n${className}::${className}() : name(""), id(0) {}\n\n// Parameterized constructor\n${className}::${className}(const std::string& name, int id) : name(name), id(id) {}\n\n// Copy constructor\n${className}::${className}(const ${className}& other) : name(other.name), id(other.id) {}\n\n// Destructor\n${className}::~${className}() {}\n\n// Assignment operator\n${className}& ${className}::operator=(const ${className}& other) {\n    if (this != &other) {\n        name = other.name;\n        id = other.id;\n    }\n    return *this;\n}\n\n// Getters\nstd::string ${className}::getName() const {\n    return name;\n}\n\nint ${className}::getId() const {\n    return id;\n}\n\n// Setters\nvoid ${className}::setName(const std::string& name) {\n    this->name = name;\n}\n\nvoid ${className}::setId(int id) {\n    this->id = id;\n}\n\n// Utility methods\nvoid ${className}::display() const {\n    std::cout << "${className}{name='" << name << "', id=" << id << "}" << std::endl;\n}\n\nbool ${className}::equals(const ${className}& other) const {\n    return name == other.name && id == other.id;\n}\n`;

                // Write both files
                await vscode.workspace.fs.writeFile(vscode.Uri.file(headerFilePath), Buffer.from(headerContent));
                await vscode.workspace.fs.writeFile(vscode.Uri.file(implFilePath), Buffer.from(implContent));

                // Open the header file in editor
                const document = await vscode.workspace.openTextDocument(headerFilePath);
                await vscode.window.showTextDocument(document);

                vscode.window.showInformationMessage(`TSI Header: Created C++ class "${className}" (${headerFileName} + ${implFileName})`);
                return; // Don't continue with the insert logic below
                
            } else if (languageId === 'csharp') {
                fullContent += `\nusing System;\n\nnamespace ${className}Namespace\n{\n    public class ${className}\n    {\n        // Private fields\n        private string name;\n        private int id;\n\n        // Default constructor\n        public ${className}()\n        {\n            this.name = "";\n            this.id = 0;\n        }\n\n        // Parameterized constructor\n        public ${className}(string name, int id)\n        {\n            this.name = name;\n            this.id = id;\n        }\n\n        // Copy constructor\n        public ${className}(${className} other)\n        {\n            this.name = other.name;\n            this.id = other.id;\n        }\n\n        // Properties\n        public string Name\n        {\n            get { return name; }\n            set { name = value; }\n        }\n\n        public int Id\n        {\n            get { return id; }\n            set { id = value; }\n        }\n\n        // Override ToString method\n        public override string ToString()\n        {\n            return $"${className}{{Name='{name}', Id={id}}}";\n        }\n\n        // Override Equals method\n        public override bool Equals(object obj)\n        {\n            if (obj == null || GetType() != obj.GetType())\n            {\n                return false;\n            }\n            ${className} other = (${className})obj;\n            return name == other.name && id == other.id;\n        }\n\n        // Override GetHashCode method\n        public override int GetHashCode()\n        {\n            return name.GetHashCode() * 31 + id;\n        }\n\n        // Display method\n        public void Display()\n        {\n            Console.WriteLine(ToString());\n        }\n    }\n}\n`;
                
            } else if (languageId === 'python') {
                fullContent += `\nclass ${className}:\n    """${className} class with basic functionality."""\n\n    def __init__(self, name="", id_num=0):\n        """Initialize ${className} instance.\n\n        Args:\n            name (str): The name attribute\n            id_num (int): The ID attribute\n        """\n        self._name = name\n        self._id = id_num\n\n    @property\n    def name(self):\n        """Get the name attribute."""\n        return self._name\n\n    @name.setter\n    def name(self, value):\n        """Set the name attribute."""\n        if not isinstance(value, str):\n            raise TypeError("Name must be a string")\n        self._name = value\n\n    @property\n    def id(self):\n        """Get the ID attribute."""\n        return self._id\n\n    @id.setter\n    def id(self, value):\n        """Set the ID attribute."""\n        if not isinstance(value, int):\n            raise TypeError("ID must be an integer")\n        self._id = value\n\n    def __str__(self):\n        """String representation of the object."""\n        return f"${className}{{name='{self._name}', id={self._id}}}"\n\n    def __repr__(self):\n        """Official string representation of the object."""\n        return f"${className}('{self._name}', {self._id})"\n\n    def __eq__(self, other):\n        """Check equality with another object."""\n        if not isinstance(other, ${className}):\n            return NotImplemented\n        return self._name == other._name and self._id == other._id\n\n    def __hash__(self):\n        """Hash function for the object."""\n        return hash((self._name, self._id))\n\n    def display(self):\n        """Display the object information."""\n        print(f"${className}{{name='{self._name}', id={self._id}}}")\n\n\n# Example usage\nif __name__ == "__main__":\n    # Create an instance\n    obj = ${className}("Example", 123)\n    obj.display()\n\n    # Test property setters\n    obj.name = "Updated Name"\n    obj.id = 456\n    obj.display()\n`;
                
            } else if (languageId === 'javascript') {
                fullContent += `\n/**\n * ${className} class with basic functionality\n */\nclass ${className} {\n    /**\n     * Create a ${className} instance\n     * @param {string} name - The name attribute\n     * @param {number} id - The ID attribute\n     */\n    constructor(name = "", id = 0) {\n        this._name = name;\n        this._id = id;\n    }\n\n    /**\n     * Get the name attribute\n     * @returns {string} The name\n     */\n    get name() {\n        return this._name;\n    }\n\n    /**\n     * Set the name attribute\n     * @param {string} value - The new name\n     */\n    set name(value) {\n        if (typeof value !== 'string') {\n            throw new TypeError('Name must be a string');\n        }\n        this._name = value;\n    }\n\n    /**\n     * Get the ID attribute\n     * @returns {number} The ID\n     */\n    get id() {\n        return this._id;\n    }\n\n    /**\n     * Set the ID attribute\n     * @param {number} value - The new ID\n     */\n    set id(value) {\n        if (typeof value !== 'number') {\n            throw new TypeError('ID must be a number');\n        }\n        this._id = value;\n    }\n\n    /**\n     * String representation of the object\n     * @returns {string} String representation\n     */\n    toString() {\n        return \`${className}{name='\${this._name}', id=\${this._id}}\`;\n    }\n\n    /**\n     * Check equality with another object\n     * @param {*} other - Object to compare with\n     * @returns {boolean} True if equal\n     */\n    equals(other) {\n        if (!(other instanceof ${className})) {\n            return false;\n        }\n        return this._name === other._name && this._id === other._id;\n    }\n\n    /**\n     * Display the object information\n     */\n    display() {\n        console.log(this.toString());\n    }\n}\n\n// Example usage\n// const obj = new ${className}("Example", 123);\n// obj.display();\n\n// Export for use as module\nmodule.exports = { ${className} };\n`;
                
            } else if (languageId === 'kotlin') {
                fullContent += `\n/**\n * ${className} class with basic functionality\n */\nclass ${className}(\n    var name: String = "",\n    var id: Int = 0\n) {\n    /**\n     * Secondary constructor\n     */\n    constructor(name: String) : this(name, 0)\n\n    /**\n     * Copy constructor equivalent\n     */\n    constructor(other: ${className}) : this(other.name, other.id)\n\n    /**\n     * Override toString method\n     */\n    override fun toString(): String {\n        return "${className}(name='\${name}', id=\${id})"\n    }\n\n    /**\n     * Override equals method\n     */\n    override fun equals(other: Any?): Boolean {\n        if (this === other) return true\n        if (other !is ${className}) return false\n        return name == other.name && id == other.id\n    }\n\n    /**\n     * Override hashCode method\n     */\n    override fun hashCode(): Int {\n        return name.hashCode() * 31 + id\n    }\n\n    /**\n     * Display method\n     */\n    fun display() {\n        println(toString())\n    }\n\n    /**\n     * Companion object for static methods\n     */\n    companion object {\n        /**\n         * Create instance with default values\n         */\n        fun create(): ${className} {\n            return ${className}("Default", 0)\n        }\n    }\n}\n\n// Example usage\n// val obj = ${className}("Example", 123)\n// obj.display()\n\n// Test companion object\n// val defaultObj = ${className}.create()\n`;
                
            } else if (languageId === 'plaintext') {
                // For plaintext files, assume they might be Kotlin (.kt files detected as plaintext)
                // Try to detect based on file extension
                const fileExtension = fileName.split('.').pop().toLowerCase();
                if (fileExtension === 'kt') {
                    // Treat as Kotlin
                    fullContent += `\n/**\n * ${className} class with basic functionality\n */\nclass ${className}(\n    var name: String = "",\n    var id: Int = 0\n) {\n    /**\n     * Secondary constructor\n     */\n    constructor(name: String) : this(name, 0)\n\n    /**\n     * Copy constructor equivalent\n     */\n    constructor(other: ${className}) : this(other.name, other.id)\n\n    /**\n     * Override toString method\n     */\n    override fun toString(): String {\n        return "${className}(name='\${name}', id=\${id})"\n    }\n\n    /**\n     * Override equals method\n     */\n    override fun equals(other: Any?): Boolean {\n        if (this === other) return true\n        if (other !is ${className}) return false\n        return name == other.name && id == other.id\n    }\n\n    /**\n     * Override hashCode method\n     */\n    override fun hashCode(): Int {\n        return name.hashCode() * 31 + id\n    }\n\n    /**\n     * Display method\n     */\n    fun display() {\n        println(toString())\n    }\n\n    /**\n     * Companion object for static methods\n     */\n    companion object {\n        /**\n         * Create instance with default values\n         */\n        fun create(): ${className} {\n            return ${className}("Default", 0)\n        }\n    }\n}\n\n// Example usage\n// val obj = ${className}("Example", 123)\n// obj.display()\n\n// Test companion object\n// val defaultObj = ${className}.create()\n`;
                } else {
                    vscode.window.showErrorMessage(`Class generation not supported for plaintext files with extension: ${fileExtension}`);
                    return;
                }
                
            } else {
                vscode.window.showErrorMessage(`Class generation not supported for language: ${languageId}`);
                return;
            }

            // Insert the content at cursor position or end of file
            const position = editor.selection.isEmpty ? editor.selection.active : editor.selection.end;
            await editor.edit(editBuilder => {
                editBuilder.insert(position, fullContent);
            });

            vscode.window.showInformationMessage(`TSI Header: Added ${languageId} class "${className}" to current file`);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to add class: ${error.message}`);
        }
    });

    // Register add code base command
    const addCodeBaseCommand = vscode.commands.registerCommand('tsiheader.addCodeBase', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor found');
            return;
        }

        const document = editor.document;
        const languageId = document.languageId;
        const fileName = document.fileName;

        // Get credentials for template
        const config = vscode.workspace.getConfiguration('tsiheader');
        const username = config.get('username');
        const email = config.get('email');

        let finalUsername = username || 'unknown';
        let finalEmail = email || 'unknown@students.tsi.lv';

        // Check git config as fallback
        try {
            if (!username) {
                finalUsername = execSync('git config --global user.name', { encoding: 'utf8' }).trim();
            }
            if (!email) {
                finalEmail = execSync('git config --global user.email', { encoding: 'utf8' }).trim();
            }
        } catch (e) {
            // Git config not available, use defaults
        }

        const extensionPath = context.extensionPath;
        const now = new Date();
        // Format date to match Ruby header generator format: "Sep 24 2025 02:32"
        const dateStr = now.toLocaleDateString('en-US', { month: 'short', day: '2-digit', year: 'numeric' }).replace(',', '') + 
                       ' ' + now.toTimeString().slice(0, 5);

        try {
            // Generate proper TSI header using Ruby CLI
            const extensionPath = context.extensionPath;
            const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
            
            // Set environment variables for configuration
            const env = {
                ...process.env
            };
            
            if (finalUsername && finalUsername.trim() !== '') {
                env.TSI_USERNAME = finalUsername;
            }
            if (finalEmail && finalEmail.trim() !== '') {
                env.TSI_EMAIL = finalEmail;
            }
            
            // Generate header using Ruby CLI
            const headerCommand = `ruby "${cliPath}" insert "${languageId}" "${fileName}"`;
            const headerResult = execSync(headerCommand, { encoding: 'utf8', cwd: extensionPath, env: env });
            const headerResponse = JSON.parse(headerResult);
            
            if (!headerResponse.success) {
                vscode.window.showErrorMessage(`Failed to generate header: ${headerResponse.message}`);
                return;
            }
            
            let fullContent = headerResponse.header;
            
            // Generate code structure based on language
            if (languageId === 'c') {
                fullContent += `\n#include <stdio.h>\n\nint main(int argc, char *argv[]) {\n    printf("Hello, World!\\n");\n    return 0;\n}\n`;
            } else if (languageId === 'csharp') {
                fullContent += `\nusing System;\n\nclass Program\n{\n    static void Main(string[] args)\n    {\n        Console.WriteLine("Hello, World!");\n        Console.WriteLine("This is a basic C# program.");\n    }\n}\n`;
            } else if (languageId === 'python') {
                fullContent += `\ndef main():\n    """Main function - entry point of the program."""\n    print("Hello, World!")\n    print("This is a basic Python script.")\n\n\nif __name__ == "__main__":\n    main()\n`;
            } else if (languageId === 'javascript') {
                fullContent += `\n/**\n * Main function - entry point of the program\n */\nfunction main() {\n    console.log('Hello, World!');\n    console.log('This is a basic JavaScript script.');\n}\n\n// Execute main function\nmain();\n\n// Export for use as module\nmodule.exports = { main };\n`;
            } else if (languageId === 'kotlin') {
                fullContent += `\n/**\n * Main function - entry point of the program\n */\nfun main(args: Array<String>) {\n    println("Hello, World!")\n    println("This is a basic Kotlin program.")\n}\n`;
            } else if (languageId === 'plaintext') {
                // For plaintext files, assume they might be Kotlin (.kt files detected as plaintext)
                const fileExtension = fileName.split('.').pop().toLowerCase();
                if (fileExtension === 'kt') {
                    // Treat as Kotlin
                    fullContent += `\n/**\n * Main function - entry point of the program\n */\nfun main(args: Array<String>) {\n    println("Hello, World!")\n    println("This is a basic Kotlin program.")\n}\n`;
                } else {
                    // Default to C for other plaintext files
                    fullContent += `\n#include <stdio.h>\n\nint main(int argc, char *argv[]) {\n    printf("Hello, World!\\n");\n    return 0;\n}\n`;
                }
            } else {
                // Default to C for unsupported languages
                fullContent += `\n#include <stdio.h>\n\nint main(int argc, char *argv[]) {\n    printf("Hello, World!\\n");\n    return 0;\n}\n`;
            }

            // Insert the content at cursor position or end of file
            const position = editor.selection.isEmpty ? editor.selection.active : editor.selection.end;
            await editor.edit(editBuilder => {
                editBuilder.insert(position, fullContent);
            });

            vscode.window.showInformationMessage(`TSI Header: Added ${languageId} code base to current file`);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to add code base: ${error.message}`);
        }
    });

    context.subscriptions.push(insertHeaderCommand);
    context.subscriptions.push(updateHeaderCommand);
    context.subscriptions.push(addClassCommand);
    context.subscriptions.push(addCodeBaseCommand);
    context.subscriptions.push(onSaveListener);
}

function deactivate() {}

module.exports = {
    activate,
    deactivate
};