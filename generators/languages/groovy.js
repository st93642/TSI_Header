/**
 * Groovy Code Base Generator
 * Generates boilerplate code for Groovy projects
 */

function generateGroovyCodeBase() {
    return `// Basic Groovy program

// Main class
class TSIApplication {
    String message
    String version

    // Constructor
    TSIApplication() {
        this.message = "Hello, World!"
        this.version = "1.0"
    }

    // Main method
    static void main(String[] args) {
        def app = new TSIApplication()
        app.run()
    }

    // Run application
    void run() {
        println message
        println "This is a basic Groovy program."
        println "Version: \${version}"

        // Example with lists
        def languages = ["Groovy", "Java", "Scala"]
        println "Languages:"
        languages.each { lang ->
            println "  \${lang}"
        }

        // Example with maps
        def config = [
            debug: true,
            port: 8080,
            features: ["logging", "caching", "gradle"]
        ]

        println "Configuration:"
        println "  Debug: \${config.debug}"
        println "  Port: \${config.port}"
    }

    // Utility methods
    String getInfo() {
        "TSI Application v\${version}"
    }

    String greet(String name) {
        "Hello, \${name}!"
    }

    // Static factory method
    static TSIApplication createDefault() {
        def app = new TSIApplication()
        app.message = "Welcome to TSI!"
        app
    }
}

// Execute main method
TSIApplication.main()

// Example usage
// def defaultApp = TSIApplication.createDefault()
// println defaultApp.getInfo()
// println defaultApp.greet("TSI Student")`;
}

module.exports = {
    generateGroovyCodeBase
};