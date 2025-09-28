/**
 * Rust Code Base Generator
 * Generates boilerplate code for Rust projects
 */

function generateRustCodeBase() {
    return `// Basic Rust program

/// Main function - entry point of the program
fn main() {
    println!("Hello, World!");
    println!("This is a basic Rust program.");

    // Create and run the application
    let mut app = TSIApplication::new();
    app.run();
}

/// TSI Application struct
struct TSIApplication {
    message: String,
    version: String,
}

impl TSIApplication {
    /// Create a new TSI Application instance
    fn new() -> Self {
        TSIApplication {
            message: "Welcome to TSI!".to_string(),
            version: "1.0".to_string(),
        }
    }

    /// Run the application
    fn run(&mut self) {
        println!("{}", self.message);
        println!("Version: {}", self.version);

        // Example with vectors
        let languages = vec!["Rust", "Go", "C++"];
        for lang in &languages {
            println!("Language: {}", lang);
        }
    }

    /// Get application info
    fn get_info(&self) -> String {
        format!("TSI Application v{}", self.version)
    }

    /// Greet method with parameter
    fn greet(&self, name: &str) -> String {
        format!("Hello, {}!", name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_application_creation() {
        let app = TSIApplication::new();
        assert_eq!(app.version, "1.0");
        assert_eq!(app.message, "Welcome to TSI!");
    }

    #[test]
    fn test_greet() {
        let app = TSIApplication::new();
        assert_eq!(app.greet("TSI"), "Hello, TSI!");
    }
}`;
}

module.exports = {
    generateRustCodeBase
};