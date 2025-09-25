/**
 * Rust Project Creator
 * Creates Rust-specific project files and structure
 */

const { generateTSIHeaderContent } = require('../headerUtils');

/**
 * Create Rust-specific files
 */
async function createRustFiles(vscode, projectName, projectUri) {
    await createCargoToml(vscode, projectName, projectUri);
    await createLibRs(vscode, projectName, projectUri);
    await createBaseStructRs(vscode, projectName, projectUri);
}

/**
 * Create Cargo.toml file for Rust projects
 */
async function createCargoToml(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'Cargo.toml');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('Cargo.toml', vscode);

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
async function createLibRs(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', 'lib.rs');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('lib.rs', vscode);

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
async function createBaseStructRs(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', 'base_struct.rs');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('base_struct.rs', vscode);

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

module.exports = {
    createRustFiles
};