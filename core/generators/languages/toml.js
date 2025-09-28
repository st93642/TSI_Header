/**
 * TOML Language Code Base Generator
 * Generates TOML configuration file boilerplate
 */

/**
 * Generates TOML code base
 * @returns {string} TOML configuration template
 */
function generateTomlCodeBase() {
    return `\n# TOML Configuration File Example\n# This is a comprehensive TOML configuration demonstrating various features\n\n# Basic data types\ntitle = "TSI Header Configuration"\nversion = "1.0.0"\nenabled = true\nport = 8080\nmax_connections = 100\n\n# Arrays\nsupported_languages = [\n  "javascript",\n  "python",\n  "java",\n  "ruby",\n  "go",\n  "rust"\n]\n\nfile_extensions = [".js", ".py", ".java", ".rb", ".go", ".rs"]\n\n# Table\nauthor = { name = "TSI Student", email = "student@tsi.lv", year = 2025 }\n\n# Nested tables\n[database]\n  host = "localhost"\n  port = 5432\n  name = "tsi_db"\n  ssl = true\n\n  [database.credentials]\n    username = "tsi_user"\n    password = "secure_password"\n\n# Array of tables\n[[servers]]\n  name = "web-server"\n  ip = "192.168.1.100"\n  role = "frontend"\n\n[[servers]]\n  name = "api-server"\n  ip = "192.168.1.101"\n  role = "backend"\n\n[[servers]]\n  name = "db-server"\n  ip = "192.168.1.102"\n  role = "database"\n\n# Configuration sections\n[logging]\n  level = "info"\n  file = "/var/log/tsi/app.log"\n  max_size = "10MB"\n  max_files = 5\n\n[features]\n  auto_update = true\n  notifications = true\n  dark_mode = false\n\n# Environment-specific settings\n[environments.production]\n  debug = false\n  cache_enabled = true\n  workers = 4\n\n[environments.development]\n  debug = true\n  cache_enabled = false\n  workers = 1\n\n# Multi-line strings\ndescription = """\nThis is a multi-line string\nthat spans several lines.\nUseful for longer text content.\n"""\n\n# Date and time examples\ncreated_at = 2025-09-26T10:00:00Z\nupdated_at = 2025-09-26T10:30:00+02:00\n\n# Inline table\ninline_config = { theme = "dark", language = "en", timezone = "Europe/Riga" }\n\n# Comments throughout the file\n# This configuration supports:\n# - Header generation for multiple languages\n# - Code base templates\n# - Project scaffolding\n# - Quality assurance testing`;
}

module.exports = {
    generateTomlCodeBase
};