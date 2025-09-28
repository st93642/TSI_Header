/**
 * Lua Code Base Generator
 * Generates boilerplate code for Lua projects
 */

function generateLuaCodeBase() {
    return `\n-- Basic Lua program\n\n-- Main function - entry point of the program\nfunction main()\n    print("Hello, World!")\n    print("This is a basic Lua script.")\nend\n\n-- Execute main function\nmain()\n\n-- Alternative direct approach\n-- print("Hello, World!")\n-- print("This is a basic Lua script.")\n\n-- Example function with parameters\nfunction greet(name)\n    return "Hello, " .. (name or "World") .. "!"\nend\n\n-- Example usage\n-- print(greet("TSI Student"))\n\n-- Example table (Lua's main data structure)\nlocal config = {\n    version = "1.0",\n    debug = true,\n    features = {"logging", "caching"}\n}\n\n-- print("Version: " .. config.version)\n`;
}

module.exports = {
    generateLuaCodeBase
};