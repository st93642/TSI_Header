/**
 * IDL Code Base Generator
 * Generates boilerplate code for IDL (Interactive Data Language) projects
 */

function generateIdlCodeBase() {
    return `\n;; Basic IDL program\n\n;; Main procedure\nPRO hello_world\n    PRINT, 'Hello, World!'\n    PRINT, 'This is a basic IDL program.'\nEND\n\n;; Execute main procedure\nhello_world\n\n;; Alternative direct approach\n;; PRINT, 'Hello, World!'\n;; PRINT, 'This is a basic IDL program.'\n\n;; Example with variables\n;; message1 = 'Hello, World!'\n;; message2 = 'This is a basic IDL program.'\n;; PRINT, message1\n;; PRINT, message2\n`;
}

module.exports = {
    generateIdlCodeBase
};