/**
 * Elm Language Code Base Generator
 * Generates Elm code base/boilerplate code
 */

/**
 * Generates Elm code base
 * @returns {string} Elm code base template
 */
function generateElmCodeBase() {
    return `\nmodule Main exposing (main)\n\n\nimport Html exposing (text)\n\n\n-- Main function - entry point of the program\nmain =\n    text "Hello, World! This is a basic Elm program."\n`;
}

module.exports = {
    generateElmCodeBase
};