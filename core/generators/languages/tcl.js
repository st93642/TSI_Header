/**
 * TCL Code Base Generator
 * Generates boilerplate code for TCL scripts
 */

function generateTclCodeBase() {
    return `\n# Basic TCL script\n\n# Main procedure - entry point of the program\nproc main {} {\n    puts "Hello, World!"\n    puts "This is a basic TCL script."\n}\n\n# Execute main procedure\nmain\n\n# Alternative direct approach\n# puts "Hello, World!"\n# puts "This is a basic TCL script."\n\n# Example procedure with parameters\n# proc greet {name} {\n#     if {$name eq ""} {\n#         set name "World"\n#     }\n#     return "Hello, $name!"\n# }\n\n# Example usage\n# puts [greet "TSI Student"]\n\n# Example with arrays and variables\n# set languages [list "TCL" "Python" "Ruby"]\n# array set config {\n#     version "1.0"\n#     debug 1\n#     author "TSI Student"\n# }\n\n# foreach lang $languages {\n#     puts "Language: $lang"\n# }\n\n# puts "Version: $config(version)"\n`;
}

module.exports = {
    generateTclCodeBase
};