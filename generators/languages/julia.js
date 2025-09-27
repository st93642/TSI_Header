/**
 * Julia Code Base Generator
 * Generates boilerplate code for Julia projects
 */

function generateJuliaCodeBase() {
    return `\n# Basic Julia program\n\n\"\"\"\nMain function - entry point of the program\n\"\"\"\nfunction main()\n    println("Hello, World!")\n    println("This is a basic Julia program.")\nend\n\n# Execute main function\nmain()\n\n# Alternative direct approach\n# println("Hello, World!")\n# println("This is a basic Julia program.")\n\n# Example function with type annotations\nfunction greet(name::String)::String\n    return "Hello, \$name!"\nend\n\n# Example usage\n# println(greet("TSI Student"))\n\n# Example of list processing\n# numbers = [1, 2, 3, 4, 5]\n# doubled = [x * 2 for x in numbers]\n# println("Doubled: \$doubled")\n`;
}

module.exports = {
    generateJuliaCodeBase
};