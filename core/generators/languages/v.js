/**
 * V Code Base Generator
 * Generates basic V code templates
 */

function generateVCodeBase() {
    return `\n// Basic V program\n\nfn main() {\n    println('Hello, World! This is a basic V program.')\n}\n\n// Alternative simple approach\n// println('Hello, World! This is a basic V program.')\n\n// Example with variables and types\n// name := 'TSI Student'\n// age := 20\n// println('Hello, \$name! You are \$age years old.')\n\n// Example function definition\n// fn greet(name string) string {\n//     return 'Hello, \$name!'\n// }\n\n// Example usage\n// println(greet('TSI Student'))\n\n// Example with arrays and loops\n// mut numbers := [1, 2, 3, 4, 5]\n// for num in numbers {\n//     println('Number: \$num')\n// }\n\n// Example with structs\n// struct Person {\n//     name string\n//     age  int\n// }\n\n// fn new_person(name string, age int) Person {\n//     return Person{\n//         name: name\n//         age: age\n//     }\n// }\n\n// fn (p Person) introduce() {\n//     println('Hello, I\\'m \$p.name and I\\'m \$p.age years old!')\n// }\n\n// mut person := new_person('TSI Student', 20)\n// person.introduce()\n`;
}

module.exports = {
    generateVCodeBase
};