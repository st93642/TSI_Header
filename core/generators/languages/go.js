/**
 * Go Code Base Generator
 * Generates boilerplate Go code
 */

function generateGoCodeBase() {
    return `\npackage main\n\nimport "fmt"\n\n// Main function - entry point of the program\nfunc main() {\n    fmt.Println("Hello, World!")\n    fmt.Println("This is a basic Go program.")\n}\n\n// Example function with parameters\nfunc greet(name string) string {\n    return "Hello, " + name + "!"\n}\n\n// Example struct definition\ntype Person struct {\n    Name string\n    Age  int\n}\n\n// Example method\nfunc (p Person) Introduce() {\n    fmt.Printf("Hello, I'm %s and I'm %d years old!\\n", p.Name, p.Age)\n}\n\n// Example usage (uncomment to use)\n/*\nfunc main() {\n    // Basic greeting\n    fmt.Println("Hello, World!")\n    fmt.Println("This is a basic Go program.")\n\n    // Function usage\n    greeting := greet("TSI Student")\n    fmt.Println(greeting)\n\n    // Struct usage\n    person := Person{Name: "TSI Student", Age: 20}\n    person.Introduce()\n}\n*/\n`;
}

module.exports = {
    generateGoCodeBase
};