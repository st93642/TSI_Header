/**
 * Nim Code Base Generator
 * Generates basic Nim code templates
 */

function generateNimCodeBase() {
    return `\n# Basic Nim program\n\nproc main() =\n  echo "Hello, World! This is a basic Nim program."\n\n# Execute main procedure\nmain()\n\n# Alternative simple approach\n# echo "Hello, World! This is a basic Nim program."\n\n# Example with variables and types\n# var name: string = "TSI Student"\n# let age: int = 20\n# echo "Hello, ", name, "! You are ", age, " years old."\n\n# Example procedure definition\n# proc greet(name: string): string =\n#   result = "Hello, " & name & "!"\n\n# Example usage\n# echo greet("TSI Student")\n\n# Example with arrays and sequences\n# var numbers: seq[int] = @[1, 2, 3, 4, 5]\n# for num in numbers:\n#   echo "Number: ", num\n\n# Example with objects/types\n# type\n#   Person = object\n#     name: string\n#     age: int\n\n# proc newPerson(name: string, age: int): Person =\n#   Person(name: name, age: age)\n\n# proc introduce(self: Person) =\n#   echo "Hello, I'm ", self.name, " and I'm ", self.age, " years old!"\n\n# var person = newPerson("TSI Student", 20)\n# person.introduce()\n`;
}

module.exports = {
    generateNimCodeBase
};