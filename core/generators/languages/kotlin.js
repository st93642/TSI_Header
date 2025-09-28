/**
 * Kotlin Code Base Generator
 * Generates boilerplate Kotlin code
 */

function generateKotlinCodeBase() {
    return `\n// Basic Kotlin program\n\n// Main function - entry point of the program\nfun main() {\n    println("Hello, World!")\n    println("This is a basic Kotlin program.")\n}\n\n// Alternative direct approach\n// println("Hello, World!")\n// println("This is a basic Kotlin program.")\n\n// Example function with parameters\nfun greet(name: String = "World"): String {\n    return "Hello, \$name!"\n}\n\n// Example usage\n// println(greet("TSI Student"))\n\n// Example class definition\nclass TSIStudent(val name: String, val program: String) {\n    fun introduce() {\n        println("Hello, I'm \$name from TSI!")\n    }\n}\n\n// Example usage\n// val student = TSIStudent("TSI Student", "Computer Science")\n// student.introduce()\n\n// Example with collections\n// val languages = listOf("Kotlin", "Java", "Python")\n// val config = mapOf(\n//     "version" to "1.0",\n//     "debug" to true,\n//     "author" to "TSI Student"\n// )\n\n// languages.forEach { lang ->\n//     println("Language: \$lang")\n// }\n\n// println("Version: \${config["version"]}")\n`;
}

module.exports = {
    generateKotlinCodeBase
};