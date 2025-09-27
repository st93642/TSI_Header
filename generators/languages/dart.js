/**
 * Dart Code Base Generator
 * Generates boilerplate Dart code
 */

function generateDartCodeBase() {
    return `\n// Basic Dart program\n\n// Main function - entry point of the program\nvoid main() {\n  print('Hello, World!');\n  print('This is a basic Dart program.');\n}\n\n// Alternative direct approach\n// void main() => print('Hello, World!');\n\n// Example function with parameters\nString greet(String name) {\n  return 'Hello, $name!';\n}\n\n// Example usage\n// void main() {\n//   print(greet('TSI Student'));\n// }\n\n// Example class definition\nclass TSIStudent {\n  String name;\n  String program;\n  \n  TSIStudent(this.name, this.program);\n  \n  void introduce() {\n    print('Hello, I\\'m $name from TSI!');\n  }\n}\n\n// Example usage\n// void main() {\n//   var student = TSIStudent('TSI Student', 'Computer Science');\n//   student.introduce();\n// }\n\n// Example with collections\n// void main() {\n//   var languages = ['Dart', 'Flutter', 'JavaScript'];\n//   var config = {\n//     'version': '1.0',\n//     'debug': true,\n//     'author': 'TSI Student'\n//   };\n//   \n//   languages.forEach((lang) => print('Language: $lang'));\n//   print('Version: \${config['version']}');\n// }\n`;
}

module.exports = {
    generateDartCodeBase
};