/**
 * VBScript Code Base Generator
 * Generates boilerplate VBScript code
 */

function generateVBScriptCodeBase() {
    return `\n' Basic VBScript program\n\n' Main subroutine - entry point of the program\nSub Main()\n    MsgBox "Hello, World!"\n    MsgBox "This is a basic VBScript program."\n    \n    ' Alternative console output (for WSH)\n    ' WScript.Echo "Hello, World!"\n    ' WScript.Echo "This is a basic VBScript program."\nEnd Sub\n\n' Execute main subroutine\nMain()\n\n' Example function with parameters\n' Function Greet(name)\n'     If IsEmpty(name) Or name = "" Then\n'         name = "World"\n'     End If\n'     Greet = "Hello, " & name & "!"\n' End Function\n\n' Example usage\n' MsgBox Greet("TSI Student")\n' WScript.Echo Greet("TSI Student")\n\n' Example with variables and conditional logic\n' Dim message\n' message = "Hello from TSI!"\n' If message <> "" Then\n'     MsgBox message\n' End If\n\n' Example with loops\n' Dim i\n' For i = 1 To 3\n'     MsgBox "Count: " & i\n' Next\n`;
}

module.exports = {
    generateVBScriptCodeBase
};