/**
 * VB.NET Code Base Generator
 * Generates boilerplate VB.NET code
 */

function generateVbCodeBase() {
    return `\n' Basic VB.NET program\n\nImports System\n\nModule HelloWorld\n    ' Main subroutine - entry point of the program\n    Sub Main()\n        Console.WriteLine("Hello, World!")\n        Console.WriteLine("This is a basic VB.NET program.")\n        \n        ' Wait for user input\n        Console.ReadLine()\n    End Sub\n    \n    ' Example function\n    ' Function Greet(name As String) As String\n    '     If String.IsNullOrEmpty(name) Then\n    '         name = "World"\n    '     End If\n    '     Return "Hello, " & name & "!"\n    ' End Function\n    \n    ' Example usage\n    ' Console.WriteLine(Greet("TSI Student"))\nEnd Module\n\n' Alternative class-based approach\n' Public Class HelloWorldClass\n'     Public Shared Sub Main(args() As String)\n'         Console.WriteLine("Hello, World!")\n'         Console.WriteLine("This is a basic VB.NET program.")\n'     End Sub\n' End Class\n`;
}

module.exports = {
    generateVbCodeBase
};