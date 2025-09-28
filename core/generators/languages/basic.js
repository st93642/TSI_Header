/**
 * Basic Code Base Generator
 * Generates boilerplate Basic code
 */

function generateBasicCodeBase() {
    return `\n' Basic BASIC program\n\n' Main program - entry point\nPRINT "Hello, World!"\nPRINT "This is a basic BASIC program."\n\n' Example subroutine\n' SUB Greet(name$)\n'     PRINT "Hello, "; name$; "!"\n' END SUB\n\n' Example usage\n' CALL Greet("TSI Student")\n\n' Example with variables and loops\n' DIM languages$(3)\n' languages$(1) = "BASIC"\n' languages$(2) = "Pascal"\n' languages$(3) = "C"\n' \n' FOR i = 1 TO 3\n'     PRINT "Language: "; languages$(i)\n' NEXT i\n\nEND\n`;
}

module.exports = {
    generateBasicCodeBase
};