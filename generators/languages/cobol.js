/**
 * COBOL Code Base Generator
 * Generates boilerplate COBOL code
 */

function generateCobolCodeBase() {
    return `\n      * Basic COBOL program\n       IDENTIFICATION DIVISION.\n       PROGRAM-ID. HELLO-WORLD.\n       AUTHOR. TSI-STUDENT.\n       \n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  WS-MESSAGE         PIC X(50) VALUE 'Hello, World!'.\n       01  WS-PROGRAM         PIC X(50) VALUE 'This is a basic COBOL program.'.\n       \n       PROCEDURE DIVISION.\n       MAIN-PROCEDURE.\n           DISPLAY WS-MESSAGE.\n           DISPLAY WS-PROGRAM.\n           \n           STOP RUN.\n           \n       END PROGRAM HELLO-WORLD.\n\n      * Example subroutine (uncomment to use)\n      * IDENTIFICATION DIVISION.\n      * PROGRAM-ID. GREET.\n      * \n      * DATA DIVISION.\n      * LINKAGE SECTION.\n      * 01  LS-NAME            PIC X(20).\n      * \n      * PROCEDURE DIVISION USING LS-NAME.\n      *     DISPLAY 'Hello, ' LS-NAME '!'.\n      *     EXIT PROGRAM.\n      * END PROGRAM GREET.\n`;
}

module.exports = {
    generateCobolCodeBase
};