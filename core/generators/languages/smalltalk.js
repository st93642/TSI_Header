/**
 * Smalltalk Code Base Generator
 * Generates boilerplate code for Smalltalk projects
 */

function generateSmalltalkCodeBase() {
    return `\n"Basic Smalltalk program"\n\n"Main class definition"\nObject subclass: #HelloWorld\n    instanceVariableNames: ''\n    classVariableNames: ''\n    poolDictionaries: ''\n    category: 'TSI-Examples'.\n\n"Main method"\nHelloWorld class >> main\n    "Main method - entry point of the program"\n    Transcript show: 'Hello, World!'.\n    Transcript cr.\n    Transcript show: 'This is a basic Smalltalk program.'.\n    Transcript cr.\n\n"Execute main method"\nHelloWorld main.\n\n"Alternative direct approach"\n"Transcript show: 'Hello, World!'; cr."\n"Transcript show: 'This is a basic Smalltalk program.'; cr."\n\n"Example instance method"\n"HelloWorld >> greet: aName\n    ^'Hello, ', aName, '!'"\n\n"Example usage"\n"| hello |\nhello := HelloWorld new.\nTranscript show: (hello greet: 'TSI Student'); cr."\n`;
}

module.exports = {
    generateSmalltalkCodeBase
};