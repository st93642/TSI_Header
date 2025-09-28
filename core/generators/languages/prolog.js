/**
 * Prolog Code Base Generator
 * Generates boilerplate Prolog code
 */

function generatePrologCodeBase() {
    return `\n% Basic Prolog program\n\n% Main predicate - entry point\nhello_world :- \n    write('Hello, World!'), nl,\n    write('This is a basic Prolog program.'), nl.\n\n% Execute main predicate\n:- hello_world.\n\n% Alternative direct approach\n% :- write('Hello, World!'), nl,\n%    write('This is a basic Prolog program.'), nl.\n\n% Example predicate with parameters\n% greet(Name) :-\n%     write('Hello, '),\n%     write(Name),\n%     write('!'), nl.\n\n% Example usage\n% :- greet('TSI Student').\n\n% Example with facts and rules\n% student(tsi_student, computer_science).\n% student(jane_doe, electrical_engineering).\n% \n% is_computer_science_student(Name) :-\n%     student(Name, computer_science).\n% \n% Example query\n% ?- is_computer_science_student(tsi_student).\n% ?- student(Name, Program).\n`;
}

module.exports = {
    generatePrologCodeBase
};