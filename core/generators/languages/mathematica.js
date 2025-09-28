/**
 * Mathematica Code Base Generator
 * Generates boilerplate code for Mathematica projects
 */

function generateMathematicaCodeBase() {
    return `\n(* Basic Mathematica program *)\n\n(* Main function - entry point of the program *)\nhelloWorld[] := (\n  Print["Hello, World!"];\n  Print["This is a basic Mathematica program."]\n)\n\n(* Execute main function *)\nhelloWorld[]\n\n(* Alternative direct approach *)\n(* Print["Hello, World!"] *)\n(* Print["This is a basic Mathematica program."] *)\n\n(* Example mathematical computations *)\n(* f[x_] := x^2 + 2*x + 1 *)\n(* result = f[5] *)\n(* Print["f(5) = ", result] *)\n\n(* Example symbolic math *)\n(* expr = x^3 - 6*x^2 + 11*x - 6; *)\n(* factored = Factor[expr]; *)\n(* Print["Factored form: ", factored] *)\n\n(* Example plotting (uncomment to use) *)\n(* Plot[Sin[x], {x, 0, 2*Pi}] *)\n`;
}

module.exports = {
    generateMathematicaCodeBase
};