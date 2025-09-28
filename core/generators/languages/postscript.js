/**
 * PostScript Code Base Generator
 * Generates boilerplate code for PostScript projects
 */

function generatePostScriptCodeBase() {
    return `\n%% Basic PostScript program\n\n%% Main procedure - entry point of the program\n/main {\n    (Hello, World!) show\n    newline\n    (This is a basic PostScript program.) show\n    newline\n} def\n\n%% Execute main procedure\nmain\n\n%% Alternative direct approach\n%% (Hello, World!) show\n%% newline\n%% (This is a basic PostScript program.) show\n%% newline\n\n%% Example procedures\n%% /greet {\n%%     /name exch def\n%%     (Hello, ) show\n%%     name show\n%%     (!) show\n%%     newline\n%% } def\n\n%% Example usage\n%% (TSI Student) greet\n\n%% Example drawing (uncomment to use)\n%% newpath\n%% 100 100 50 0 360 arc\n%% stroke\n%% showpage\n`;
}

module.exports = {
    generatePostScriptCodeBase
};