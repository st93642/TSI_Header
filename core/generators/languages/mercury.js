/**
 * Mercury Code Base Generator
 * Generates boilerplate code for Mercury projects
 */

function generateMercuryCodeBase() {
    return `\n%% Basic Mercury program\n\n:- module hello_world.\n:- interface.\n\n:- import_module io.\n\n:- pred main(io::di, io::uo) is det.\n\n:- implementation.\n\nmain(!IO) :-\n    io.write_string("Hello, World!\\n", !IO),\n    io.write_string("This is a basic Mercury program.\\n", !IO).\n\n%% To compile and run:\n%% mmc --make hello_world\n%% ./hello_world\n\n%% Example predicate\n%% :- pred greet(string::in, string::out) is det.\n%% greet(Name, Greeting) :-\n%%     string.append_list(["Hello, ", Name, "!"], Greeting).\n`;
}

module.exports = {
    generateMercuryCodeBase
};