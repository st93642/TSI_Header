/**
 * Perl Code Base Generator
 * Generates boilerplate code for Perl projects
 */

function generatePerlCodeBase() {
    return `\n#!/usr/bin/env perl\n\n# Basic Perl program\n\nuse strict;\nuse warnings;\nuse v5.10;\n\n# Main subroutine - entry point of the program\nsub main {\n    say "Hello, World!";\n    say "This is a basic Perl script.";\n}\n\n# Execute main subroutine\nmain();\n\n# Alternative direct approach\n# say "Hello, World!";\n# say "This is a basic Perl script.";\n\n# Example subroutine with parameters\n# sub greet {\n#     my ($name) = @_;\n#     return "Hello, " . ($name // "World") . "!";\n# }\n\n# Example usage\n# say greet("TSI Student");\n\n# Example with arrays and hashes\n# my @languages = ("Perl", "Python", "Ruby");\n# my %config = (\n#     version => "1.0",\n#     debug   => 1,\n#     author  => "TSI Student"\n# );\n\n# foreach my $lang (@languages) {\n#     say "Language: $lang";\n# }\n`;
}

module.exports = {
    generatePerlCodeBase
};