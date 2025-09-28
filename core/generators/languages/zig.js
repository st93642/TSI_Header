/**
 * Zig Language Code Base Generator
 * Generates Zig code base/boilerplate code
 */

/**
 * Generates Zig code base
 * @returns {string} Zig code base template
 */
function generateZigCodeBase() {
    return `\nconst std = @import("std");\n\npub fn main() !void {\n    const stdout = std.io.getStdOut().writer();\n    try stdout.print("Hello, World! This is a basic Zig program.\\n", .{});\n}\n`;
}

module.exports = {
    generateZigCodeBase
};