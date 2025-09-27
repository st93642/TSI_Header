/**
 * Octave Code Base Generator
 * Generates boilerplate code for Octave (MATLAB clone) projects
 */

function generateOctaveCodeBase() {
    return `\n% Basic Octave program\n\n% Main function - entry point of the program\nfunction main()\n    disp('Hello, World!');\n    disp('This is a basic Octave script.');\nend\n\n% Execute main function\nmain();\n\n% Alternative direct approach\n% disp('Hello, World!');\n% disp('This is a basic Octave script.');\n\n% Example mathematical operations\n% x = [1, 2, 3, 4, 5];\n% y = x .^ 2;\n% disp('Squares:');\n% disp(y);\n\n% Example plotting (uncomment to use)\n% x = linspace(0, 2*pi, 100);\n% y = sin(x);\n% plot(x, y);\n% title('Sine Wave');\n% xlabel('x');\n% ylabel('sin(x)');\n`;
}

module.exports = {
    generateOctaveCodeBase
};