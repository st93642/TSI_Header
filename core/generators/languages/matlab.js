/**
 * MATLAB Code Base Generator
 * Generates boilerplate MATLAB code
 */

function generateMatlabCodeBase() {
    return `\n% Basic MATLAB program\n\n% Main script - entry point\nfprintf('Hello, World!\\n');\nfprintf('This is a basic MATLAB script.\\n');\n\n% Example function definition (save as separate greet.m file)\n% function greet(name)\n%     if nargin < 1\n%         name = 'World';\n%     end\n%     fprintf('Hello, %s!\\n', name);\n% end\n\n% Example usage\n% greet('TSI Student');\n\n% Example with matrices and plotting\n% Clear workspace\n% clear; clc;\n% \n% % Create sample data\n% x = linspace(0, 2*pi, 100);\n% y = sin(x);\n% \n% % Plot the data\n% figure;\n% plot(x, y);\n% title('Sine Wave');\n% xlabel('x');\n% ylabel('sin(x)');\n% grid on;\n% \n% % Matrix operations\n% A = [1, 2, 3; 4, 5, 6; 7, 8, 9];\n% B = A * 2;\n% disp('Original matrix:');\n% disp(A);\n% disp('Doubled matrix:');\n% disp(B);\n\n% Example data analysis\n% data = randn(1000, 1);  % Generate random data\n% mean_val = mean(data);\n% std_val = std(data);\n% fprintf('Mean: %.4f, Standard deviation: %.4f\\n', mean_val, std_val);\n`;
}

module.exports = {
    generateMatlabCodeBase
};