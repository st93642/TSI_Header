/**
 * Scheme Code Base Generator
 * Generates boilerplate code for Scheme projects
 */

function generateSchemeCodeBase() {
    return `\n;; Basic Scheme program\n\n;; Main function - entry point of the program\n(define (main)\n  (display "Hello, World!")\n  (newline)\n  (display "This is a basic Scheme program.")\n  (newline))\n\n;; Execute main function\n(main)\n\n;; Alternative direct approach\n;; (display "Hello, World!")\n;; (newline)\n;; (display "This is a basic Scheme program.")\n;; (newline)\n\n;; Example function definition\n;; (define (greet name)\n;;   (string-append "Hello, " name "!"))\n\n;; Example usage\n;; (display (greet "TSI Student"))\n;; (newline)\n\n;; Example with lists\n;; (define numbers '(1 2 3 4 5))\n;; (define doubled (map (lambda (x) (* x 2)) numbers))\n;; (display "Doubled: ")\n;; (display doubled)\n;; (newline)\n`;
}

module.exports = {
    generateSchemeCodeBase
};