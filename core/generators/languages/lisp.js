/**
 * Lisp Code Base Generator
 * Generates boilerplate code for Lisp projects
 */

function generateLispCodeBase() {
    return `\n;; Basic Lisp program\n\n;; Main function - entry point of the program\n(defun main ()\n  (format t "Hello, World!~%")\n  (format t "This is a basic Lisp program.~%"))\n\n;; Execute main function\n(main)\n\n;; Alternative direct approach\n;; (format t "Hello, World!~%")\n;; (format t "This is a basic Lisp program.~%")\n\n;; Example function definition\n(defun greet (name)\n  (format t "Hello, ~a!~%" name))\n\n;; Example usage\n;; (greet "TSI Student")\n\n;; Example of list processing\n(defun process-list (lst)\n  (if (null lst)\n      nil\n      (cons (* 2 (car lst))\n            (process-list (cdr lst)))))\n\n;; Example usage\n;; (process-list '(1 2 3 4 5))\n`;
}

module.exports = {
    generateLispCodeBase
};