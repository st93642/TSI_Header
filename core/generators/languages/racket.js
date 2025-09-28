/**
 * Racket Language Code Base Generator
 * Generates Racket code base/boilerplate code
 */

/**
 * Generates Racket code base
 * @returns {string} Racket code base template
 */
function generateRacketCodeBase() {
    return `\n#lang racket\n\n;; Main module definition\n(define (main)\n  (displayln "Hello, World!")\n  (displayln "This is a basic Racket program."))\n\n;; Execute main function\n(main)\n\n;; Example function definition\n(define (greet-user name)\n  (string-append "Hello, " name "!"))\n\n;; Example usage\n;; (displayln (greet-user "TSI Student"))\n\n;; Example with variables and data types\n;; (define user-name "TSI Student")\n;; (define user-age 20)\n;; (define greeting (string-append "Hello, " user-name "! You are " (number->string user-age) " years old."))\n;; (displayln greeting)\n\n;; Example with lists\n;; (define number-list (list 1 2 3 4 5))\n;; (for-each (lambda (num) (displayln (string-append "Number: " (number->string num)))) number-list)\n\n;; Example with structs (records)\n;; (struct person (name age program))\n;; (define student (person "TSI Student" 20 "Computer Science"))\n;; (displayln (string-append "Hello, I'm " (person-name student) " and I'm " (number->string (person-age student)) " years old!"))\n\n;; Example with conditional logic\n;; (define (check-number num)\n;;   (cond\n;;     [(> num 10) "Greater than 10"]\n;;     [(= num 10) "Equal to 10"]\n;;     [else "Less than 10"]))\n;; (displayln (check-number 15))\n\n;; Example with hash tables\n;; (define user-table (hash 'name "TSI Student" 'age 20 'program "Computer Science"))\n;; (displayln (string-append "Name: " (hash-ref user-table 'name)))\n\n;; Example with modules and requires\n;; (require racket/list)\n;; (define doubled-list (map (lambda (x) (* x 2)) (list 1 2 3 4 5)))\n;; (displayln doubled-list)`;
}

module.exports = {
    generateRacketCodeBase
};