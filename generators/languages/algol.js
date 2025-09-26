/**
 * ALGOL Language Code Base Generator
 * Generates ALGOL 60/68 program boilerplate
 */

/**
 * Generates ALGOL code base
 * @returns {string} ALGOL program template
 */
function generateAlgolCodeBase() {
    return `\n; ALGOL Program Example
; This demonstrates ALGOL 60 syntax and structure

BEGIN
    ; Variable declarations
    INTEGER i, j, result;
    REAL x, y, sum;
    BOOLEAN flag;
    STRING message;

    ; Array declarations
    INTEGER ARRAY numbers[1:10];
    REAL ARRAY matrix[1:3, 1:3];

    ; Procedure declarations
    PROCEDURE add(INTEGER a, b) RESULT: (a + b);

    PROCEDURE factorial(INTEGER n) RESULT:
        BEGIN
            INTEGER result;
            result := 1;
            FOR i := 1 STEP 1 UNTIL n DO
                result := result * i;
            factorial := result
        END;

    PROCEDURE print_array(INTEGER ARRAY arr[1:n]);
        VALUE n; INTEGER n;
    BEGIN
        INTEGER i;
        FOR i := 1 STEP 1 UNTIL n DO
            OUTSTRING("arr[");
            OUTINTEGER(i);
            OUTSTRING("] = ");
            OUTINTEGER(arr[i]);
            OUTSTRING("; ")
    END;

    ; Main program logic
    i := 1;
    j := 2;
    result := add(i, j);

    x := 3.14;
    y := 2.71;
    sum := x + y;

    flag := TRUE;
    message := "Hello, ALGOL World!";

    ; Array initialization
    FOR i := 1 STEP 1 UNTIL 10 DO
        numbers[i] := i * i;

    ; Matrix operations
    FOR i := 1 STEP 1 UNTIL 3 DO
        FOR j := 1 STEP 1 UNTIL 3 DO
            matrix[i, j] := i + j;

    ; Conditional statements
    IF result > 0 THEN
        OUTSTRING("Result is positive")
    ELSE
        OUTSTRING("Result is not positive");

    ; Switch statement (simulated with IF-THEN-ELSE)
    IF i = 1 THEN
        OUTSTRING("Case 1")
    ELSE IF i = 2 THEN
        OUTSTRING("Case 2")
    ELSE
        OUTSTRING("Default case");

    ; Loop examples
    FOR i := 1 STEP 1 UNTIL 5 DO
    BEGIN
        OUTINTEGER(i);
        OUTSTRING(" ")
    END;

    ; While loop (simulated)
    i := 1;
    WHILE i <= 5 DO
    BEGIN
        OUTINTEGER(factorial(i));
        OUTSTRING("; ");
        i := i + 1
    END;

    ; Procedure calls
    print_array(numbers, 10);

    ; String operations
    OUTSTRING(message);
    OUTSTRING("; Length: ");
    OUTINTEGER(LENGTH(message));

    ; Mathematical operations
    OUTSTRING("; Square root of 16: ");
    OUTREAL(SQRT(16.0));

    OUTSTRING("; Sine of 0: ");
    OUTREAL(SIN(0.0));

    ; File I/O simulation
    ; Note: ALGOL I/O varies by implementation
    ; This shows conceptual file operations

    ; Error handling (conceptual)
    IF flag THEN
        OUTSTRING("Program completed successfully")
    ELSE
        OUTSTRING("An error occurred");

    ; Clean up and exit
    OUTSTRING("; Program finished")

END`;
}

module.exports = {
    generateAlgolCodeBase
};