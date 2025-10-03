# Defining and Using Functions in C

Functions are the building blocks of modular, reusable code. This lesson guides you from simple function definitions through parameter passing, return values, scope management, and recursion. You'll learn to decompose problems into callable units, handle errors gracefully, and design interfaces that promote clarity and maintainability. By the end, you'll refactor monolithic programs into collections of focused functions that collaborate seamlessly.

## Learning goals

- Define functions with proper signatures, including parameter lists and return types.
- Pass arguments by value and understand the implications for primitive and composite types.
- Implement functions that return values, handle void returns, and manage early exits.
- Manage variable scope and lifetime to avoid name collisions and unintended side effects.
- Apply recursion for problems with self-similar substructures, including base cases and stack depth limits.
- Design function interfaces that are self-documenting, testable, and easy to refactor.
- Debug function calls using stack traces and parameter inspection.

## 1. Why functions matter

Functions encapsulate logic into named, reusable blocks. They prevent code duplication, enable testing in isolation, and make programs easier to understand and modify. Before writing a function, ask:

1. What does this code do (single responsibility)?
2. What inputs does it need (parameters)?
3. What does it produce (return value)?
4. How will it be called (interface design)?

```c
/* Pseudocode for function design */
return_type function_name(parameter_type parameter_name)
{
    /* implementation */
    return result;
}
```

Use descriptive names that convey intent: `calculate_average` instead of `avg`. This habit scales to larger codebases where functions are called from distant files.

### Checkpoint: Identify function candidates

Review a program you've written. Circle sections that could become functions. For each:

- Name the function.
- List its parameters and return type.
- Explain why extracting it improves the code.

## 2. Function declarations and definitions

In C, functions are declared (prototyped) before use and defined elsewhere. Declarations tell the compiler about the function's signature without providing the body.

```c
#include <stdio.h>

/* Declaration (prototype) */
int add(int a, int b);

int main(void)
{
    int result = add(3, 5);
    printf("Sum: %d\n", result);
    return 0;
}

/* Definition */
int add(int a, int b)
{
    return a + b;
}
```

Best practices:

- Place prototypes in header files (.h) for multi-file programs.
- Match parameter names in declarations and definitions for clarity.
- Use `void` for functions that take no parameters or return nothing.

### Checkpoint: Separate declaration and definition

1. Write a program that declares `double square_root(double x);` in a header-like comment.
2. Define the function below `main`, using a simple approximation (e.g., `x / 2.0`).
3. Call it from `main` and print the result for `x = 16.0`.

## 3. Parameters and arguments

Parameters receive values from the caller. In C, arguments are passed by value, meaning the function receives copies. Modifying a parameter inside the function doesn't affect the original variable.

```c
#include <stdio.h>

void swap(int x, int y)
{
    int temp = x;
    x = y;
    y = temp;
}

int main(void)
{
    int a = 10, b = 20;
    printf("Before: a=%d, b=%d\n", a, b);
    swap(a, b);
    printf("After: a=%d, b=%d\n", a, b); /* a and b unchanged */
    return 0;
}
```

To modify originals, use pointers (covered in future lessons).

Tips:

- Validate parameters at the start of the function.
- Use `const` for parameters you won't modify to signal intent.
- Document parameter units and ranges in comments.

### Checkpoint: Parameter validation

1. Define a function `int divide(int dividend, int divisor)` that returns the quotient.
2. Check if `divisor` is zero; if so, return 0 and print an error.
3. Test with valid and invalid inputs from `main`.

## 4. Return values and types

Functions can return a single value of any type, including structs (advanced). Use `return` to exit and provide the result. Functions without a return value are `void`.

```c
#include <stdio.h>

double average(int a, int b, int c)
{
    return (a + b + c) / 3.0;
}

void greet(const char *name)
{
    printf("Hello, %s!\n", name);
}

int main(void)
{
    double avg = average(10, 20, 30);
    printf("Average: %.2f\n", avg);
    greet("TSI");
    return 0;
}
```

Guidelines:

- Return meaningful values; avoid magic numbers like -1 for errors.
- Use early returns to simplify control flow.
- For complex returns, consider structs or output parameters.

### Checkpoint: Multi-return simulation

1. Write a function that computes both sum and product of two integers, returning the sum and using a pointer for the product.
2. In `main`, call it and print both results.
3. Discuss why this is preferable to two separate functions.

## 5. Scope and lifetime

Variables declared inside functions are local and exist only during execution. Global variables persist but should be used sparingly.

```c
#include <stdio.h>

int global_counter = 0;

void increment(void)
{
    int local_var = 1;
    global_counter += local_var;
}

int main(void)
{
    increment();
    printf("Counter: %d\n", global_counter); /* 1 */
    /* local_var is gone */
    return 0;
}
```

Concepts:

- **Block scope**: Variables in `{}` blocks are local to that block.
- **Function scope**: Parameters and locals hide globals with the same name.
- **Lifetime**: Locals are created on entry, destroyed on exit; globals live for the program.

### Checkpoint: Scope shadowing

1. Declare a global `int x = 100;`.
2. In `main`, declare a local `int x = 50;`.
3. Write a function that prints the global `x` using `extern int x;`.
4. Call it from `main` and observe the difference.

## 6. Recursion basics

Recursive functions call themselves to solve self-similar problems. They require a base case to terminate.

```c
#include <stdio.h>

int factorial(int n)
{
    if (n <= 1)
    {
        return 1; /* base case */
    }
    return n * factorial(n - 1); /* recursive case */
}

int main(void)
{
    printf("5! = %d\n", factorial(5));
    return 0;
}
```

Pitfalls:

- Stack overflow for deep recursion; limit to small depths.
- Ensure progress toward the base case.
- Recursion can be less efficient than iteration for simple loops.

### Checkpoint: Recursive sum

1. Implement `int sum_up_to(int n)` that sums 1 to n recursively.
2. Test with n=10 and verify against the formula `n*(n+1)/2`.
3. Add a depth counter and print it to see the call stack.

## 7. Function pointers and callbacks

Functions can be passed as arguments using pointers, enabling callbacks and generic algorithms.

```c
#include <stdio.h>

void apply_operation(int a, int b, int (*op)(int, int))
{
    printf("Result: %d\n", op(a, b));
}

int add(int x, int y) { return x + y; }
int multiply(int x, int y) { return x * y; }

int main(void)
{
    apply_operation(3, 4, add);
    apply_operation(3, 4, multiply);
    return 0;
}
```

Use cases: sorting with custom comparators, event handlers.

### Checkpoint: Custom comparator

1. Write a function that sorts an array using bubble sort and a function pointer for comparison.
2. Define ascending and descending comparators.
3. Test sorting {5, 2, 8, 1} in both orders.

## 8. Error handling in functions

Functions should handle errors gracefully. Use return codes, global error variables, or assertions.

```c
#include <stdio.h>
#include <stdlib.h>

int safe_divide(int a, int b, int *result)
{
    if (b == 0)
    {
        return 0; /* error */
    }
    *result = a / b;
    return 1; /* success */
}

int main(void)
{
    int res;
    if (safe_divide(10, 0, &res))
    {
        printf("Result: %d\n", res);
    }
    else
    {
        printf("Division by zero!\n");
    }
    return 0;
}
```

### Checkpoint: Robust input parser

1. Write a function that parses a string to int, returning success/failure.
2. Handle invalid characters and overflow.
3. Use it in `main` to read user input safely.

## 9. Inline functions and optimisation

For small functions, use `inline` (C99+) to suggest the compiler embed the code, reducing call overhead.

```c
#include <stdio.h>

inline int max(int a, int b)
{
    return a > b ? a : b;
}

int main(void)
{
    printf("Max: %d\n", max(5, 10));
    return 0;
}
```

Use sparingly; let the compiler decide.

### Checkpoint: Inline vs. macro

1. Define `max` as a macro and as an inline function.
2. Compare assembly output or performance for large loops.
3. Note any differences in type safety.

## 10. Modular design principles

Design functions to be:

- **Cohesive**: Do one thing well.
- **Loosely coupled**: Minimize dependencies.
- **Testable**: Easy to verify in isolation.

Refactor large functions by extracting helpers.

### Checkpoint: Refactor a monolithic function

1. Take a function with multiple responsibilities.
2. Break it into 2-3 smaller functions.
3. Ensure the interface remains the same.

## 11. Debugging function calls

Use debuggers to step into functions, inspect parameters, and watch variables.

```c
#include <stdio.h>

void debug_me(int x)
{
    printf("Entered with x=%d\n", x);
    /* breakpoint here */
}

int main(void)
{
    debug_me(42);
    return 0;
}
```

### Checkpoint: Stack trace simulation

1. Write a recursive function that prints its depth.
2. Set a breakpoint and observe the call stack.
3. Force a stack overflow and catch it.

## 12. Mini project: Simple calculator

Build a calculator that supports addition, subtraction, multiplication, division.

1. Define functions for each operation.
2. Write a parser that reads "op a b" and calls the appropriate function.
3. Handle division by zero and invalid operations.
4. Add a history feature using an array of structs.

### Success criteria

- Compiles cleanly.
- Functions are tested individually.
- Input validation prevents crashes.

## 13. Guided practice challenges

1. **Fibonacci**: Implement recursive and iterative versions.
2. **String length**: Write your own `strlen` using pointers.
3. **Array search**: Binary search as a function.
4. **Matrix operations**: Add and multiply matrices with functions.
5. **File processor**: Read lines and apply a callback function.

## 14. Self-check questions

1. Why pass by value in C? When would you use pointers?
2. How do you prevent stack overflow in recursion?
3. What are the benefits of function prototypes?
4. How can function pointers enable polymorphism?
5. When to use inline functions?

## Recap and next steps

Functions empower you to write modular, maintainable C code. Practice decomposing problems into functions, then move to pointers and memory management in the next lesson.
