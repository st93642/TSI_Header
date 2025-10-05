# Preprocessor Directives and Macros in C

The preprocessor transforms source code before compilation, enabling conditional compilation, macro expansion, and file inclusion. This lesson covers directives like #include, #define, #ifdef, and macro pitfalls, building robust, portable code. You'll learn to define constants, create function-like macros, and manage compilation environments.

## Learning goals

- Use #include for header inclusion.
- Define constants and macros with #define.
- Implement conditional compilation with #if, #ifdef, #ifndef.
- Create function-like macros with parameters.
- Avoid common macro pitfalls and use inline functions as alternatives.
- Manage include guards and header dependencies.

## 1. Introduction to preprocessor

Preprocessor runs before compiler, processing directives starting with #.

```c
// Comments are ignored, but directives are processed
#define PI 3.14159
#include <stdio.h>

int main(void)
{
    printf("PI: %f\n", PI);
    return 0;
}
```

### Checkpoint: Basic define

1. Define a macro for MAX_SIZE 100.
2. Use it in main to print.

## 2. Macro expansion

## define replaces text

```c
#define SQUARE(x) ((x) * (x))

int main(void)
{
    int result = SQUARE(5);
    printf("Square: %d\n", result);
    return 0;
}
```

### Checkpoint: Function macro

1. Define macro for MIN(a,b).
2. Test with values.

## 3. Conditional compilation

Compile code based on conditions.

```c
#define DEBUG 1

int main(void)
{
#ifdef DEBUG
    printf("Debug mode\n");
#endif
    return 0;
}
```

### Checkpoint: Conditional

1. Define DEBUG, use #ifdef to print debug info.

## 4. Include guards

Prevent multiple inclusion.

```c
// In header file
#ifndef MYHEADER_H
#define MYHEADER_H

// Declarations

#endif
```

### Checkpoint: Guard

1. Create header with guard, include in multiple files.

## 5. Predefined macros

__FILE__, __LINE__, __DATE__, etc.

```c
int main(void)
{
    printf("File: %s, Line: %d\n", __FILE__, __LINE__);
    return 0;
}
```

### Checkpoint: Predefined

1. Print __DATE__ and __TIME__.

## 6. Macro pitfalls

Avoid side effects.

```c
#define SQUARE(x) (x * x)  // Bad: no parentheses

int main(void)
{
    int y = SQUARE(1 + 2);  // 1 + 2 * 1 + 2 = 5, not 9
    return 0;
}
```

### Checkpoint: Fix macro

1. Fix SQUARE to use proper parentheses.

## 7. Undef and redefinition

## undef removes definition

```c
#define TEMP 10
#undef TEMP
```

### Checkpoint: Undef

1. Define, use, undef, try to use again.

## 8. Stringification and concatenation operators

""# and ## operators""

```c
#define PRINT_VAR(x) printf(#x " = %d\n", x)
#define CONCAT(a, b) a ## b

int main(void)
{
    int value = 42;
    PRINT_VAR(value);
    int CONCAT(var, 1) = 10;
    return 0;
}
```

### Checkpoint: Operators

1. Use # to stringify, ## to concatenate.

## 9. Variadic macros

__VA_ARGS__ for variable args.

```c
#define LOG(format, ...) printf(format, __VA_ARGS__)

int main(void)
{
    LOG("Value: %d\n", 42);
    return 0;
}
```

### Checkpoint: Variadic

1. Define LOG macro, use with multiple args.

## 10. Alternatives to macros

Use const, inline, enums.

```c
const double PI = 3.14159;
inline int square(int x) { return x * x; }
```

### Checkpoint: Alternatives

1. Replace macro with const and inline.

## 11. Mini project: Configurable logger

Create logger with macros for different levels.

1. Define macros for LOG_DEBUG, LOG_INFO, etc.
2. Use conditional compilation for levels.
3. Include in main, test.

### Success criteria

- Compiles with different defines.
- Outputs correct messages.

## 12. Guided practice challenges

1. Define platform-specific code.
2. Create assert macro.
3. Header with function declarations.
4. Macro for array size.
5. Conditional includes.

## 13. Self-check questions

1. Difference between #define and const?
2. When to use #ifdef?
3. What are include guards?
4. Pitfalls of function macros?

## Recap and next steps

Preprocessor enables flexible code. Next, explore multi-file programs and linking.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: External Tools & Examples (Appendix — External Tools — preprocessor_c-appendix)

Quick notes on preprocessing directives, macro hygiene, and references to cppreference's preprocessor page.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Directive</th><th>Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>#define</td><td>Define macros</td><td>Avoid complex macros; prefer inline functions</td></tr>
    <tr><td>#include</td><td>Include headers</td><td>Use angle brackets for system headers</td></tr>
    <tr><td>#ifdef / #ifndef</td><td>Conditional compilation</td><td>Use for platform-specific guards</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

Reference: [cppreference preprocessor](https://en.cppreference.com/w/c/preprocessor)

### Exercises (preprocessor_c-appendix)

1. Replace a small macro with an inline static function and run the unit tests to ensure behavior matches.
2. Add include guards or `#pragma once` to a sample header and document the differences.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Macro Hygiene & Alternatives (Appendix — preprocessor_c-continued)

Guidelines and small examples for replacing macros with safer constructs and testing preprocessor-driven code.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>Alternative</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Function-like macro</td><td>static inline function</td><td>Preserve behavior and type-safety</td></tr>
    <tr><td>Constants</td><td>const or enum</td><td>Avoid textual replacement pitfalls</td></tr>
    <tr><td>Conditional builds</td><td>CMake options</td><td>Prefer build-system flags over pervasive #defines</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: macro -> inline function

```c
// Bad macro
#define MAX(a,b) ((a) > (b) ? (a) : (b))

// Better
static inline int max_int(int a, int b) { return a > b ? a : b; }
```

### Testing note

When macros affect API shapes, prefer adding small integration tests that compile variants under different defines to ensure behaviour.

### Exercises (Appendix — preprocessor_c-continued)

1. Replace a function-like macro in a small sample with a `static inline` function and verify behaviour with unit tests.
2. Add a small CMake option that toggles a compile-time flag and ensure both variants compile in CI.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Preprocessor — Macros, Guards & Conditional Compilation (Appendix — preprocessor_c-appendix2)

Practical recipes for writing and testing preprocessor code: safe macros, header guards, and platform-specific compilation patterns.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Header guards</td><td>All headers</td><td>Use `#ifndef/#define/#endif` or `#pragma once`</td></tr>
    <tr><td>Function-like macros</td><td>Small inline code</td><td>Prefer inline functions when possible</td></tr>
    <tr><td>Conditional compile</td><td>Platform-specific</td><td>Keep build matrices small and explicit</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: header guard

```c
#ifndef MYLIB_UTILS_H
#define MYLIB_UTILS_H

/* declarations */

#endif /* MYLIB_UTILS_H */
```

### Macro hygiene

- Parenthesize macro parameters: `#define SQUARE(x) ((x) * (x))`
- Avoid macros that evaluate arguments multiple times when side effects matter.

### Conditional compilation example

```c
#ifdef _WIN32
  /* Windows-specific code */
#else
  /* POSIX code */
#endif
```

### Testing preprocessor-time behaviour

- Prefer small compile-time tests: create separate translation units that include your macros and confirm expansion via `-E` preprocessor output.
- Use `static_assert` where available to check sizes and constants.

```c
_Static_assert(sizeof(void*) == 8, "64-bit required");
```

### Exercises (Appendix — preprocessor_c-appendix2)

1. Create a header with proper guards and a small macro; compile with `-E` to view the expansion and add a short README describing the macro's behavior.
2. Write a small cross-platform compile example that uses `#ifdef` and provide a Makefile target that builds on both POSIX and Windows (where applicable).

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->


<!-- Practical Appendix: Reference and further reading -->

### Practical Appendix
This appendix contains brief practical notes and quick references to complement the lesson content. It is intentionally short and safe: no code execution or large data dumps.

- Reference: Official documentation and language core references are excellent further reading sources. Follow the standard docs for authoritative examples.
- Quick tips:
  - Re-run the examples in a REPL to experiment with small changes.
  - Use small, focused test cases when validating behavior.
  - Prefer idiomatic standard-library helpers for clarity and maintainability.

Further reading and sources:
- Official language documentation (search for `official <LANG> docs` where `<LANG>` is the lesson's language).
- Standard library reference and API pages.
- For curriculum authors: keep examples minimal and include runnable snippets in fenced code blocks.

*End of Practical Appendix.*
