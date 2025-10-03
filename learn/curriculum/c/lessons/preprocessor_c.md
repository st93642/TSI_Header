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
