# Header Basics and Organisation

As programs grow, you organise declarations in header files and definitions in source files. This lesson walks through practical patterns for keeping those files tidy.

## Declarations vs. definitions

- **Declarations** announce the existence of functions, variables, or types.
- **Definitions** provide the implementation or storage.

Headers typically contain declarations so other translation units know what exists.

## Include guards

Every header should prevent multiple inclusion using guards or `#pragma once`:

```cpp
#ifndef MATH_UTILS_H
#define MATH_UTILS_H

int add(int a, int b);

#endif
```

Many modern projects favour `#pragma once` for brevity, but guards work everywhere.

## Splitting files

A common layout is:

- `math_utils.h` with declarations and documentation
- `math_utils.cpp` with function definitions
- `main.cpp` that includes the header and calls the functions

## Forward declarations

When a function definition appears after `main`, a forward declaration keeps the compiler informed:

```cpp
int triple(int value); // looks like a header declaration

int main() {
    std::cout << triple(4);
}

int triple(int value) {
    return value * 3;
}
```

## Practice Time

Try the following before moving on:

1. Sketch a header snippet that declares `double fahrenheit_to_celsius(double);` and `double kilometers_to_meters(double);`.
2. In the implementation file, define both functions so they return the converted values.
3. Call the functions from `main` and print labelled results with clear units.
4. Add a closing comment that reminds you which header the declarations represent.

When you are ready, open the exercise to reinforce declarations, definitions, and organisation.
