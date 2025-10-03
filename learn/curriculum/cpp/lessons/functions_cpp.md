# Writing and Reusing Functions

Functions package logic into reusable blocks, making code easier to reason about, test, and maintain. This extended lesson explores function anatomy, parameter passing strategies, overloads, and best practices for organising function declarations across headers and source files.

## Learning goals

- Define functions with clear signatures and return types.
- Decide when to pass arguments by value, reference, or const reference.
- Use default arguments and overloading to model related behaviour.
- Separate declarations and definitions for multi-file projects.
- Document function intent and understand how scope affects linkage.

## Function anatomy

A function definition specifies a return type, name, parameter list, and body:

```cpp
int doubleNumber(int value) {
    return value * 2;
}
```

Call the function using its name and parentheses, supplying arguments in the same order as the parameters. Functions must be visible to the caller at compile time through either a prior definition or a forward declaration.

### Naming and scope

- Use verbs for function names (`calculateTotal`, `printReport`).
- Keep functions focused on a single responsibility. If a function has to toggle features based on flags, consider splitting it.
- Functions defined in a namespace should be qualified with the namespace name when used elsewhere.

## Return values and side effects

The `return` statement specifies the value to send back to the caller and ends the function immediately.

- Use `void` when you only need side effects, like printing or mutating passed-in references.
- For computations, return the result and let the caller decide what to do with it.
- Prefer returning values over output parameters unless performance or semantics demand otherwise.

```cpp
double average(double first, double second) {
    return (first + second) / 2.0;
}

void printResult(double value) {
    std::cout << "Average: " << value << '\n';
}
```

## Parameter passing strategies

- **Pass-by-value** (default): arguments are copied. Great for small types or when you need an isolated copy.
- **Pass-by-reference** (`int&`): lets you modify the caller's variable. Be explicit—mutations should be documented.
- **Pass-by-const-reference** (`const std::string&`): avoids copies for large objects while guaranteeing read-only access.
- **Pass-by-pointer**: useful when `nullptr` conveys "no object" or when working with APIs expecting pointers.

```cpp
void increment(int& value) { ++value; }

void appendMessage(std::string& log, const std::string& line) {
    log.append(line);
    log.push_back('\n');
}
```

## Default arguments and overloading

Default arguments provide fallback values when callers omit parameters:

```cpp
double calculatePay(double hours, double rate = 15.0) {
    return hours * rate;
}
```

Function overloading lets you reuse a name with different parameter lists:

```cpp
int max(int a, int b);
double max(double a, double b);
```

Only the parameter types (and count) participate in overload resolution; return types alone cannot differentiate overloads.

## Organising declarations and definitions

When working across multiple files, place declarations in headers and definitions in source files. Callers include the header to compile against the signature.

```cpp
// math_utils.h
int triple(int value);

// math_utils.cpp
int triple(int value) {
    return value * 3;
}
```

Include guards or `#pragma once` prevent duplicate declarations. Remember to include the header in the source file implementing the functions.

### Forward declarations

If you define a function after `main`, provide a prototype beforehand:

```cpp
int doubleNumber(int value); // declaration

int main() {
    std::cout << doubleNumber(5) << '\n';
}

int doubleNumber(int value) { // definition
    return value * 2;
}
```

## Inline and constexpr functions

- `inline` hints to place the function body in headers safely. The compiler decides whether to expand it inline.
- `constexpr` functions can run at compile time when given constant expressions, enabling zero-cost abstractions for simple calculations.

```cpp
constexpr int square(int n) { return n * n; }
static_assert(square(4) == 16);
```

## Testing and documenting functions

- Keep parameter lists short; break large functions into helpers.
- Write unit tests for pure functions—they are easy to verify.
- Document assumptions and side effects, either in comments or in the header above the declaration.

## Practice time

1. **Math helpers:** Implement `int triple(int value)` and `double average(double first, double second)` in a header/source pair. Use const references where appropriate.
2. **Overloaded greet:** Overload `greet` so one version prints `Hello, name!` and another prints `Hello, title name!` with optional title argument.
3. **Temperature conversions:** Create `constexpr` functions that convert Celsius to Fahrenheit and back. Verify them with `static_assert` checks.
4. **Log aggregator:** Write a function that accepts a `std::vector<std::string>` by const reference and returns a single newline-joined string. Add an overload that accepts `std::initializer_list<std::string>` for convenience.

## Self-check questions

1. When should you pass by const reference instead of by value?
2. Why can't you overload functions using only return type differences?
3. How does a default argument differ from an overload with fewer parameters?
4. What benefits do `constexpr` functions provide for compile-time calculations?
5. Where should you place the declaration versus definition of a function in a multi-file project?

Once you are comfortable with these patterns, move on to the exercise to cement your function-writing workflow.
