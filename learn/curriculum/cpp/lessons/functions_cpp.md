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

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Functions — APIs and Testing

This appendix shows how to structure headers, test pure functions, and add a simple CMake test target.

### Header and implementation pattern

`math_utils.h`:

```cpp
#pragma once
int add(int a, int b);
```

`math_utils.cpp`:

```cpp
#include "math_utils.h"
int add(int a, int b) { return a + b; }
```

### CMake test snippet

```cmake
add_executable(math_tests tests/math_tests.cpp)
find_package(Catch2 REQUIRED)
target_link_libraries(math_tests PRIVATE Catch2::Catch2WithMain)
add_test(NAME math-tests COMMAND math_tests)
```

### Exercises

1. Implement `add`, `sub`, `mul`, `div` with tests covering typical and edge cases (division by zero should throw).
2. Refactor a long function into smaller helpers and add unit tests for each.

<!-- markdownlint-enable MD010 -->

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

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Functions — Performance & API Design

This appendix covers common function-level design considerations: `noexcept`, `inline`, move semantics, and passing strategies.

```cpp
// Example: prefer passing by const ref for heavy objects
void process(const std::string &s);

// noexcept for small, non-throwing utilities
int add(int a, int b) noexcept { return a + b; }
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Strategy</th><th>When to use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>pass by value</td><td>Small trivials/when moving</td><td>Allows move from caller</td></tr>
    <tr><td>pass by const&</td><td>Large non-mutable</td><td>Avoids copies</td></tr>
    <tr><td>noexcept</td><td>Move ops, swap</td><td>Enables optimizations</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Practical Appendix)

1. Write `sort_by_key` taking a vector of pairs and a comparator; test exception-safety when comparator throws.
2. Add `noexcept` to the move constructor of a small type and measure effect on container move operations.

<!-- markdownlint-disable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Function Contracts & Testing (Appendix II)

Notes on designing clear contracts, writing precondition tests, and a small table comparing parameter passing styles.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Passing</th><th>When</th><th>Cost</th></tr>
  </thead>
  <tbody>
    <tr><td>by value</td><td>small trivials</td><td>copy cost</td></tr>
    <tr><td>by const&</td><td>large read-only</td><td>no copy</td></tr>
    <tr><td>by rvalue</td><td>take ownership</td><td>move</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix II — functions)

1. Add tests that assert preconditions using asserts or contract checks.
2. Document when to use `noexcept` for move operations.

<!-- markdownlint-enable MD010 -->

## Practical Appendix: External Tools & Examples (Appendix — External Tools — functions_cpp-appendix)

This appendix gives quick references for function-related topics (std::function, lambdas, noexcept), links to cppreference, and small test snippets to exercise functions in C++ lessons.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Resource</th><th>Focus</th><th>Link</th></tr>
  </thead>
  <tbody>
    <tr><td>cppreference: functions</td><td>Language & std utilities</td><td><a href="https://en.cppreference.com/w/cpp/language/functions">cppreference - functions</a></td></tr>
    <tr><td>cppreference: lambdas</td><td>Lambda syntax & captures</td><td><a href="https://en.cppreference.com/w/cpp/language/lambda">cppreference - lambda</a></td></tr>
    <tr><td>GoogleTest</td><td>Unit testing</td><td><a href="https://github.com/google/googletest">Google Test</a></td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Quick example (lambda wrapper)

```cpp
#include <functional>
#include <iostream>

int main() {
    auto add = [](int a, int b){ return a + b; };
    std::function<int(int,int)> f = add;
    std::cout << f(2,3) << "\n"; // 5
}
```

### Minimal GoogleTest example

```cpp
#include <gtest/gtest.h>

int add(int a, int b){ return a + b; }

TEST(AddTest, Basic) {
    EXPECT_EQ(5, add(2,3));
}
```

### Exercises (functions_cpp-appendix)

1. Implement a small higher-order function that takes a callable and returns a wrapped callable that logs arguments.
2. Add a basic GoogleTest that verifies behavior for at least two callables (lambda and function pointer).

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Functions — Design & Tests (Appendix — functions_cpp-continued)

A compact set of recipes for writing well-tested functions, designing APIs, and adding simple CI test targets for C++ lessons.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Focus</th><th>Why</th><th>Resource</th></tr>
  </thead>
  <tbody>
    <tr><td>Header/Source split</td><td>Encapsulation & linkage</td><td><a href="https://en.cppreference.com/w/cpp/language/functions">cppreference - functions</a></td></tr>
    <tr><td>constexpr</td><td>Compile-time computation</td><td><a href="https://en.cppreference.com/w/cpp/language/constexpr">constexpr</a></td></tr>
    <tr><td>Testing</td><td>Unit tests for pure functions</td><td><a href="https://github.com/google/googletest">GoogleTest</a></td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Minimal CMake test target

```cmake
# CMakeLists.txt snippet
add_executable(example_test tests/example_test.cpp)
find_package(GTest REQUIRED)
target_link_libraries(example_test PRIVATE GTest::gtest_main)
add_test(NAME example-test COMMAND example_test)
```

### Example function test (GoogleTest)

```cpp
#include <gtest/gtest.h>
int add(int a, int b) { return a + b; }

TEST(AddTest, Basic) {
    EXPECT_EQ(5, add(2,3));
}
```

### API design checklist

- Prefer clear names and short parameter lists.
- Document ownership and lifetime expectations (who owns pointers, if any).
- Use `const &` for heavy read-only params.
- Mark small, non-throwing functions `noexcept` when appropriate.

### Exercises (Appendix — functions_cpp-continued)

1. Add a GoogleTest for `triple(int)` and `average(double,double)`. Ensure tests compile under CMake.
2. Convert a small macro into an `inline constexpr` function and add a compile-time check using `static_assert`.


<!-- Practical Appendix: Reference and further reading -->

### Practical Appendix
This appendix contains brief practical notes and quick references to complement the lesson content. It is intentionally short and safe: no code execution or large data dumps.

- Reference: Official documentation and language core references are excellent further reading sources. Follow the standard docs for authoritative examples.
- Quick tips:
  - Re-run the examples in a REPL to experiment with small changes.
  - Use small, focused test cases when validating behavior.
  - Prefer idiomatic standard-library helpers for clarity and maintainability.

Further reading and sources:
- Official language documentation (search for "official <LANG> docs" where <LANG> is the lesson's language).
- Standard library reference and API pages.
- For curriculum authors: keep examples minimal and include runnable snippets in fenced code blocks.

*End of Practical Appendix.*
