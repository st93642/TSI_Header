# Variables and Type Basics

C++ is strongly typed: every object has a well-defined type that dictates what operations are valid and how many bytes are required. This expanded lesson builds intuition for primitive types, type safety, and modern initialization practices so that values remain predictable as programs grow.

## Learning goals

By the end of this lesson you will be able to:

- Define variables with explicit or deduced types while avoiding uninitialized storage.
- Choose between fundamental numeric types (`int`, `long long`, `float`, `double`), `bool`, `char`, and `std::string` based on required range and semantics.
- Apply `const`, `constexpr`, and `auto` to express intent and enable compiler optimizations.
- Understand narrowing conversions and how brace initialization prevents them.
- Use structured bindings and references to access compound data without copying.

## The anatomy of a declaration

Every variable declaration follows this pattern:

```text
storage-specifiers type declarator = initialiser;
```

Examples:

```cpp
int score{};                // zero-initialized, automatic storage
const double vatRate{0.21}; // read-only constant value
std::string city = "Riga";  // requires #include <string>
```

- The **type** describes the kind of data and operations allowed.
- The **declarator** (identifier) names the object.
- The **initializer** sets an initial value; omit it and you risk indeterminate data for fundamental types.

### Storage duration and lifetime

- **Automatic** (default inside a block): object exists while the enclosing scope executes.
- **Static** (`static int counter`): created once, persists until program exit.
- **Dynamic** (`new`/`delete`): manual lifetime management; avoid until you understand RAII and smart pointers.

Prefer automatic storage unless you have a compelling reason otherwise.

## Fundamental type families


<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Category</th><th>Typical types</th><th>Use when</th></tr>
  </thead>
  <tbody>
    <tr><td>Integers</td><td><code>int</code>, <code>short</code>, <code>long</code>, <code>long long</code>, <code>std::size_t</code></td><td>Counting, indexing, discrete values</td></tr>
    <tr><td>Floating point</td><td><code>float</code>, <code>double</code>, <code>long double</code></td><td>Measurements requiring fractions</td></tr>
    <tr><td>Boolean</td><td><code>bool</code></td><td>Flags, logical decisions</td></tr>
    <tr><td>Character</td><td><code>char</code>, <code>wchar_t</code>, <code>char16_t</code>, <code>char32_t</code></td><td>Single code units (ASCII/Unicode)</td></tr>
    <tr><td>Text</td><td><code>std::string</code>, <code>std::u16string</code></td><td>Human-readable text</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

Key guidelines:

- Start with `int` for counters; use `std::size_t` when indexing containers.
- Use `double` for real numbers—the extra precision vs `float` is worth the cost on modern CPUs.
- Avoid unsigned integers for loop indices unless you understand wraparound behavior (negative values silently convert to large positives).
- Use `char` for individual characters, `std::string` for sequences.

## Initialization styles compared

```cpp
int width = 640;        // copy initialization
int height(480);        // direct initialization
int depth{24};          // brace (list) initialization
int maxDepth{};         // value initialization (zero)
```

- **Copy initialization** invokes implicit conversions; `double rate = 4;` converts 4 to 4.0.
- **Direct initialization** behaves similarly but can select explicit constructors for class types.
- **Brace initialization** prevents narrowing conversions:

```cpp
int tiny{3.5};   // error: narrowing from double to int
int tiny = 3.5;  // OK but value becomes 3 (narrowed)
```

Default to braces—they make mistakes visible.

### Zero-initialization with `{}`

```cpp
int counter{};      // 0
double total{};     // 0.0
bool isActive{};    // false
std::string name{}; // empty string
```

Using `{}` ensures deterministic starting values without verbose assignments inside the body of `main`.

## Constants and immutability

- `const` objects cannot change after initialization. Use this for configuration values or intermediate results that should not mutate.
- `constexpr` implies `const` **and** guarantees the value is usable at compile time (when the initializer is constant). This enables array bounds, template parameters, and switch cases.

```cpp
constexpr int semesterWeeks{16};
const double tuitionPerCredit{42.5};
```

Favor `constexpr` when the value never depends on runtime input.

## Type deduction with `auto`

`auto` helps eliminate redundant type declarations while preserving strong typing. The deduced type is determined by the initializer.

```cpp
auto ratio = 0.6180339887;        // double
auto name = std::string{"Ada"};  // std::string
const auto maxCredits = 180;      // const int
auto& ref = tuitionPerCredit;     // reference (when `&` is present)
```

Guidelines:

- Use `auto` when the type is obvious or irrelevant (iterators, lambda expressions).
- Avoid `auto` if the initializer hides important semantics (`auto flag = getStatus();` may be unclear if `flag` is bool or enum).
- Combine with `const`/`&` to control mutability and value category.

### Structured bindings (C++17)

```cpp
std::pair<int, double> result{42, 3.14};
auto [count, average] = result; // count: int, average: double
```

Use structured bindings to unpack tuples, pairs, or structs returned from helper functions without creating temporary variables.

## Implicit conversions and casting

Conversions happen automatically when types differ. Classify them as:

- **Safe widening**: `int` → `double` (no data loss).
- **Potentially unsafe narrowing**: `double` → `int`, `long long` → `int`.
- **Signed/unsigned mismatch**: `-1` stored in `unsigned int` becomes a large positive number.

Detect narrowing with braces or static analysis tools. When you must convert, be explicit:

```cpp
double precise = 42.75;
int rounded = static_cast<int>(precise);
```

## Scope and shadowing

Variables live inside blocks `{}`. Declaring a variable with the same name in an inner scope hides the outer one. This often leads to bugs:

```cpp
int credits{60};
if (true) {
    int credits{30}; // shadows outer variable
    // ...
}
```

Prefer unique names or restructure logic to avoid shadowing entirely.

## Putting it together: student summary

```cpp
#include <iomanip>
#include <iostream>
#include <string>

int main() {
    const std::string programName{"Aviation Management"};
    int completedCredits{};
    double attendanceRate{0.0};

    std::cout << "Enter completed credits: ";
    std::cin >> completedCredits;

    std::cout << "Enter attendance rate (0-1): ";
    std::cin >> attendanceRate;

    constexpr int degreeCredits{180};
    const int remainingCredits = degreeCredits - completedCredits;

    std::cout << '\n';
    std::cout << "Programme: " << programName << '\n';
    std::cout << "Completed: " << completedCredits << " credits" << '\n';
    std::cout << "Remaining: " << remainingCredits << " credits" << '\n';
    std::cout << std::fixed << std::setprecision(1);
    std::cout << "Attendance: " << attendanceRate * 100 << "%" << '\n';
}
```

Notice how constants, initialization, and type deduction combine to keep the code expressive and safe.

## Common pitfalls

- Forgetting to initialize fundamental types, resulting in unpredictable values.
- Using `auto` with brace initialization (`auto value{1};` deduces `int`, but `auto value{1, 2}` is invalid).
- Assuming `char` is signed or unsigned—it is implementation-defined. When storing raw bytes, prefer `std::uint8_t`.
- Mixing `float` and `double` in arithmetic; the compiler promotes operands but may lose precision if the result is assigned to `float`.
- Declaring global variables to share state across functions. Prefer passing parameters or using structures/classes.

## Practice time

1. **Budget snapshot:** Declare constants for tuition per credit and total credits. Read completed credits and compute remaining cost using `double`. Use brace initialization everywhere.
2. **Sensor log:** Store a timestamp (`std::string`), temperature (`double`), and error flag (`bool`). Print a single formatted line describing the reading.
3. **Type deduction drill:** Create three variables with `auto` whose deduced types are `int`, `double`, and `std::string`. Print the `typeid(...).name()` to confirm (requires `<typeinfo>`). Reflect on readability.
4. **Structured binding:** Simulate a function returning `std::tuple<std::string, int, double>` and unpack it into named variables with `auto [name, credits, gpa]`.

## Self-check questions

1. Why is brace initialization preferred over the `=` syntax for new code?
2. What is the difference between `const` and `constexpr`?
3. When does `auto` deduce a reference type?
4. How can you detect and prevent narrowing conversions at compile time?
5. What advantages do structured bindings offer compared to manually accessing `pair.first` and `pair.second`?

Answer these before moving forward to deepen your comfort with C++'s type system.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Variables & Types (Appendix)

Brief RAII examples and an HTML table comparing storage durations.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Storage</th><th>Duration</th><th>When</th></tr>
  </thead>
  <tbody>
    <tr><td>Automatic</td><td>Scope</td><td>Local RAII</td></tr>
    <tr><td>Static</td><td>Program lifetime</td><td>Config caches</td></tr>
    <tr><td>Heap</td><td>Manual</td><td>Large buffers</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix)

1. Write an RAII wrapper for a raw resource and test scope-based release.
2. Measure copy vs move for a large object type.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Types & Safety — Deep Dive (Appendix II — External Links)

Guidance for choosing integer types, using fixed-width types, and documenting overflow behavior.

```cpp
#include <cstdint>
#include <limits>
#include <iostream>

int main() {
    std::int64_t a = std::numeric_limits<std::int64_t>::max();
    std::cout << "Max int64: " << a << '\n';
}
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Link</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Fixed-width types</td><td><a href="https://en.cppreference.com/w/cpp/types/integer">integer types</a></td><td>Use for precise sizes</td></tr>
    <tr><td>Overflow</td><td><a href="https://en.cppreference.com/w/cpp/language/operator_arithmetic">overflow rules</a></td><td>Undefined for signed overflow</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix II — variables_types)

1. Replace `int` with fixed-width types in a lesson and add tests that check expected ranges.
2. Add assertions or checks for potential overflows where inputs are untrusted.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Variables & Types — `auto`, `size_t`, and Best Practices (Appendix — variables_types_cpp-appendix2)

Notes on choosing types, using `auto` safely, integer widths, and avoiding signed/unsigned surprises.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Recommendation</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>`auto`</td><td>Use for long types</td><td>Prefer when it improves readability</td></tr>
    <tr><td>`size_t`</td><td>Container sizes/indices</td><td>Unsigned — be careful with arithmetic</td></tr>
    <tr><td>Fixed-width types</td><td>`int32_t`, `uint64_t`</td><td>Use in ABI or protocol code</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```cpp
auto it = vec.begin();
size_t n = vec.size();
int64_t counter = 0;
```

### Testing type-sensitive code

- Test boundary conditions (max/min) for integer code paths.
- Use static assertions where applicable: `static_assert(sizeof(long) >= 8, "need 64-bit");`

### Exercises (Appendix — variables_types_cpp-appendix2)

1. Write a function that accepts a container and returns the sum using `auto` for the iterator and test it with `vector<int>` and `list<long>`.
2. Demonstrate a signed/unsigned bug with a small test and fix it by changing types or using `std::make_signed`/`std::make_unsigned` appropriately.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Variables & Types — auto, decltype & Aliases (Appendix — variables_types_cpp-appendix-20251005-01)

Practical examples using modern C++ type features and guidance for choosing value vs reference semantics.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>auto</td><td>Type deduction</td><td>Use to avoid redundancy, but keep clarity</td></tr>
    <tr><td>decltype</td><td>Obtain type of expression</td><td>Useful in templates and generic code</td></tr>
    <tr><td>using</td><td>Type aliases</td><td>Prefer `using Name = T;` over typedef</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: auto and decltype

```cpp
auto v = std::vector<int>{1,2,3};
using Vec = decltype(v);
Vec v2 = v;
```

### Exercises (Appendix — variables_types_cpp-appendix-20251005-01)

1. Refactor a legacy function signature using `auto` and `using` aliases to improve readability.
2. Add tests (static_assert) demonstrating deduced types match expected types.

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
- Official language documentation (search for "official <LANG> docs" where <LANG> is the lesson's language).
- Standard library reference and API pages.
- For curriculum authors: keep examples minimal and include runnable snippets in fenced code blocks.

*End of Practical Appendix.*
