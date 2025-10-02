# Lesson 1.4: Constants and the Preprocessor

Constants document intent (“this value never changes”) and prevent accidental modification. This lesson covers every tool you need to define immutable values in C++ and C, shows when to use compile-time vs. run-time constants, and highlights the trade-offs between `constexpr`, `const`, and macros.

## Learning Goals

- Declare compile-time constants with `constexpr`, `consteval`, and enumerations.
- Use `const` and `#define` appropriately in C when modern C++ features are unavailable.
- Group related constants for clarity and maintainability.
- Understand how constant expressions interact with templates and array bounds.
- Avoid common pitfalls such as macro side effects and duplicate definitions.

## 1. Constants in Modern C++

```cpp
#include <iostream>
#include <iomanip>

constexpr const char* CAMPUS_NAME {"TSI Riga"};
constexpr int MAX_GROUP_SIZE {32};
constexpr double VAT_RATE {0.21};

int main() {
    std::cout << "Campus: " << CAMPUS_NAME << '\n';
    std::cout << "Max group size: " << MAX_GROUP_SIZE << '\n';

    std::cout << std::fixed << std::setprecision(2);
    std::cout << "VAT rate: " << VAT_RATE * 100 << "%\n";

    return 0;
}
```

- `constexpr` guarantees the value is usable in constant expressions (array bounds, template parameters).
- `const` indicates read-only after initialization; combine `constexpr` + `const` for clarity when the value is known at compile time.
- Use uppercase with underscores for simple global constants; reserve camelCase for function-scope constants.

### `consteval` (C++20)

`consteval` functions execute at compile time and return constant values:

```cpp
consteval int default_sessions() { return 4; }
constexpr int SESSIONS = default_sessions();
```

While we target C++17 in this course, knowing about `consteval` prepares you for future upgrades.

## 2. Enumerations for Related Constants

Scoped enumerations (`enum class`) group related integral constants with type safety.

```cpp
enum class StudyPhase { Work = 0, ShortBreak = 1, LongBreak = 2 };

constexpr StudyPhase DEFAULT_PHASE {StudyPhase::Work};
```

Use `static_cast<int>(StudyPhase::Work)` when you need the numeric value. Enumerations are ideal for menu options, statuses, or other constrained sets.

## 3. Constants in C

When writing C code, you generally combine `const` variables and preprocessor macros:

```c
#include <stdio.h>

#define CAMPUS_NAME "TSI Riga"

const int MAX_GROUP_SIZE = 32;   // still occupies storage
const double VAT_RATE = 0.21;    // cannot be reassigned

int main(void) {
    printf("Campus: %s\n", CAMPUS_NAME);
    printf("Max group size: %d\n", MAX_GROUP_SIZE);
    printf("VAT rate: %.0f%%\n", VAT_RATE * 100);
    return 0;
}
```

- `const` in C does not imply compile-time evaluation by default; compilers may still place it in read-only memory.
- `#define` simply substitutes text before compilation—no type checking!

## 4. Choosing the Right Tool

| Scenario | Recommended Approach |
|----------|---------------------|
| Value known at compile time in C++ | `constexpr` (optionally with `const`) |
| Related named values | `enum class` or `enum` |
| Value computed at runtime but never modified | `const` |
| Cross-file constant needed in multiple translation units | `inline constexpr` in C++17+, or `extern const` defined in one `.cpp` |
| Legacy C header requires macro | `#define` (keep names ALL_CAPS and wrapped in parentheses) |

## 5. Formatting Numeric Constants

- Use `<iomanip>` manipulators (`std::setprecision`, `std::setw`) to control output.
- Literal suffixes convey the intended type: `42u` (unsigned), `3.14f` (float), `1'000` (digit separators).
- Prefer `constexpr double PI {3.14159};` over magic numbers scattered throughout your code.

## 6. Avoiding Pitfalls

- **Duplicate definitions**: place `constexpr` definitions in headers with `inline` if multiple source files include them. In C, declare in a header with `extern` and define in exactly one `.c` file.
- **Macro side effects**: macros do not respect scope and evaluate parameters repeatedly. Wrap expressions in parentheses: `#define SQUARE(x) ((x) * (x))`.
- **Type confusion**: macros have no type information. Prefer typed constants whenever you can.
- **Implicit conversions**: mixing integer and floating-point constants may downcast unexpectedly. Use suffixes to make your intent explicit.

## 7. Practice Ideas

1. Create a header `constants.hpp` with `inline constexpr` definitions for campus metadata and reuse them across multiple source files.
2. Define an `enum class MenuOption { Add = 1, Remove = 2, Quit = 3 };` and use it in a `switch` statement.
3. Compare two implementations of `SQUARE(x)`—one using a macro, the other using an inline function—and note the behavior with arguments like `x++`.

When you start the Lesson 1.4 exercise you will:

- Declare campus-related constants using both C and C++ styles.
- Format floating-point output with the required precision.
- Keep the output order and labels identical to the blueprint provided in the starter comments.
