# Header Basics and Organisation

As projects expand, keeping declarations and definitions organised becomes vital for fast builds and readable code. This lesson explores header design, guards, separation between interface and implementation, and conventions that scale to real-world codebases.

## Learning goals

- Distinguish declarations from definitions and know where each belongs.
- Protect headers against multiple inclusion with guards or `#pragma once`.
- Structure folders so headers expose a stable interface while source files implement details.
- Understand forward declarations, compilation units, and include order.
- Apply documentation and naming conventions that make APIs discoverable.

## Declarations vs. definitions

- **Declarations** announce the existence of symbols—functions, classes, variables—so other translation units can reference them.
- **Definitions** allocate storage or provide implementation.

Headers generally contain declarations, inline definitions, and documentation. Source (`.cpp`) files provide the full implementations.

```cpp
// math_utils.h
int add(int a, int b);      // declaration

// math_utils.cpp
int add(int a, int b) {     // definition
    return a + b;
}
```

## Include guards and `#pragma once`

Prevent multiple inclusion with either traditional guards or the single-line `#pragma once` directive. Guards work with every compiler and reveal the include path when named thoughtfully.

```cpp
#ifndef MATH_UTILS_H
#define MATH_UTILS_H

int add(int a, int b);
double scale(double value, double factor);

#endif // MATH_UTILS_H
```

Tips:

- Choose guard names that reflect the path (`PROJECT_MODULE_MATH_UTILS_H`).
- Do not reuse the same guard in different headers.
- Place includes and declarations between the `#define` and the closing `#endif`.

## File organisation patterns

Common layout for a library or feature:

- `include/finance/currency.h`: public declarations.
- `src/currency.cpp`: implementations.
- `tests/currency_test.cpp`: unit tests including the header.

Organise headers by responsibility. group related declarations into a single header instead of scattering them across many files with one function each.

### Include order

Adopt a consistent include order to catch missing dependencies:

1. The matching header (e.g., `#include "currency.h"`).
2. Other project headers.
3. System or library headers.

This pattern ensures the header stands on its own—if it forgets an include, the source file will fail to compile when it includes the header first.

## Forward declarations

Forward declarations let you reference types without including the full header, reducing compile-time dependencies. They work when you only need pointers or references to a type.

```cpp
// logger.h
class Session;
void logSessionStart(const Session& session);

// logger.cpp
#include "session.h"
void logSessionStart(const Session& session) {
    // use Session implementation details here
}
```

Use forward declarations sparingly; if you need the full definition (e.g., to create an object or access members), include the header instead.

## Inline and header-only utilities

- Mark tiny helper functions `inline` so multiple source files can include the header without violating the one-definition rule.
- For templates, keep both declaration and definition in the header because the compiler needs the implementation to instantiate templates for each type used.

```cpp
template <typename T>
inline T clamp(T value, T low, T high) {
    return std::min(std::max(value, low), high);
}
```

## Documenting headers

- Place brief summaries at the top of each header describing its purpose.
- Use comment blocks above declarations to explain parameters, return values, and invariants.
- Keep headers free of using-directives (`using namespace std;`) to avoid polluting every file that includes them.

## Practice time

1. **Conversion library:** Create `conversions.h` with declarations for `double fahrenheit_to_celsius(double);` and `double kilometers_to_meters(double);`. Implement them in `conversions.cpp`, include the header, and test from `main.cpp`.
2. **Forward declaration drill:** Write `report.h` that forward declares a `struct Summary;` and declares `void print_report(const Summary&);`. Implement the function in `report.cpp` with the full struct definition.
3. **Header-only utility:** Implement a templated `inline` `clamp` function in a header and use it from two different source files to confirm the include guard works.
4. **Include audit:** Reorder includes in an existing project file to follow the "self, project, system" pattern and validate that the file still compiles.

## Self-check questions

1. What distinguishes a declaration from a definition, and why does the compiler need both?
2. When would you prefer `#pragma once` over classic include guards?
3. How does include order help catch missing dependencies early?
4. Why must template implementations live in headers?
5. When is a forward declaration appropriate, and when must you include the full header instead?

Once these concepts feel natural, tackle the exercise to practise splitting code between headers and source files.
