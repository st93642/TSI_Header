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

<!-- markdownlint-disable MD033 MD010 -->

## Header Best Practices & Tooling Appendix

This appendix collects advanced patterns and practical tooling to keep C++ headers maintainable at scale: the PIMPL idiom, include-what-you-use practices, modules intro, header-hygiene CI checks, and helpful scripts.

### PIMPL (Pointer to IMPL) Idiom

PIMPL hides implementation details and reduces compile-time coupling by moving private members into a separate implementation struct. It reduces header churn when members change.

```cpp
// widget.h
#ifndef PROJECT_WIDGET_H
#define PROJECT_WIDGET_H

#include <memory>

class Widget {
public:
  Widget();
  ~Widget();
  void draw();
private:
  struct Impl;
  std::unique_ptr<Impl> pimpl;
};

#endif // PROJECT_WIDGET_H
```

```cpp
// widget.cpp
#include "widget.h"
#include "widget_impl.h" // defines struct Widget::Impl

struct Widget::Impl { /* private fields */ };
Widget::Widget() : pimpl(std::make_unique<Impl>()) {}
Widget::~Widget() = default;
void Widget::draw() { /* forward to pimpl */ }
```

Benefits:

- Changes to impl do not require recompiling clients.
- Improves ABI stability for libraries.

Tradeoffs:

- Slight runtime and code complexity cost (indirection, heap allocation).

### Include-What-You-Use (IWYU) Practices

Always include headers you directly depend on. Use IWYU tools to analyze and suggest missing or redundant includes.

Sample CI step (conceptual) that runs IWYU and fails on regressions:

```yaml
name: iwyu-check
on: [push, pull_request]
jobs:
  iwyu:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install dep
        run: sudo apt-get update && sudo apt-get install -y include-what-you-use
      - name: Run IWYU
        run: |
          mkdir build && cd build
          cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
          run_iwyu.py -p . ../src | tee iwyu.out
          if grep -q "should add these lines:" iwyu.out; then exit 1; fi
```

### C++ Modules (brief intro)

C++20 modules aim to improve compile times and encapsulation compared to traditional headers. Trade-offs include toolchain availability and migration complexity.

```cpp
// math.ixx (module interface)
export module math;
export int add(int a, int b);
```

```cpp
// math.cppm (module implementation)
module math;
int add(int a, int b) { return a + b; }
```

Pros:

- Faster builds by avoiding textual inclusion.
- Stronger encapsulation of implementation.

Cons:

- Tooling and ecosystem support still maturing in many environments.

### Header Hygiene: CI Checks & Scripts

Small scripts and CI checks can catch common header problems: missing include guards, public headers that pull private headers, or headers that include too many transitive dependencies.

Header-check script (simple example):

```bash
#!/usr/bin/env bash
# header-hygiene.sh: check for pragma once or include guards and basic IWYU hints
set -euo pipefail
fail=0
for h in $(git ls-files '*.h' '*.hpp' '*.hxx'); do
  if ! grep -q "#pragma once\|#ifndef" "$h"; then
    echo "Missing include guard or pragma once: $h"
    fail=1
  fi
done
exit $fail
```

Automate this in CI as a pre-merge gate and pair it with a lighter IWYU scan to suggest improvements.

### Header Reference Table (HTML)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Issue</th><th>Check</th><th>Action</th></tr>
  </thead>
  <tbody>
    <tr><td>Missing include guard</td><td>header-hygiene.sh</td><td>Add `#pragma once` or guard</td></tr>
    <tr><td>Unnecessary transitive include</td><td>include-what-you-use</td><td>Remove and include direct dependency</td></tr>
    <tr><td>Large header compile cost</td><td>build timing profile</td><td>Split header, use forward declaration or PIMPL</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Practical: Minimal IWYU helper (python)

```python
#!/usr/bin/env python3
# scan-headers.py: naive scan for direct includes vs usage (toy example)
import re,sys
for path in sys.argv[1:]:
    text=open(path).read()
    includes=re.findall(r'\\#include \"([^\"]+)\"', text)
    uses=re.findall(r'\bstd::\w+\b', text)
    print(path, 'includes=', includes, 'uses=', list(set(uses)))
```

### Include Order Enforcement

Enforce a style that puts the corresponding header first; this ensures the header is self-contained. CI can run a quick compile of each header in isolation using the compile_commands.json produced by CMake.

### Header-only Libraries & Inline Costs

For small utilities, header-only libraries reduce friction. For large libraries, prefer compiled libraries with stable headers to limit recompiles.

### HTML: Public vs Private Header Layout

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Location</th><th>Purpose</th><th>Visibility</th></tr>
  </thead>
  <tbody>
    <tr><td>include/project/foo.h</td><td>Public API</td><td>Installed to SDK</td></tr>
    <tr><td>src/foo_impl.h</td><td>Private helpers</td><td>Not installed</td></tr>
    <tr><td>tests/foo_test.cpp</td><td>Unit tests referencing public header</td><td>Test-only</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Module & ABI guidance

- When producing libraries for distribution, maintain a stable header ABI. Use PIMPL for private data and document any ABI-affecting changes.
- Keep exported headers minimal; do not expose private headers in the installed include path.

### Quick CI snippet: compile headers in isolation (CMake)

```bash
mkdir -p build && cd build
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
# compile each header's translation unit if supported
python3 ../scripts/compile_headers.py --compile-commands compile_commands.json
```

### Exercises: Header Best Practices (unique)

1. Convert a small module to use PIMPL for private members and measure rebuild times before/after a private change.
2. Add `header-hygiene.sh` to CI as a pre-merge gate and create an exemption process for legacy headers that require gradual migration.
3. Try migrating a small part of a project to C++ modules and record build time differences on your CI runner.

---

End of Header Best Practices & Tooling Appendix.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Header Basics — Guards, PIMPL & Minimal Includes (Appendix — header_basics_cpp-appendix2)

Practical advice for writing headers: minimize includes, prefer forward declarations, and use PIMPL for ABI stability.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Include minimal</td><td>Use forward declarations</td><td>Reduces compile-time coupling</td></tr>
    <tr><td>PIMPL</td><td>Pointer to impl</td><td>Helps hide implementation and reduce recompiles</td></tr>
    <tr><td>Guards</td><td>#pragma once / include guards</td><td>Prevent multiple inclusion</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: forward decl

```cpp
// header.h
class Widget; // forward
void operate(Widget* w);
```

### Exercises (Appendix — header_basics_cpp-appendix2)

1. Refactor a header to replace heavy includes with forward declarations and measure compile time difference for a small project.
2. Implement a simple PIMPL for a class with private data and test compilation and linking.

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
