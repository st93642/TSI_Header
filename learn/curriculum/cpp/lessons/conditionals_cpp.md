# Making Decisions with Conditionals

Every interactive program relies on branching: evaluating a condition and picking one path over another. In this extended lesson you will master `if`, `else if`, `else`, and `switch`, learn how boolean expressions work under the hood, and spot the patterns that keep decision-heavy code readable and bug-free.

## Learning goals

- Craft clear boolean expressions using relational and logical operators.
- Structure multi-branch decisions with `if`/`else if` chains and guard clauses.
- Choose between nested `if` statements and `switch` depending on the data shape.
- Understand truthiness, short-circuit evaluation, and common comparison pitfalls.
- Validate user input before branching so business rules stay predictable.

## Anatomy of an `if` statement

```cpp
#include <iostream>

int main() {
    int score{};
    std::cout << "Enter score: ";
    if (!(std::cin >> score)) {
        std::cerr << "Input must be numeric.\n";
        return 1;
    }

    if (score >= 90) {
        std::cout << "Excellent!\n";
    } else {
        std::cout << "Keep practising.\n";
    }
}
```

- The condition inside parentheses evaluates to `true` or `false`.
- Only the first block whose condition evaluates to `true` is executed.
- Braces are recommended even for single statements to avoid accidental dangling `else` bugs.

### Guard clauses and early returns

Instead of deep nesting, validate assumptions up front and exit early when something is wrong:

```cpp
if (score < 0 || score > 100) {
    std::cerr << "Score must be within 0-100.\n";
    return 1;
}
```

This keeps the main happy-path logic close to the left margin and easier to read.

## Cascading branches and ordering

`else if` chains should be ordered from most restrictive to least restrictive condition. Conditions are evaluated top to bottom and stop once a match is found.

```cpp
if (score >= 90) {
    grade = 'A';
} else if (score >= 80) {
    grade = 'B';
} else if (score >= 70) {
    grade = 'C';
} else if (score >= 60) {
    grade = 'D';
} else {
    grade = 'F';
}
```

- Avoid overlapping ranges; otherwise earlier clauses steal matches from later ones.
- If two conditions share the same outcome, consider consolidating them to simplify the chain.

### Nested vs. flat decisions

Nested `if` statements are sometimes necessary, but excessive nesting reduces readability. Look for ways to flatten the logic using compound conditions.

```cpp
if (isMember && purchaseTotal >= 100.0) {
    applyDiscount();
}
```

Instead of nesting `if (isMember) { if (purchaseTotal >= 100.0) { ... } }` combine them with logical operators in a single expression.

## Building boolean expressions

- `==` (equality): Compare values, not floating-point approximations without tolerance.
- `!=` (inequality): True when operands differ.
- `<`, `<=`, `>`, `>=` (relational): Work with numbers and any types that define ordering semantics.
- `&&` (logical AND): Short-circuits; the right-hand side is evaluated only if the left side is true.
- `||` (logical OR): Short-circuits; the right-hand side is evaluated only if the left side is false.
- `!` (logical NOT): Flips a boolean.

Short-circuiting lets you safely combine checks, such as verifying the denominator before performing division:

```cpp
if (denominator != 0 && numerator % denominator == 0) {
    std::cout << "Divides evenly!\n";
}
```

## Decision tables with `switch`

`switch` statements shine when you branch on discrete integral or enumeration values. They execute the matching `case` label and continue until a `break` or the end of the switch.

```cpp
enum class MenuOption { Start = 1, Settings = 2, Quit = 3 };

MenuOption option{};
int rawChoice{};
std::cin >> rawChoice;
option = static_cast<MenuOption>(rawChoice);

switch (option) {
case MenuOption::Start:
    startGame();
    break;
case MenuOption::Settings:
    openSettings();
    break;
case MenuOption::Quit:
    std::cout << "Goodbye!\n";
    break;
default:
    std::cout << "Unknown option.\n";
    break;
}
```

- Always provide a `default` to handle unexpected inputs.
- `switch` works on integral types, scoped enums, and unscoped enums; it does not work on strings directly.
- Omit `break` only when deliberately falling through, and document it with comments or the C++23 `[[fallthrough]]` attribute.

## Advanced branching techniques

- **Ternary operator**: `condition ? trueValue : falseValue` is perfect for inline expressions.
- **`if constexpr`** (C++17): compile-time branching in templates.
- **`std::optional` checks**: guard whether optional values contain data before use.

## Common pitfalls

- Assigning instead of comparing: `if (x = 5)` assigns and evaluates to `true`. Use `==` for comparisons.
- Comparing floating-point values with `==`; use an epsilon tolerance instead.
- Forgetting braces around multi-line blocks, leading to unexpected execution.
- Ignoring user input validation, causing stale values from previous reads to leak into logic.

## Worked example: grading rubric

```cpp
#include <iostream>
#include <string>

int main() {
    int score{};
    std::cout << "Enter score (0-100): ";
    if (!(std::cin >> score)) {
        std::cerr << "Non-numeric score.\n";
        return 1;
    }

    if (score < 0 || score > 100) {
        std::cout << "Result: Invalid score\n";
        return 0;
    }

    std::string band{};
    if (score >= 90) {
        band = "Outstanding";
    } else if (score >= 75) {
        band = "Great";
    } else if (score >= 60) {
        band = "Satisfactory";
    } else {
        band = "Needs improvement";
    }

    std::cout << "Result: " << band << '\n';
}
```

This routine validates input, branches by range, and returns a descriptive outcome.

## Practice time

1. **Weather advisory:** Ask for temperature and precipitation chance. Recommend `"Wear a coat"`, `"Bring an umbrella"`, `"Sunny skies"`, or `"Stay inside"` depending on thresholds you define.
2. **Membership perks:** Read a customer's loyalty tier (`bronze`, `silver`, `gold`, `platinum`) and use either cascading `if` or `switch` (with an enum) to print perks unlocked at each tier.
3. **Shipping estimator:** Use guard clauses to reject invalid package weights, then branch on weight bands to compute shipping costs.
4. **Ternary formatting:** Output `"Pass"` or `"Fail"` inline using the ternary operator on a boolean flag.

## Self-check questions

1. When does short-circuit evaluation prevent a crash?
2. How do you handle overlapping ranges in an `if`/`else if` chain?
3. Why is falling through a `switch` without `break` risky, and how can you signal intentional fallthrough?
4. What are the pros and cons of using the ternary operator for readability?
5. How would you compare two doubles for equality while accounting for floating-point precision errors?

Tackle the exercise once you can answer these confidently—you will combine conditionals with previous I/O and arithmetic skills.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Conditionals — Resources & Patterns (Appendix — External Links)

Authoritative references and patterns for conditional logic, pattern matching (C++23), and best practices.

- cppreference: [cppreference — if statement](https://en.cppreference.com/w/cpp/language/if)
- C++23 pattern matching notes: [cppreference — pattern matching](https://en.cppreference.com/w/cpp/language/pattern_match) (when available)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Link</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>if/else</td><td><a href="https://en.cppreference.com/w/cpp/language/if">cppreference if</a></td><td>Prefer clear branching</td></tr>
    <tr><td>constexpr if</td><td><a href="https://en.cppreference.com/w/cpp/language/if#constexpr_if">constexpr if</a></td><td>Use for compile-time dispatch</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Replace a chain of `if/else` that tests types with `constexpr if` or tag dispatch and add compile-time tests.
2. Document when pattern matching (C++23) simplifies nested conditionals.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Conditionals — Deep Dive (Appendix II — External Links)

Show more about `constexpr if`, type traits, and when to prefer tag-dispatch or concept-based overloads.

```cpp
#include <type_traits>
#include <iostream>

template <typename T>
void print_if_integer(const T& v) {
    if constexpr (std::is_integral_v<T>) {
        std::cout << "integer: " << v << '\n';
    } else {
        std::cout << "not integer\n";
    }
}
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Link</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>constexpr if</td><td><a href="https://en.cppreference.com/w/cpp/language/if">if (constexpr)</a></td><td>Compile-time branching</td></tr>
    <tr><td>Type traits</td><td><a href="https://en.cppreference.com/w/cpp/types/type_traits">type_traits</a></td><td>Use to inspect types at compile time</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix II — conditionals)

1. Rework a function to use `if constexpr` to select behavior based on type.
2. Add a small set of static_asserts that validate template branches.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Conditionals — Patterns & Tests (Appendix — conditionals_cpp-appendix2)

Common conditional patterns, avoiding nested if-else complexity, and small unit-testable examples.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>Why</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr><td>Early return</td><td>Reduce nesting</td><td>if (!ok) return;</td></tr>
    <tr><td>Use enums</td><td>Make states explicit</td><td>enum class State { Pending, Ready }</td></tr>
    <tr><td>Branch coverage</td><td>Test all paths</td><td>Use unit tests for each branch</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: early returns & enums

```cpp
enum class Status { OK, ERROR };

Status check(int x) {
    if (x < 0) return Status::ERROR;
    if (x == 0) return Status::OK;
    return Status::OK;
}
```

### Testing tips

- Write unit tests that explicitly exercise each conditional path.
- Use parameterized tests for combinations of inputs that affect multiple branches.

### Exercises (Appendix — conditionals_cpp-appendix2)

1. Refactor a nested-if example into early-return style and add tests for each path.
2. Replace magic numbers in conditionals with named constants or enums and add static_asserts where appropriate.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Conditionals — switch, constexpr & Pattern Dispatch (Appendix — conditionals_cpp-appendix3)

Practical tips for writing clear conditionals in C++: prefer `switch` for integer enums, use `constexpr` for compile-time branches, and emulate pattern dispatch with variant/visit.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Technique</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>switch</td><td>Enums/ints</td><td>Use `default` and avoid fall-through unless intended</td></tr>
    <tr><td>constexpr if</td><td>Compile-time</td><td>Use for template dispatch</td></tr>
    <tr><td>std::variant</td><td>Sum types</td><td>Use `std::visit` for type-based dispatch</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: constexpr-if

```cpp
template<typename T>
void f(T t) {
  if constexpr (std::is_integral_v<T>) {
    // integer-specific
  } else {
    // other
  }
}
```

### Exercises (Appendix — conditionals_cpp-appendix3)

1. Replace a chain of `if-else` on an enum with a `switch` statement and add tests covering all values.
2. Use `std::variant` and `std::visit` to dispatch behavior for multiple possible value types and test each case.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Testing Conditionals — Property Tests & Edge Cases (Appendix — conditionals_cpp-appendix4)

Practical guidance for testing branches: use property-based approaches and enumerate edge cases to avoid logic regression.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Strategy</th><th>Tooling</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Enumerate enums</td><td>Unit tests</td><td>Cover every enum value including default</td></tr>
    <tr><td>Property tests</td><td>Smallcheck-style</td><td>Use randomized inputs to find invariants</td></tr>
    <tr><td>Boundary tests</td><td>Edge values</td><td>Focus on off-by-one</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: table-driven tests

```cpp
struct Case { int in; int expected; };
Case cases[] = {{0,0}, {1,1}, {2,1}};
for (auto &c : cases) EXPECT_EQ(f(c.in), c.expected);
```

### Exercises (Appendix — conditionals_cpp-appendix4)

1. Add table-driven tests enumerating all enum values and a failing case, then fix the implementation.
2. Create a small randomized tester that asserts invariants over hundreds of generated inputs.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
