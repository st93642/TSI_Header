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

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Conditionals — Deep Dive (Appendix II — External Links)

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
