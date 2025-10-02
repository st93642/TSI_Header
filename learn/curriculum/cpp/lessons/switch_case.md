# Lesson 2.2: Switch Statements

`if`/`else-if` chains are great for combining a handful of precise checks, but sometimes you need to branch on one value across many specific cases. In those scenarios, `switch` gives you a compact, readable alternative that avoids repetitive comparisons.

## What You'll Learn

- The anatomy of a `switch` statement in modern C++
- How control flows through `case`, `break`, and `default` labels
- When fallthrough is useful (and when it is a bug)
- Pattern suggestions for enumerations and character menus
- How C++17 features like `switch` initializers mirror `if` initializers
- Differences you should remember when comparing C and C++ usage

## 1. Basic Structure

```cpp
char grade = 'B';

switch (grade) {
    case 'A':
        std::cout << "Outstanding" << std::endl;
        break;
    case 'B':
        std::cout << "Great job" << std::endl;
        break;
    case 'C':
        std::cout << "Keep practicing" << std::endl;
        break;
    default:
        std::cout << "Grade unavailable" << std::endl;
        break;
}
```

Key rules:

- The expression after `switch` is evaluated once. It must be an integral, enumeration, or `std::byte` value (C++20 adds more options).
- Each `case` label must be a constant expression.
- `break` exits the `switch`. Without it, execution “falls through” to the next case.
- `default` handles any values not matched by earlier `case` labels.

### Decision Flow Illustration

```mermaid
flowchart TD
    A[switch(grade)] -->|A| AA[Outstanding]
    A -->|B| AB[Great job]
    A -->|C| AC[Keep practicing]
    A -->|default| AD[Grade unavailable]
```

Having a mental model like this helps you reason about which branch triggers for a given input, especially when debugging without a debugger.

## 2. Why Use `switch`?

- Clarity: a single expression with many discrete values is easier to scan than a long `else-if` ladder.
- Cheap comparisons: the compiler can generate jump tables or binary searches for faster dispatch.
- Enum-friendly: when you `switch` over an enum, compilers warn you if you miss a case (when `default` is omitted).

Use `switch` when the logic depends on one value’s exact alternatives. Stick with `if` when conditions involve ranges, inequalities, or unrelated values.

### Comparing `if` vs. `switch`

| Feature | `if`/`else if` | `switch` |
|---------|----------------|----------|
| Supports ranges (`x > 10`) | ✅ | ❌ |
| Supports enums/integers | ✅ | ✅ |
| Compiler can warn about missing enum cases | ⚠️ limited | ✅ when no `default` |
| Fallthrough control | Manual via nested `if` | Built-in (requires careful `break` use) |
| Pattern matching on strings | ✅ (with `std::string`) | ❌ (use helper map instead) |

## 3. Fallthrough (Intentional and Accidental)

```cpp
int month = 6;
int days = 31;

switch (month) {
    case 4: case 6: case 9: case 11:
        days = 30;
        break;
    case 2:
        days = 28; // ignoring leap years for now
        break;
    default:
        days = 31;
        break;
}
```

Combining cases on one line is the safest way to share behavior without accidental fallthrough. If you truly intend to fall through in modern C++, add the `[[fallthrough]];` attribute to make intent explicit and silence warnings:

```cpp
switch (command) {
    case 'h':
        printHelp();
        [[fallthrough]]; // continue into default to show prompt
    default:
        showPrompt();
        break;
}
```

## 4. Switch with Initializer (C++17)

Just like `if`, C++17 lets you initialize a helper variable right in the switch header. The variable’s scope is limited to the switch body.

```cpp
switch (int remainder = value % 3; remainder) {
    case 0:
        std::cout << value << " is divisible by 3" << std::endl;
        break;
    case 1:
        std::cout << "Remainder is 1" << std::endl;
        break;
    default:
        std::cout << "Remainder is 2" << std::endl;
        break;
}
```

This keeps temporary values localized and prevents reuse errors in later code. (Classic C does not support this syntax.)

### Enumerations + Switch

Scoped enumerations provide type safety for category-based switches:

```cpp
enum class MenuOption { New, Load, Save, Quit };

auto handle(MenuOption option) -> void {
    switch (option) {
        case MenuOption::New:
            startNewGame();
            break;
        case MenuOption::Load:
            loadGame();
            break;
        case MenuOption::Save:
            saveGame();
            break;
        case MenuOption::Quit:
            exitPrompt();
            break;
    }
}
```

Leaving out the `default` branch allows the compiler to warn you when a new enumerator is added but not handled.

## 5. Modern C++ Tips

- **Prefer enums over raw integers** for domain-specific categories. Scoped enums (`enum class`) keep names out of the global scope and force explicit conversions when needed.
- **Always cover unexpected values**. A `default` branch is ideal for logging errors or handling future additions.
- **Declare variables inside cases carefully**. Introduce braces to control lifetime and prevent cross-initialization errors:

  ```cpp
  switch (option) {
      case 'n': {
          std::string name;
          std::cin >> name;
          processName(name);
          break;
      }
      case 'q':
          quit();
          break;
      default:
          std::cout << "Unknown option" << std::endl;
          break;
  }
  ```

- **Avoid long logic per case**. If a branch grows, extract a helper function to keep the switch readable.

## 6. C vs. C++ Differences

Both languages share the same syntax, but C++ adds conveniences:

- `switch` initializers and scoped enums improve safety.
- `std::cout`/`std::cin` integrate with C++ types, whereas C uses `printf`/`scanf` and manual format specifiers.
- C++17’s `[[fallthrough]]` attribute intentionally documents fallthrough.

Example in C:

```c
int menu = read_menu_choice();

switch (menu) {
    case 1:
        printf("Starting new game\n");
        break;
    case 2:
        printf("Loading save\n");
        break;
    default:
        printf("Unknown entry\n");
        break;
}
```

### Handling Strings or Complex Keys

`switch` cannot operate on strings directly. Use a lookup table or hash map when your cases depend on text input:

```cpp
const std::unordered_map<std::string, MenuOption> actions {
    {"new",  MenuOption::New},
    {"load", MenuOption::Load},
    {"save", MenuOption::Save},
    {"quit", MenuOption::Quit},
};

if (auto it = actions.find(command); it != actions.end()) {
    handle(it->second);
} else {
    std::cout << "Unknown command\n";
}
```

This pattern keeps your switch logic for enums while still accepting flexible user input.

## 7. Common Pitfalls

- **Missing breaks** cause unintended fallthrough. Compilers normally warn you—address every warning.
- **Non-constant labels** are invalid. Wrap calculations in `constexpr` variables or compute before the switch.
- **Switching on floating-point values** is not allowed in standard C++—convert to an integral representation first.
- **Complex conditions** still belong in `if`/`else` logic.

## Practice Ideas

1. Build a menu that accepts the characters `n`, `l`, `s`, and `q` and prints matching descriptions.
2. Convert integer weekday numbers (1-7) to text using `switch`; include a default branch for invalid inputs.
3. Combine a `switch` initializer with `value % 5` to print custom messages for remainders.

When you are ready, start the practice exercise. It asks you to classify status codes using a `switch` statement with well-ordered cases, fallthrough awareness, and explicit defaults. Keep the output format identical to the blueprint, and lean on scoped enumerations or constants if you extend it later.

### Build & Run Checklist

```bash
g++ -std=c++17 -Wall -Wextra -pedantic switch_demo.cpp -o switch_demo
./switch_demo
```

Always compile with warnings enabled so the compiler alerts you to missing cases or unreachable branches.

## References

- cppreference.com: [Switch statement](https://en.cppreference.com/w/cpp/language/switch)
- ISO C++ Core Guidelines: Enum + switch recommendations (ES.74, ES.78)
- C Standard Library reference: [`switch` statement](https://en.cppreference.com/w/c/language/switch)
