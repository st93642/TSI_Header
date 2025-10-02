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

## 2. Why Use `switch`?

- Clarity: a single expression with many discrete values is easier to scan than a long `else-if` ladder.
- Cheap comparisons: the compiler can generate jump tables or binary searches for faster dispatch.
- Enum-friendly: when you `switch` over an enum, compilers warn you if you miss a case (when `default` is omitted).

Use `switch` when the logic depends on one value’s exact alternatives. Stick with `if` when conditions involve ranges, inequalities, or unrelated values.

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

## References

- *Beginning C++17*, Chapter 4 “Making Decisions” (sections on switch statements and fallthrough)
- cppreference.com: [Switch statement](https://en.cppreference.com/w/cpp/language/switch)
- ISO C++ Core Guidelines: Enum + switch recommendations (ES.74, ES.78)
