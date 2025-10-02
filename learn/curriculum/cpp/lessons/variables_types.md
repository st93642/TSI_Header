# Lesson 1.2: Variables and Data Types

Variables give your programs memory to remember intermediate results, user input, settings, and more. This lesson expands each fundamental type used in beginner-level C and C++ programs, explores how values live in memory, and shows how to initialize them safely.

## Learning Goals

- Declare variables with clear, descriptive names.
- Choose the right fundamental type for integers, floating-point numbers, characters, booleans, and text.
- Understand initialization styles (copy, direct, list) and when to prefer each.
- Recognize how C and C++ differ when working with strings and booleans.
- Avoid common pitfalls such as uninitialized memory and overflow.

## 1. What Is a Variable?

A variable is a named storage location with a type. The type controls:

- **Size** (how many bytes the value occupies)
- **Valid operations** (e.g., arithmetic vs. text manipulation)
- **Default formatting** when you print it

In C++ the general pattern for a declaration is:

```cpp
<type> <name> = <initial value>;
```

The initializer is optional, but leaving it out means the variable holds an indeterminate (garbage) value until you assign something.

## 2. Integer Types

Integers store whole numbers (no decimal point). Modern code prefers fixed-width types from `<cstdint>` when you require an exact size.

| Type | Typical Size | Range (two's complement) | Usage |
|------|---------------|---------------------------|-------|
| `int` | 4 bytes | −2,147,483,648 to 2,147,483,647 | General arithmetic |
| `long long` | 8 bytes | Very large ranges | Large counters, timestamps |
| `unsigned int` | 4 bytes | 0 to 4,294,967,295 | Quantities that cannot be negative |
| `std::int32_t` | 4 bytes | Guaranteed 32-bit | Cross-platform consistency |

Examples:

```cpp
int attendeeCount = 120;
unsigned int ticketsRemaining = 500u;  // literal suffix 'u' marks unsigned
std::int64_t totalBytes = 4'194'304;    // digit separators improve readability
```

> **Overflow alert:** If a signed integer exceeds its range, the behaviour is undefined. For counters that may grow, promote to a wider type (`long long`) or use arbitrary-precision libraries.

## 3. Floating-Point Types

Floating-point numbers store fractional values and scientific notation.

| Type | Precision | Typical Use |
|------|-----------|-------------|
| `float` | ~7 decimal digits | Graphics, embedded systems |
| `double` | ~15 decimal digits | Default choice for most calculations |
| `long double` | implementation-defined | High-precision calculations when available |

```cpp
double invoiceTotal = 149.99;
float efficiency = 0.823f;      // 'f' suffix keeps the literal as float
long double scientificValue = 6.02214076e23L;
```

When printing floating-point values, use `<iomanip>` manipulators:

```cpp
std::cout << std::fixed << std::setprecision(2) << invoiceTotal << '\n';
```

## 4. Character and Text Types

- `char` stores a single byte. Use it for characters, ASCII codes, or small integers.
- `char16_t`/`char32_t` represent UTF-16/UTF-32 code units when working with international text.

```cpp
char grade = 'A';
char newline = '\n';
```

For multi-character strings, prefer `std::string` from `<string>`:

```cpp
#include <string>

std::string courseName {"Programming Fundamentals"};
std::string welcome = "Welcome to TSI!";
```

`std::string` manages dynamic memory automatically and offers rich member functions (`size()`, `substr()`, `find()`). In C, you would instead use arrays of `char` terminated with `\0`.

## 5. Boolean Values

`bool` represents truth values. C++ prints `true`/`false` by default when you enable the `std::boolalpha` manipulator; otherwise `1`/`0` appears.

```cpp
bool isRegistered = true;
bool hasOutstandingFees = false;

std::cout << std::boolalpha << isRegistered; // prints "true"
```

In C you must `#include <stdbool.h>` to obtain the `bool` alias; internally it is an `_Bool` and still prints `1` or `0` unless you convert it to text manually.

## 6. Initialization Styles

Modern C++ offers several ways to initialize variables:

```cpp
int a = 42;        // copy initialization
int b(42);         // direct initialization
int c{42};         // list (brace) initialization – prevents narrowing
double ratio{3.0 / 7.0};
```

Brace initialization (`{}`) is preferred because it prevents narrowing conversions (e.g., assigning a `double` to an `int` silently).

### Default Initialization

```cpp
int uninitialized;      // indeterminate value – do not use before assignment
int zeroed{};           // brace init with no value sets to 0
std::string empty{};    // constructs an empty string
```

## 7. Naming Conventions and Scope

- Use descriptive names (`studentCount`, `averageScore`).
- Stick to `camelCase` or `snake_case` consistently.
- Avoid leading underscores for your own identifiers; many are reserved for the implementation.
- Variables obey scope rules: the lifetime begins at declaration and ends when the enclosing block (`{}`) finishes.

```cpp
if (bool isEligible = score >= 60; isEligible) {
    std::cout << "Student passed" << std::endl;
}
// isEligible is out of scope here
```

## 8. Working in C

The C version of the earlier example highlights what changes:

```c
#include <stdio.h>
#include <stdbool.h>

int main(void) {
    int age = 20;
    double height = 1.82;
    char grade = 'A';
    bool isStudent = true;

    printf("Age: %d\n", age);
    printf("Height: %.2f meters\n", height);
    printf("Grade: %c\n", grade);
    printf("Is student: %s\n", isStudent ? "true" : "false");

    return 0;
}
```

- C lacks `std::string`; use character arrays or pointers with care.
- `printf` format specifiers (`%d`, `%f`, `%c`) must match the variable type exactly.
- Booleans still print as integers; convert them to human-friendly text explicitly.

## 9. Common Pitfalls

- **Uninitialized values**: always assign before reading. Enable compiler warnings (`-Wall -Wextra`) to catch uses of indeterminate values.
- **Mixing signed/unsigned**: subtracting a larger unsigned number from a smaller one wraps around. Prefer signed integers unless you truly need unsigned semantics.
- **Implicit conversions**: converting large `double` values to `int` truncates them. Use `std::round`, `std::floor`, or a wider type.
- **Magic numbers**: replace unexplained constants with named variables or `constexpr` values for readability.

## 10. Practice Ideas

1. Declare variables for a student record: ID number, GPA, attendance percentage, and full name. Print them with clear labels.
2. Experiment with `std::size_t` when counting elements in a container. Note how the unsigned type interacts with subtraction.
3. Write a short program that reads two floating-point numbers, calculates their average, and prints it with two decimal places in both C and C++ versions.

When you are ready, open the Lesson 1.2 exercise. The tests will verify that you:

- Choose appropriate types for integers, floating-point values, characters, booleans, and strings.
- Initialize each variable before printing.
- Format output exactly as shown in the starter comments (spacing, casing, punctuation).
