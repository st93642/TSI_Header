# Lesson 1.3: Input and Output

Programs become useful when they interact with the outside world. This lesson teaches you how to read data from the keyboard, display results cleanly, and control formatting so your output matches automated tests in both C++ and C.

## Learning Goals

- Use `std::cout`, `std::cin`, and other C++ stream objects effectively.
- Format output with manipulators (`std::setw`, `std::fixed`, `std::setprecision`).
- Read entire lines of text with `std::getline` and mix it safely with formatted extraction.
- Work with C's `printf`/`scanf` family and understand how format specifiers map to variable types.
- Handle whitespace, validation, and error states gracefully.

## 1. Console Output in C++

`std::cout` is an output stream that sends data to standard output (usually the terminal window).

```cpp
#include <iostream>
#include <iomanip>

int main() {
    double subtotal = 125.456;
    double taxRate = 0.21;
    double total = subtotal * (1.0 + taxRate);

    std::cout << "Subtotal: " << subtotal << '\n';
    std::cout << "Tax Rate: " << taxRate * 100 << "%\n";
    std::cout << std::fixed << std::setprecision(2);
    std::cout << "Total: " << total << '\n';

    return 0;
}
```

Key ideas:

- Chain multiple `<<` operations in a single statement.
- Manipulators such as `std::fixed`, `std::setprecision`, and `std::setw` change formatting state and remain active until you modify it again.
- Prefer `"\n"` for new lines; `std::endl` also flushes the stream, which is slower inside loops.

## 2. Console Input in C++

`std::cin` reads formatted data—integers, doubles, strings—from standard input.

```cpp
int quantity{};
double unitPrice{};

std::cout << "Enter quantity and unit price: ";
std::cin >> quantity >> unitPrice; // extraction skips leading whitespace

double lineTotal = quantity * unitPrice;
```

When mixing formatted extraction with line-based input, guard against leftover newline characters:

```cpp
std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
std::string name;
std::getline(std::cin, name);
```

Always check the stream state before trusting the data:

```cpp
if (!std::cin) {
    std::cerr << "Input error. Please run again." << std::endl;
    return 1;
}
```

## 3. Reading and Writing Strings

- `std::getline` reads entire lines, including spaces.
- Use `std::ws` manipulator to consume leading whitespace before a `getline` when mixing input styles.
- Streams automatically stop at whitespace when reading into `std::string` with `>>`.

```cpp
std::cout << "Enter your first and last name: ";
std::string fullName;
std::getline(std::cin >> std::ws, fullName);
```

## 4. Input/Output in C

C uses the `<stdio.h>` library with format specifiers.

```c
#include <stdio.h>

int main(void) {
    int quantity = 0;
    double unitPrice = 0.0;

    printf("Enter quantity and unit price: ");
    if (scanf("%d %lf", &quantity, &unitPrice) != 2) {
        fprintf(stderr, "Invalid input.\n");
        return 1;
    }

    double lineTotal = quantity * unitPrice;
    printf("Line Total: %.2f\n", lineTotal);

    return 0;
}
```

Important details:

- Always pass **addresses** to `scanf`: `&variable`.
- `%d`, `%lf`, `%s`, `%c` must match the type exactly. Using the wrong specifier invokes undefined behavior.
- `scanf` returns the number of successful conversions—check it to validate input.

### Reading Strings in C

```c
char buffer[64];
printf("Enter your name: ");
if (fgets(buffer, sizeof buffer, stdin)) {
    // remove trailing newline if present
    buffer[strcspn(buffer, "\n")] = '\0';
    printf("Hello, %s!\n", buffer);
}
```

`fgets` reads up to `sizeof(buffer) - 1` characters and terminates with `\0`. It includes the newline when the user presses Enter, so remove it manually.

## 5. Coordinating Input and Output

Regardless of language:

- Prompt users clearly before reading input.
- Validate data and handle errors before performing calculations.
- Store intermediate results in well-named variables for readability.
- Keep output formatting consistent so automated tests match exactly.
- Separate computation from I/O in helper functions as your programs grow.

## 6. Common Pitfalls

- **Forgotten newline consumption**: after `std::cin >> number`, the trailing newline stays in the buffer. Call `std::cin.ignore()` before `std::getline`.
- **Mismatched format strings**: `scanf` with the wrong specifier corrupts memory. Double-check `%d`, `%lf`, `%s`, `%c`, etc.
- **Unchecked stream state**: if extraction fails, the stream enters a fail state and all subsequent reads silently fail. Call `std::cin.clear()` and discard invalid input if you want to retry.
- **Locale differences**: decimal separators vary. The default "C" locale expects a dot (`.`). If user input includes commas, either inform the user or imbue the stream with the appropriate locale.

## 7. Practice Ideas

1. Build a shipping calculator that reads weight and destination, then prints a formatted receipt with aligned columns (hint: `std::setw`).
2. Prompt the user for their full name and favorite quote; print both lines in a friendly message. Repeat the task in C and handle newline removal with `fgets`.
3. Implement a simple error-recovery loop: keep asking for an integer until the user enters valid data. Use `std::cin.clear()` and `std::cin.ignore()` to recover from bad input.

Launch the Lesson 1.3 exercise when you are ready. The automated tests will expect:

- Accurate reading of the required values (no hard-coded numbers).
- Output formatted exactly as described in the starter comments (including spaces, punctuation, and newline placement).
- Clean handling of both C++ stream I/O and C formatted I/O.
