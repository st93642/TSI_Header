# Streams and the iostream Library

Modern C++ favors the iostream family for type-safe console I/O. This expanded lesson shows how extraction and insertion work, how formatting interacts with buffering, and how to recover from bad input without restarting your program. You will move beyond print statements into full conversational programs.

## Learning goals

After studying this lesson you will be able to:

- Use `std::cout`, `std::cerr`, and `std::clog` appropriately.
- Format textual and numeric output with manipulators from `<iomanip>`.
- Read whitespace-sensitive input using both `operator>>` and `std::getline`.
- Detect and recover from stream errors (fail, eof, bad states).
- Explain how buffering and flushing influence user experience.

## A tour of standard streams

| Stream | Purpose | Typical usage |
| --- | --- | --- |
| `std::cout` | Primary output channel | User prompts, results |
| `std::cin` | Primary input channel | Reading numbers, words |
| `std::cerr` | Unbuffered error output | Immediate diagnostics |
| `std::clog` | Buffered logging | Status messages, debug info |

All four share the same formatting rules and state flags. Choosing between them is mostly about audience (user vs developer) and buffering needs.

## Writing with `std::cout`

The insertion operator (`<<`) pushes values into a stream. The operator is overloaded for built-in types and many standard library types, so the compiler knows how to convert them to text.

```cpp
#include <iostream>

int main() {
    const int x = 42;
    const double ratio = 0.6180339887;

    std::cout << "The answer is " << x << '\n';
    std::cout << "Golden ratio ≈ " << ratio << '\n';
}
```

### Chaining insertions

```cpp
std::cout << "Student " << name << " earned " << credits << " credits." << '\n';
```

- Expression evaluation proceeds left to right, so the output order matches the code order.
- Each `<<` returns a reference to the stream, enabling fluent chaining.
- You can insert manipulators mid-chain, e.g., `std::cout << std::hex << number;` to change formatting.

### Formatting with `<iomanip>`

```cpp
#include <iomanip>
double pi = 3.1415926535;
std::cout << std::fixed << std::setprecision(2) << pi << '\n'; // 3.14
```

- `std::fixed` switches to fixed-point notation.
- `std::setprecision` controls digits after the decimal when used with `std::fixed`.
- Reset formatting using `std::cout.unsetf(std::ios::floatfield);` or stream manipulators such as `std::defaultfloat`.

## Understanding buffering and flushing

- `\n` inserts a newline character but leaves the buffer intact.
- `std::endl` flushes the buffer after adding a newline.
- `std::flush` flushes without inserting a newline.
- `std::cin.tie(nullptr);` decouples `cin`/`cout` flushing for performance-critical sections (use with caution in interactive programs).

Example showing targeted flushing:

```cpp
std::cout << "Processing..." << std::flush;
// long computation
std::cout << " done!\n";
```

## Reading with `std::cin`

### Extraction operator (`>>`)

```cpp
std::string name;
int age{};

std::cout << "Enter your first name: ";
std::cin >> name;

std::cout << "Enter your age: ";
std::cin >> age;
```

- Skips leading whitespace before reading.
- Stops at the next whitespace character (space, tab, newline).
- Fails if the input cannot be converted (e.g., user types letters when expecting `int`).

### Mixing formatted and line input

When you need spaces (full names, addresses), use `std::getline`. Always discard the leftover newline after an extraction.

```cpp
std::string fullName;

std::cout << "Enter your full name: ";
std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
std::getline(std::cin, fullName);
```

### Reading multiple values at once

```cpp
double x{}, y{};
if (std::cin >> x >> y) {
    std::cout << "Sum: " << (x + y) << '\n';
}
```

- Streams short-circuit on failure; the `if` guards against using invalid data.
- Reusing the same stream consecutively is efficient because it maintains state between reads.

## Error handling and recovery

Streams track four bits of state: `goodbit`, `eofbit`, `failbit`, and `badbit`.

1. Check `if (!std::cin)` or `if (std::cin.fail())` after extractions.
2. Call `std::cin.clear()` to reset error flags.
3. Discard the offending input with `std::cin.ignore(...)`.
4. Prompt again.

```cpp
#include <limits>

int age{};
while (true) {
    std::cout << "Enter age (0-120): ";
    if (std::cin >> age && age >= 0 && age <= 120) {
        break;
    }
    std::cout << "Invalid input. Try again.\n";
    std::cin.clear();
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}
```

This pattern is the backbone of resilient console applications.

### Detecting end-of-file (EOF)

EOF occurs when no more data is available. In interactive shells it often corresponds to `Ctrl+D` (Unix) or `Ctrl+Z` (Windows).

```cpp
while (std::cin >> value) {
    // loop automatically stops on EOF or failure
}
```

Use `if (std::cin.eof())` to provide a friendly message when the user ends input.

## Worked example: interactive profile

```cpp
#include <iostream>
#include <iomanip>
#include <limits>

int main() {
    std::string name;
    int year{};
    double gpa{};

    std::cout << "Enter your full name: ";
    std::getline(std::cin, name);

    std::cout << "Enter your enrollment year: ";
    while (!(std::cin >> year)) {
        std::cerr << "Year must be a number. Try again.\n";
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << "Enter your enrollment year: ";
    }

    std::cout << "Enter your GPA: ";
    while (!(std::cin >> gpa)) {
        std::cerr << "GPA must be numeric. Try again.\n";
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << "Enter your GPA: ";
    }

    std::cout << '\n';
    std::cout << "Student profile" << '\n';
    std::cout << "---------------" << '\n';
    std::cout << "Name : " << name << '\n';
    std::cout << "Year : " << year << '\n';
    std::cout << std::fixed << std::setprecision(2);
    std::cout << "GPA  : " << gpa << '\n';
}
```

This example demonstrates chaining, formatting, error recovery, and mixed input patterns in a single cohesive program.

## Common pitfalls

- Forgetting to `#include <limits>` when using `std::numeric_limits` in `ignore` calls.
- Mixing `std::getline` with `>>` without discarding the leftover newline.
- Using `std::endl` in tight loops, causing I/O bottlenecks.
- Assuming `std::cin >> std::string` reads spaces—it does not.
- Ignoring the return value of input operations; always validate.

## Practice time

1. **Meal planner:** Ask the user for breakfast, lunch, and dinner descriptions. Use `std::getline` to keep spaces. Display a formatted three-line summary.
2. **Study tracker:** Prompt for the number of courses and average weekly hours per course (double). Output the total weekly commitment with two decimal places.
3. **Robust integer input:** Write a loop that keeps asking for a positive integer until the user provides one. Log invalid attempts on `std::clog`.
4. **Temperature conversion:** Read Celsius values until EOF and print Fahrenheit equivalents, aligning the output with `std::setw`.

## Self-check questions

1. What are the differences between `std::cout`, `std::cerr`, and `std::clog`?
2. How do you safely read a full line after using `operator>>` on the same stream?
3. Which stream state flags indicate recoverable failure versus fatal errors?
4. When is it appropriate to break the tie between `std::cin` and `std::cout`?
5. Why does `operator>>` stop reading at whitespace, and how can you change that behavior?

Answering these questions solidifies your understanding before moving to richer data processing in the next lesson.
