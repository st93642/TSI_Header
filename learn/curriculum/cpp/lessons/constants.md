# Lesson 1.4: Constants and the Preprocessor

Constants represent values that should not change while the program runs. You'll often use them for configuration details such as maximum sizes or institution names. This lesson shows how to declare constants in both modern C++ and classic C.

## What You'll Learn

- Declaring compile-time constants in C++
- Using `#define` macros and `const` in C
- Formatting numeric constants for output
- Choosing the right style for your project

## Constants in Modern C++

C++17 offers multiple ways to define immutable data. `constexpr` ensures the value is available at compile time.

```cpp
#include <iostream>
#include <iomanip>

int main() {
    constexpr const char* CAMPUS = "TSI Riga";
    constexpr int MAX_SEATS = 32;
    constexpr double PI = 3.1416;

    std::cout << "Campus: " << CAMPUS << std::endl;
    std::cout << "Max Seats: " << MAX_SEATS << std::endl;
    std::cout << std::fixed << std::setprecision(4);
    std::cout << "Pi Value: " << PI << std::endl;

    return 0;
}
```

### Tips for C++

- Prefer `constexpr` for values known at compile time.
- Use `std::fixed` and `std::setprecision` to format floating-point output.
- Keep constant names descriptive and use uppercase for simple values.

## Constants in C

C relies on the C preprocessor and the `const` keyword.

```c
#include <stdio.h>

#define CAMPUS "TSI Riga"
#define PI 3.1416

int main(void) {
    const int MAX_SEATS = 32;

    printf("Campus: %s\n", CAMPUS);
    printf("Max Seats: %d\n", MAX_SEATS);
    printf("Pi Value: %.4f\n", PI);

    return 0;
}
```

### Tips for C

- Use `#define` for literal values shared across multiple files.
- `const` variables still occupy storage but prevent accidental reassignment.
- Control floating-point formatting with `%.2f`, `%.4f`, etc.

## Choosing Between Styles

- Use C++ features (`constexpr`, `const`) in modern projects for type safety.
- Use macros sparingly—prefer typed constants when possible.
- Keep related constants together for readability.

## Practice Exercise

Your exercise will combine both approaches: create constants for TSI campus data and print them with precise formatting using either C or C++.
