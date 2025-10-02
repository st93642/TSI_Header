# Lesson 1.3: Input and Output

Input/output (I/O) is how your program communicates with users. In this lesson, you'll see how to gather information and display results in both C++ and C.

## What You'll Learn

- Reading integers from the console
- Writing formatted output
- Handling newlines and spacing
- Differences between C and C++ I/O APIs

## Reading and Writing in C++

C++ uses stream objects such as `std::cin` and `std::cout`.

```cpp
#include <iostream>

int main() {
    int first = 0;
    int second = 0;

    std::cout << "Enter a number: ";
    std::cin >> first;

    std::cout << "Enter another number: ";
    std::cin >> second;

    std::cout << "Sum: " << first + second << std::endl;
    std::cout << "Product: " << first * second << std::endl;

    return 0;
}
```

### Tips for C++

- `std::cin >> value;` skips leading whitespace and reads formatted data.
- Chain `std::cout` operations with `<<` to build clear messages.
- `std::endl` prints a newline **and** flushes the stream; use `"\n"` when you don't need flushing.

## Reading and Writing in C

C uses formatted functions like `scanf` and `printf` from `<stdio.h>`.

```c
#include <stdio.h>

int main(void) {
    int first = 0;
    int second = 0;

    printf("Enter a number: ");
    scanf("%d", &first);

    printf("Enter another number: ");
    scanf("%d", &second);

    printf("Sum: %d\n", first + second);
    printf("Product: %d\n", first * second);

    return 0;
}
```

### Tips for C

- Always pass the **address** of the variable to `scanf` using `&`.
- Match your format specifiers: `%d` for integers, `%f` for floating-point values.
- Add `\n` to move to a new line after each `printf` call.

## Coordinating Input and Output

Regardless of language:

- Read all required input before performing calculations.
- Store intermediate results in variables for clarity.
- Keep output labels consistent so automated tests can compare results.

## Practice Exercise

In the exercise you'll build a tiny calculator that reads two integers and prints their sum, difference, and product. Try it first in C++, then repeat in C to master both styles!
