# Arithmetic and User Input

Mixing user input with arithmetic is the fastest way to make interactive programs. This lesson covers the core operators, precedence rules, and reading values from the console.

## Operator recap

C++ supports the familiar arithmetic operators:

- Addition `+`
- Subtraction `-`
- Multiplication `*`
- Division `/`
- Modulo `%` (remainder)

Operator precedence mirrors school algebra. Use parentheses to make intent obvious.

## Integer vs floating-point division

Dividing two integers discards the fractional part. If you need a precise result, promote at least one operand to `double`:

```cpp
int total{7};
int pieces{2};
double exactShare = static_cast<double>(total) / pieces; // 3.5
```

## Reading multiple values

You can chain extractions with `std::cin` to read multiple values in one line, or prompt separately:

```cpp
int a{};
int b{};

std::cout << "Enter two integers separated by spaces:\n";
std::cin >> a >> b;
```

## Guarding against invalid input

If a user types something that cannot be converted, `std::cin` enters a failure state. Clear the error and discard the invalid characters before trying again:

```cpp
if (!std::cin) {
    std::cin.clear();
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}
```

## Practice Time

Try the following before moving on:

1. Ask the user for two integers on separate lines.
2. Compute and print their sum, difference, product, and quotient (as a floating-point value with one decimal place).
3. Label every result so the output is easy to read.
4. Use `static_cast<double>` for the quotient to avoid integer division surprises.

When you are ready, open the exercise to reinforce I/O mixed with arithmetic operations.
