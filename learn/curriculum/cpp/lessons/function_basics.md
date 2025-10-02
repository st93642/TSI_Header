# Lesson 3.1: Function Basics

Functions let you break programs into self-contained units, improving readability, reuse, and testability. This lesson walks through everything you need to declare, define, and organize functions without assuming any external material.

## What You'll Learn

- Declaring and defining functions (prototype + body)
- Choosing parameter lists and return types
- Passing arguments by value and understanding when copies are made
- Organizing code so declarations precede `main()` and definitions follow
- Using standard headers to support your function implementations

## 1. Function Anatomy

```cpp
// Declaration (prototype)
double compute_tax(double amount);

int main() {
    double subtotal = 120.0;
    double tax = compute_tax(subtotal);
    std::cout << "Tax: " << tax << std::endl;
}

// Definition
double compute_tax(double amount) {
    return amount * 0.21; // 21% VAT
}
```

Key parts:

- **Return type**: `double` indicates the type of the value sent back to the caller.
- **Function name**: `compute_tax` should describe the calculation clearly so callers know what it returns.
- **Parameter list**: `double amount` is passed **by value**; the function receives its own copy.
- **Body**: wrapped in `{}`, contains executable statements.

## 2. Declarations vs. Definitions

- A **declaration** (prototype) tells the compiler a function exists. Place prototypes before `main()` if definitions come later in the file.
- A **definition** supplies the body. You can place definitions below `main()` once the prototype is visible above.
- Separate files often put declarations in headers (`.hpp`) and definitions in source files (`.cpp`). We will adopt that structure later.

## 3. Parameters and Arguments

- **Parameters** are placeholders in the function definition.
- **Arguments** are the concrete values passed by the caller.
- Pass-by-value copies the argument. Modifying the parameter does **not** modify the caller’s variable—useful for protecting inputs.

```cpp
void announce(std::string message) {
    std::cout << "Announcement: " << message << std::endl;
}

announce("System maintenance at 02:00"); // argument
```

## 4. Return Values

Use `return` to send a value back to the caller. Only one value is returned directly, but you can package multiple items in a `struct` or `std::tuple` later.

```cpp
int max_of_two(int first, int second) {
    if (first > second) {
        return first;
    }
    return second;
}
```

If a function has return type `void`, omit the return value:

```cpp
void log_result(int value) {
    std::cout << "Result: " << value << std::endl;
}
```

## 5. Organizing Source Files

Many style guides encourage a consistent layout:

1. Include directives (`#include <iostream>`).
2. Function declarations/prototypes.
3. `main()` definition.
4. Function definitions (if not placed above `main`).

Example layout for small programs:

```cpp
#include <iostream>

double calculate_total(double price, double tax_rate);

double calculate_total(double price, double tax_rate) {
    return price + (price * tax_rate);
}

int main() {
    double total = calculate_total(120.0, 0.21);
    std::cout << "Total: " << total << std::endl;
    return 0;
}
```

As programs grow, move declarations into header files and definitions into `.cpp` files to keep compilation units organized.

## 6. Best Practices

- Keep functions short and focused on one task.
- Name functions with verbs (`compute`, `print`, `format`) to reflect actions.
- Avoid hidden global state—ideally, every piece of data a function uses should arrive via parameters.
- Document what the function expects and returns; later lessons will reinforce this with comments and contracts.

## 7. Common Pitfalls

- **Missing prototypes**: leads to implicit declarations (disallowed in modern C++). Always declare before use.
- **Mismatched signatures**: parameter types/order must match between declaration and definition.
- **Forgetting `return`**: if a non-`void` function falls off the end, behaviour is undefined. Ensure every control path returns a value.
- **Namespace issues**: explicitly qualify names (`std::cout`) or use `using` directives carefully.

## Practice Ideas

1. Write a function that converts Celsius to Fahrenheit and returns the result.
2. Implement `double rectangle_area(double width, double height)` and call it from `main()` with user input.
3. Create stubs for `read_input()`, `calculate_result()`, and `display_output()` to organize a larger program.

## Next Steps

Complete the practice exercise. You will:

- Declare prototypes for helper functions before `main()`.
- Define the functions after `main()`.
- Pass arguments by value to perform calculations without mutating caller data.
- Format output exactly as specified (spacing, capitalization, punctuation).

## References

- cppreference.com: [Functions](https://en.cppreference.com/w/cpp/language/functions)
- ISO C++ Core Guidelines: Function design (F.1, F.2, F.3)
