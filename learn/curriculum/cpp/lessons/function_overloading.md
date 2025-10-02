# Lesson 3.2: Function Overloading

Modern C++ lets you declare multiple functions with the same name as long as their parameter lists differ. Chapter 8 of *Beginning C++17* (“Function Overloading”) discusses this in detail—use it as the authoritative source for definitions, examples, and naming conventions. This lesson summarizes the chapter and prepares you for the companion exercise.

## What You'll Learn

- What qualifies as an overload (Chapter 8, “Function Overloading”)
- Which parts of a function signature matter (parameter types, count, qualifiers)
- Why return type alone cannot distinguish overloads
- Overloading for convenience vs. clarity
- Resolving overloads with conversions and best matches

## 1. Overload Basics (Book reference: Chapter 8, Function Overloading)

```cpp
int square(int value) {
    return value * value;
}

double square(double value) {
    return value * value;
}
```

The compiler picks the correct `square()` based on the argument type:

```cpp
std::cout << square(5);    // calls int version
std::cout << square(5.5);  // calls double version
```

Each overload must differ in its **parameter list**—either the number of parameters or the types (including references). The return type is not considered when choosing an overload.

## 2. Matching Rules (Chapter 8, “Overloading and Pointer/Reference Parameters”)

When the compiler encounters a call, it follows these steps:

1. Find all functions with the given name that are in scope.
2. Filter to those whose parameter lists can be converted from the arguments provided.
3. Choose the best match (exact match wins over conversions).

### Exact vs. Conversion

```cpp
void log(int value);
void log(double value);

log(42);    // uses int overload (exact)
log(2.5f);  // float converts to double (best match)
log('A');   // char converts to int or double; int is a better match
```

## 3. Avoid Ambiguity

Ambiguous calls cause compilation errors. Example:

```cpp
void report(long value);
void report(double value);

report(5); // ambiguous: both long and double are viable with conversions
```

To avoid this:

- Favor explicit types when calling (`report(5L)`)
- Provide overloads for common literal types (`int`, `double`)
- Use `std::int64_t` or other fixed-width types instead of relying on implicit conversions

## 4. Overloading with References (Chapter 8, “Overloading and Reference Parameters”)

You can overload by reference qualifiers (e.g., `const std::string&` vs. `std::string&&`). For beginner exercises, focus on distinguishing between value and `const` reference parameters:

```cpp
void print(const std::string& text);
void print(const std::vector<std::string>& lines);
```

## 5. Default Parameters and Overloads

- Adding default arguments can reduce the need for overloads (“Default Argument Values” section).
- Avoid mixing overloads and default arguments in ways that create ambiguous calls.

## 6. Best Practices (as emphasized in *Beginning C++17*)

- Overload for closely related tasks. If the functions perform different work, use different names.
- Keep implementations consistent—if one overload logs extra info, all should follow suit.
- Document the expected argument types and side effects.

## 7. Common Pitfalls

- **Return type only**: Compiler cannot choose between `int compute()` and `double compute()` if both take no parameters.
- **Implicit conversions**: Too many overloads relying on conversions can produce surprising matches.
- **Ambiguous templates**: Function templates plus overloads can create conflicts; later lessons cover this.

## Practice Ideas (Book exercises, Chapter 8)

1. Overload `max_value()` for `int`, `double`, and `std::string` (lexicographic comparison).
2. Provide overloads for `log_message()` that accept `std::string`, `const char*`, and an `int` severity.
3. Implement `print_box()` overloads for squares versus rectangles—one takes one side, the other takes width/height.

## Next Steps

The exercise will have you write overloaded `format_grade()` functions—one that accepts raw scores, another that accepts percentages, and a third that accepts letter grades. You will:

- Declare prototypes before `main()` for all overloads.
- Define each overload after `main()`.
- Ensure output format matches the expected strings exactly.
- Demonstrate why return type alone cannot be used to overload.

## References

- *Beginning C++17*, Chapter 8 “Function Overloading”
- cppreference.com: [Overload resolution](https://en.cppreference.com/w/cpp/language/overload_resolution)
- ISO C++ Core Guidelines: F.3 (“Keep functions short and simple”) and F.38 (“Use function overloading judiciously”)
