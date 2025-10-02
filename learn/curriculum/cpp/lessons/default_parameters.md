# Lesson 3.3: Default Parameters

Default arguments let you supply fallback values for function parameters so callers can omit them when the defaults make sense. *Beginning C++17* devotes the second half of Chapter 8 (sections “Default Argument Values” and “Combining Defaults with Overloading”) to this topic—treat those pages as your primary reference while working through this lesson.

## What You'll Learn

- How to declare default arguments in prototypes (Chapter 8, “Default Argument Values”)
- The rule that only trailing parameters may have defaults
- Where to place defaults when you separate declarations and definitions
- How default arguments interact with overloading and constructors
- Pitfalls involving multiple translation units or redefinitions

## 1. Declaring Defaults (Chapter 8, p. 276–279)

You place the default in the function declaration, typically in a header:

```cpp
// declaration (usually in a header)
std::string format_report(int total, int passed = 0, double weight = 1.0);
```

The corresponding definition should omit the defaults:

```cpp
std::string format_report(int total, int passed, double weight) {
    // ...
}
```

When you call `format_report(50);`, the compiler substitutes `passed = 0` and `weight = 1.0` automatically.

## 2. Only Trailing Parameters May Use Defaults

All parameters with defaults must appear at the end of the parameter list. Once you give a parameter a default value, every parameter to its right must also have one. This ensures the compiler knows which values you’re supplying explicitly.

```cpp
void configure_timer(int durationMinutes, int breakMinutes = 5, bool autoStart = false);
// OK: defaults trail the required parameter

void bad(int durationMinutes = 25, bool autoStart); // ❌ invalid: non-default follows default
```

## 3. Avoid Repeating Defaults in Multiple Declarations

If you place defaults in more than one declaration, you’ll get redefinition errors or inconsistent behavior. The book recommends keeping defaults **only** in the primary declaration (often the header) and never in the definition or additional forward declarations.

```cpp
// header
void log_message(const std::string& message, bool newline = true);

// source file — no defaults here
void log_message(const std::string& message, bool newline) {
    // ...
}
```

## 4. Default Arguments vs. Overloads

Default arguments can sometimes replace simple overloads by letting callers omit common options. Chapter 8 warns against combining both features if it introduces ambiguity. When mixing them, ensure every call still resolves unambiguously.

Example:

```cpp
void print_box(int width, int height = 1, char fill = '#');
void print_box(int width, int height, const std::string& title);
```

Calling `print_box(5);` uses defaults and still picks the first overload.

## 5. Constructors and Default Arguments

In classes, default arguments often appear in constructors instead of (or in addition to) overloads:

```cpp
class ProgressTracker {
public:
    explicit ProgressTracker(int goal, int streak = 0, bool notifications = true);
    // ...
};
```

## 6. Common Pitfalls (Chapter 8 highlights)

- **Multiple translation units**: If two source files provide different defaults, behavior becomes undefined. Keep defaults in a single header.
- **Ambiguous overloads**: Defaults that make two overloads identical lead to compilation errors.
- **Changing defaults**: Updating a default value requires recompiling callers to pick up the new value.

## Practice Ideas

1. Give `send_notification()` optional `importance` and `channel` defaults.
2. Refactor a set of overloads that only differ by optional flags into a single function with defaults.
3. Combine defaults with references to avoid extra overloads for `const std::string&` vs. string literals.

## Upcoming Exercise

You’ll implement `format_assignment()` with defaults for points, weight, and whether the assignment is extra credit. Focus on:

- Declaring prototypes with defaults **before** `main()`
- Defining the function **after** `main()` without repeating defaults
- Using `std::ostringstream`, `std::fixed`, and `std::setprecision(1)` for formatting
- Converting boolean flags to “Yes” or “No” text explicitly (per the instructions in the exercise comments)

Review Chapter 8 examples before starting so you can mirror the patterns recommended there.

## References

- *Beginning C++17*, Chapter 8 “Default Argument Values”
- cppreference.com: [Default arguments](https://en.cppreference.com/w/cpp/language/default_arguments)
- ISO C++ Core Guidelines: F.54 (“If you can, avoid overloading by default arguments or by overloading when a default will do”)
