# Lesson 4.4: References

References provide aliases to existing objects, enabling you to pass data efficiently without pointer syntax. *Beginning C++17* covers references in Chapter 9 (“Functions and References”) and Chapter 13 (“Classes and References”), emphasizing const correctness and reference binding rules. Use those sections as your guide while reading this lesson.

## What You'll Learn

- Declaring lvalue references (`int&`) and const references (`const int&`)
- Binding rules: references must bind to valid objects upon initialization
- Reference semantics when passing arguments to functions
- When and why to use references instead of pointers
- An overview of rvalue references (`T&&`) and move semantics (Chapter 13)

## 1. Basic Reference Syntax

```cpp
int score {90};
int& alias {score};       // alias refers to score
const int& view {score};  // read-only view
```

Rules from Chapter 9:

- References must be initialized immediately; they cannot reseat to another object later.
- `const` references can bind to temporaries, allowing safe read-only access.

## 2. Using References in Functions

```cpp
void curve_score(int& score, int delta) {
    score += delta;
}

void print_score(const int& score) {
    std::cout << score;
}
```

- Pass by reference when the callee must modify the argument or avoid copying large objects.
- Pass by const reference for read-only access without copying.

## 3. References vs. Pointers

- References always refer to valid objects (no null references in standard C++), reducing the need for null checks.
- Pointers can be reseated and can be null, giving more flexibility but requiring extra safety checks.
- Prefer references for mandatory arguments and pointers for optional or nullable relationships.

## 4. Rvalue References and Move Semantics (Chapter 13)

Rvalue references (`T&&`) bind to temporaries and enable transfers of ownership with move constructors and move assignment operators. While this lesson focuses on lvalue references, note that move semantics rely on similar aliasing concepts to optimize performance.

## 5. Avoiding Pitfalls

- Do not return references to local variables—they go out of scope.
- Ensure reference parameters clearly document whether they modify the argument.
- Avoid taking non-const references to temporaries; the compiler will reject them unless you use const references.

## Practice Ideas

1. Implement `void normalize(int& value, int min, int max)` that clamps a value via reference.
2. Write a function that fills a struct by reference and returns a status string.
3. Implement swap logic manually with references to two integers.

## Upcoming Exercise

You will build `adjust_boundaries()`, which takes two `int&` parameters (`minimum`, `maximum`) and an `int` adjustment value. The function modifies the references in place and returns a formatted string describing the before-and-after values. Focus on:

- Capturing the original values before modification for reporting
- Ensuring the new minimum does not exceed the new maximum (swap if necessary)
- Returning a string with the exact blueprint used by automated tests
- Leaving printing duties to the caller so the tests can gather output reliably

## References

- *Beginning C++17*, Chapter 9 “Functions and References”
- *Beginning C++17*, Chapter 13 “Classes, Objects, and References”
- cppreference.com: [References](https://en.cppreference.com/w/cpp/language/reference)
- ISO C++ Core Guidelines: F.15 (“Prefer simple and conventional ways of passing information between functions”) and F.16 (“For "in" parameters, pass by value or const reference”)
