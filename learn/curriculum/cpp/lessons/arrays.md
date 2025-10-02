# Lesson 4.1: Arrays

Static arrays give you contiguous, fixed-size storage—perfect for small collections where the size is known at compile time. *Beginning C++17* covers arrays in Chapter 10 (“Working with Arrays”) and Chapter 11 (“Standard Library Containers”), contrasting built-in arrays with `std::array`. Use those chapters—especially the sections on declaration syntax, initialization rules, and iteration—as your primary reference.

## What You'll Learn

- Declaring and initializing built-in arrays and `std::array` (Chapter 10, “Declaring Arrays”)
- Understanding array decay to pointers when passed to functions
- Safe iteration techniques (range-based for, index loops, `std::array::at`)
- Common pitfalls: uninitialized elements, out-of-bounds access, and magic numbers
- When to prefer `std::array` over raw arrays for stronger safety guarantees

## 1. Declaring Arrays

```cpp
int legacyScores[5] {90, 82, 88, 91, 95};            // built-in array
std::array<int, 5> saferScores {90, 82, 88, 91, 95}; // std::array
```

Key points (Chapter 10):

- Built-in arrays require compile-time size known at declaration.
- `std::array` is a light wrapper that stores the size as part of the type.
- Aggregate initialization lets you set values at declaration; missing values default to zero.

## 2. Accessing Elements Safely

- Built-in arrays do **not** perform bounds checking—using an invalid index is undefined behavior.
- `std::array::at(index)` throws an exception on out-of-range access, while `operator[]` does not.
- Prefer explicit size constants or `std::size(array)` to avoid “magic numbers.”

## 3. Passing Arrays to Functions (Chapter 10, “Arrays and Functions”)

Built-in arrays decay to pointers when passed to functions. To preserve size information, pass the length as a separate parameter or use `std::array` references:

```cpp
void print_scores(const int* scores, std::size_t length);
void print_scores(const std::array<int, 5>& scores);
```

## 4. Iteration Patterns

- **Index loop**: classic `for (std::size_t i = 0; i < length; ++i)`
- **Range-based for**: ideal for read-only traversal
- **Algorithms**: use `<numeric>` for sums, `<algorithm>` for min/max

## 5. Avoiding Pitfalls

- Always initialize arrays before reading from them.
- Keep track of the logical number of elements used.
- When the size varies at runtime, switch to `std::vector` or a dynamic allocation strategy (covered in Lesson 4.3).

## Practice Ideas

1. Store weekly productivity scores in an `std::array<int, 7>` and compute min/max/average.
2. Write a function that normalizes grades stored in a raw array using pointer arithmetic.
3. Use `std::array<std::string, 4>` to hold activity labels and print them with indices.

## Upcoming Exercise

You will build an `analyze_scores()` function that accepts exactly five integers, stores them in `std::array<int, 5>`, and produces a summary string with minimum, maximum, and average (formatted to one decimal place). Focus on:

- Declaring and initializing `std::array` from input values
- Using `<algorithm>` helpers like `std::minmax_element`
- Computing averages with `std::accumulate`
- Producing output that exactly matches the provided blueprint for the automated tests

## References

- *Beginning C++17*, Chapter 10 “Working with Arrays”
- *Beginning C++17*, Chapter 11 “Standard Library Containers” (sections on `std::array`)
- cppreference.com: [`std::array`](https://en.cppreference.com/w/cpp/container/array)
- ISO C++ Core Guidelines: ES.22 (“Don’t declare a variable until you have a value for it”)
