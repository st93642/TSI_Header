# Lesson 3.4: Recursion

Recursion lets a function call itself to solve a problem by breaking it into smaller subproblems. *Beginning C++17* covers this pattern in Chapter 9 (“Recursion” section), including the mechanics of call stacks, base cases, and practical examples such as factorial and power functions. Use that chapter as your primary reference while you study this lesson.

## What You'll Learn

- How to identify the two essential parts of a recursive function (Chapter 9, “Designing Recursive Functions”)
- How each call creates a new stack frame storing parameters and local variables
- Why every recursion needs a base case to stop the call chain
- How to reason about recursion depth and guard against stack overflow
- When to combine recursion with helper functions for state tracking

## 1. Anatomy of a Recursive Function

Every recursive function must include:

1. **Base case**: Stops the recursion when the problem is small enough to solve directly.
2. **Recursive step**: Reduces the problem and calls the function again with the smaller input.

Example from Chapter 9 (factorial):

```cpp
unsigned long long factorial(unsigned int n) {
    if (n <= 1) {
        return 1;               // base case
    }
    return n * factorial(n - 1); // recursive step
}
```

## 2. Visualizing the Call Stack (Chapter 9, “Function Activation Records”)

Each recursive call gets its own activation record on the call stack. Parameters, return addresses, and local variables live in that activation record until the call finishes. Understanding this layout helps debug recursive logic and avoid accidental mutation of shared state.

## 3. Designing Safe Recursion

- **Ensure the input shrinks** every recursive step; otherwise, the function never reaches the base case.
- **Guard against invalid input** before calling recursively (e.g., negative integers when only nonnegative inputs make sense).
- **Watch recursion depth**: deep trees or large numbers can overflow the stack.
- Consider writing an **iterative alternative** when performance or stack depth becomes a concern.

## 4. Tail Recursion (Chapter 9, “Tail Recursion”)

Tail recursion occurs when the recursive call is the final operation in the function. Compilers can optimize tail recursion, but that optimization isn’t guaranteed in C++. You can help by carrying extra state forward:

```cpp
unsigned long long factorial_helper(unsigned int n, unsigned long long accumulator) {
    if (n <= 1) {
        return accumulator;
    }
    return factorial_helper(n - 1, n * accumulator);
}
```

Then expose a wrapper: `unsigned long long factorial(unsigned int n) { return factorial_helper(n, 1ULL); }`.

## 5. Common Patterns

- **Mathematical sequences**: factorial, Fibonacci, exponentiation
- **Divide-and-conquer**: merge sort, quicksort
- **Tree and graph traversal**: depth-first searches
- **Backtracking**: solving mazes, generating permutations

## 6. Avoiding Pitfalls

- Define the base case clearly and test it first.
- Keep the recursive step simple; off-by-one errors easily appear here.
- Prefer returning values instead of printing inside recursion to keep it reusable.
- Document recursion depth expectations, especially for user-supplied input.

## Practice Ideas

1. Write `sum_to(n)` recursively to add integers from 1 through `n`.
2. Implement `string reverse_string(const std::string&)` recursively without using iterative loops.
3. Traverse a nested directory structure (conceptually) with recursion to count files.

## Upcoming Exercise

You will implement a recursive grade calculator, `calculate_cumulative_score`, that works through a vector of assignment weights. Focus on:

- Providing prototypes before `main()` and definitions afterward (matching Chapter 9 examples)
- Handling empty collections as the base case
- Building the output string with `std::ostringstream` so you can compare against the provided blueprint
- Demonstrating the recursion by walking through multiple test vectors

## References

- *Beginning C++17*, Chapter 9 “Recursion”
- cppreference.com: [recursion](https://en.cppreference.com/w/cpp/language/recursion)
- ISO C++ Core Guidelines: SF.8 (“Prefer iterative constructs to recursion unless recursion is natural and clear”)
