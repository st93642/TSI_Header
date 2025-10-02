# Lesson 2.4: Break and Continue

Loops become more expressive once you can selectively skip work or exit early. `break` and `continue` let you respond to special cases without restructuring the entire loop. Used carefully, they keep iteration code clear and intentional.

## What You'll Learn

- When to exit a loop immediately with `break`
- How `continue` skips the remainder of the current iteration
- Typical patterns from *Beginning C++17*, Chapter 5 for filtering and sentinel-driven loops
- Guardrails that prevent confusing control flow
- C vs. modern C++ nuances when working with loop control statements

## 1. `break`: Exit Now

```cpp
for (int attempt = 1; attempt <= 3; ++attempt) {
    if (login()) {
        std::cout << "Welcome back!" << std::endl;
        break; // stop looping once we succeed
    }
}
```

- `break` exits the nearest syntactic loop (or `switch`).
- Everything after the `break` inside the loop body is skipped.
- Common uses: stop on success, bail out on error, terminate sentinel loops when a special value arrives.

## 2. `continue`: Skip This Round

```cpp
for (int value : readings) {
    if (value < 0) {
        continue; // ignore sensor noise
    }
    process(value);
}
```

- `continue` jumps to the next iteration.
- In `for` loops, the increment expression runs before the next condition check.
- Use it for “skip and carry on” logic—e.g., ignore invalid inputs while keeping the loop running.

## 3. Combining `break` and `continue`

```cpp
int total = 0;

while (true) {
    int value = 0;
    std::cin >> value;

    if (value == -1) {
        break;               // sentinel stops the loop
    }
    if (value == 0) {
        continue;            // ignore zeros
    }

    total += value;
    std::cout << "Running total: " << total << std::endl;
}
```

*Beginning C++17* highlights this sentinel-and-skip pattern: read until a sentinel (`-1`) breaks the loop, while `continue` filters out data you do not want to process.

## 4. Guardrails

- **Make the exit obvious**: comment why you break or continue; future readers appreciate the breadcrumb.
- **Prefer single exit per loop** when possible; multiple breaks can make logic harder to trace. Group them to avoid surprises.
- **Avoid deep nesting**: if you need to break out of multiple levels, consider helper functions or state flags.
- **Coordinate with resource management**: RAII objects (e.g., `std::ofstream`) release resources automatically even when you break early.

## 5. C vs. Modern C++ Notes

| Topic | C Approach | Preferred C++ Approach |
|-------|------------|------------------------|
| Sentinel loops | `while(1) { ... if (sentinel) break; }` | `while (true)` with clear comments and strong types |
| Skipping values | continue with ints | Same, but prefer `bool` flags and Standard Library containers |
| Structured alternatives | manual state machines | consider algorithms like `std::find_if`, `std::copy_if` when appropriate |

C++ inherits the syntax from C, but modern code often favors expressing the same intent with STL algorithms when they improve readability. When you stay in loops, follow the book’s guidance: keep exit paths obvious and contiguous.

## 6. Common Pitfalls

- **Hidden break conditions**: scattering breaks inside nested branches can lead to missed cases.
- **Forgetting loop invariants**: even if you `continue`, ensure the loop’s assumptions stay valid (e.g., do not skip incrementing a manual counter).
- **Using `break` instead of `return`**: inside functions, returning immediately is often clearer than breaking and relying on later code.

## Practice Ideas

1. Read integers until `-1` and compute their sum while skipping multiples of 5 with `continue`.
2. Iterate over a vector of words and stop printing once you encounter "STOP".
3. Scan a grid (nested loop) and break both loops when a target value is found—try refactoring into a helper function for clarity.

When you are ready, start the practice exercise. You will process a bounded set of scores, skipping invalid entries, and stop when a sentinel appears. Focus on:

- Guarding your loop with a maximum iteration count
- Using `continue` to skip invalid values while keeping the loop alive
- Employing `break` to exit immediately when a sentinel value is read
- Matching the output format exactly (spacing, capitalization, punctuation)

## References

- *Beginning C++17*, Chapter 5 “Arrays and Loops” – sections on `break`, `continue`, sentinel-controlled loops
- cppreference.com: [`break` statement](https://en.cppreference.com/w/cpp/language/break), [`continue` statement](https://en.cppreference.com/w/cpp/language/continue)
- ISO C++ Core Guidelines: Loop control (ES.79, ES.82)
