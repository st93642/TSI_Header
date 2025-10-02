# Lesson 2.3: Loops (for, while, do-while)

Control flow often means repeating work until a condition changes. C++ offers several loop constructs, each with a distinct rhythm. This lesson shows when to pick `for`, `while`, and `do-while`, plus modern conveniences such as range-based `for` loops.

## What You'll Learn

- Anatomy and use cases for `while`, `do-while`, and `for`
- How to manage loop counters and termination conditions safely
- Combining loops with `break`/`continue`
- Range-based `for` loops over arrays and containers
- Practical guidelines that distinguish C and C++ idioms

## 1. `while` Loops

Use `while` when you do not know how many iterations you need ahead of time.

```cpp
int remaining = getRemainingTasks();

while (remaining > 0) {
    processNextTask();
    --remaining;
}
```

The condition is checked *before* each iteration. If it is already false, the loop body never runs.

## 2. `do-while` Loops

A `do-while` guarantees at least one execution.

```cpp
int option = 0;

do {
    showMenu();
    std::cin >> option;
} while (option != 0);
```

Use this when you must perform an action before checking whether you should continue (e.g., interactive menus).

## 3. `for` Loops

`for` brings initialization, condition, and increment together.

```cpp
for (int i = 0; i < 10; ++i) {
    std::cout << i << ' ';
}
```

Pattern:

```text
for (init; condition; update) {
    // loop body
}
```

- `init` runs once.
- `condition` is checked before each iteration.
- `update` executes after each iteration.

Prefer pre-increment (`++i`) for iterators and scalar counters; it avoids unnecessary temporaries.

## 4. Range-Based `for`

Iterating over collections is more expressive with range-based `for`.

```cpp
std::vector<int> scores{92, 87, 78};

for (int score : scores) {
    std::cout << score << std::endl;
}
```

Add `const` and references to avoid copies:

```cpp
for (const std::string& name : attendeeNames) {
    std::cout << name << '\n';
}
```

## 5. `break` and `continue`

- `break` exits the loop immediately.
- `continue` skips to the next iteration.

```cpp
for (int value : numbers) {
    if (value < 0) {
        continue; // skip negative values
    }
    if (value == 0) {
        break;    // stop entirely
    }
    process(value);
}
```

Use these sparingly; often a well-structured condition or helper function is clearer.

## 6. Loop Guardrails

- Ensure your loop variable changes so the loop terminates.
- Avoid modifying a container while iterating unless you know the iterator rules (see Lesson 4.1).
- Prefer meaningful variable names (`index`, `count`) over single letters in public-facing code.
- Keep loop bodies focused—extract helper functions when work becomes non-trivial.

## 7. C vs. Modern C++ Notes

| Topic | C Approach | Preferred C++ Approach |
|-------|------------|------------------------|
| Loop counters | `int i = 0; i < n; ++i` | Same, but consider `std::size_t` for container sizes |
| Boolean flags | integers (`0`, non-zero) | `bool` values with `true`/`false` |
| Containers | raw arrays | `std::array`, `std::vector`, range-based `for` |
| Infinite loops | `while(1)` | `while (true)` or `for(;;)` with intention-comments |

Modern C++ code is clearer when it uses the Standard Library and type-safe booleans.

## 8. Common Pitfalls

- **Off-by-one errors**: double-check inclusive vs. exclusive limits.
- **Infinite loops**: verify that loop variables change each iteration.
- **Shadowed variables**: declare loop counters inside the loop header when possible to limit scope.
- **Accumulation precision**: choose the right type (`long long`, `double`) when sums can grow.

## Practice Ideas

1. Sum the first `n` positive integers with a `for` loop.
2. Keep reading user commands until `quit` is entered (use `while`).
3. Use a range-based `for` to print every element in a vector with its index (hint: `std::size_t index = 0;`).

When you finish exploring, begin the practice exercise. You will read a limit, build a simple number series with a loop, and calculate the total. Focus on:

- Selecting the correct loop type for the task
- Updating your accumulator correctly
- Producing output that matches the required format exactly (spacing, capitalization, punctuation)

## References

- *Beginning C++17*, Chapter 5 “Arrays and Loops” (sections on `for`, `while`, `do-while`, range-based `for`, break/continue)
- cppreference.com: [`while`](https://en.cppreference.com/w/cpp/language/while), [`do-while`](https://en.cppreference.com/w/cpp/language/do), [`for`](https://en.cppreference.com/w/cpp/language/for)
- ISO C++ Core Guidelines: Loop safety and clarity (ES.74, ES.78, ES.87)
