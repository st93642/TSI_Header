# Looping with `for`, `while`, and `do-while`

Loops repeat work efficiently, turning tedious manual repetition into compact control structures. This extended lesson dives into the three primary loop forms, shows how to pick the right one, and highlights debugging tricks that save you from common pitfalls.

## Learning goals

- Choose between `while`, `for`, and `do-while` based on whether the iteration count is known.
- Manage loop variables, conditions, and increments to prevent off-by-one errors.
- Accumulate results, search collections, and break out of loops safely.
- Detect and recover from bad user input inside loops.
- Recognise infinite loop risks and instrument loops for debugging.

## `while`: pre-check loops

`while` evaluates the condition before each iteration. Use it when you do not know the number of iterations in advance.

```cpp
int countdown{5};
while (countdown > 0) {
    std::cout << countdown << '\n';
    --countdown;
}
std::cout << "Lift off!\n";
```

- Update the loop variable inside the body; otherwise the condition never changes.
- When consuming input, reset the stream on failure so the next iteration does not reuse stale data.

## `for`: counted loops

`for` loops bundle initialisation, condition, and increment into a single header. Ideal for iterating a fixed number of times or across indexable data.

```cpp
for (int i{0}; i < 5; ++i) {
    std::cout << "Index " << i << '\n';
}
```

Tips:

- Prefer pre-increment (`++i`) with iterators; it avoids unnecessary copies.
- Use `std::size_t` when indexing containers like `std::vector` to match their unsigned size type.
- Declare loop variables inside the header to limit their scope.

### Range-based `for`

When you have a container, use range-based `for` to focus on the values without manual indexing:

```cpp
std::vector<int> scores{87, 92, 76};
for (int score : scores) {
    std::cout << score << ' ';
}
```

Add `const` when you do not intend to modify the element.

## `do-while`: post-check loops

`do-while` guarantees the body runs at least once, perfect for menus or retry prompts.

```cpp
#include <limits>

int choice{};
do {
    std::cout << "1. Start\n2. Settings\n3. Quit\n";
    std::cout << "Choose an option: ";
    std::cin >> choice;
    if (!std::cin) {
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        choice = 0; // force another iteration
    }
} while (choice != 3);
```

- Include a way to escape the loop—typically by updating the condition variable.
- Keep user prompts inside the loop so the user sees them again on invalid input.

## Managing control flow

### Breaking early

`break` exits the nearest loop immediately. Useful when you have found the answer and do not need to keep iterating.

```cpp
for (int value : scores) {
    if (value == target) {
        found = true;
        break;
    }
}
```

### Skipping to the next iteration

`continue` jumps to the next condition check, skipping the rest of the body. Use it sparingly to avoid hiding essential work.

```cpp
for (int n : numbers) {
    if (n == 0) {
        continue; // skip division by zero
    }
    total += 100 / n;
}
```

### Avoiding infinite loops

- Ensure loop variables change toward the exit condition.
- Log or print iteration counters during debugging to confirm progress.
- For user input loops, reinitialise the state when validation fails.

## Accumulators, searches, and sliding windows

Loops rarely just print values. They often build aggregates or search for patterns.

```cpp
double sum{0.0};
int count{0};
double value{};

while (std::cin >> value) {
    sum += value;
    ++count;
}

if (count > 0) {
    std::cout << "Average: " << sum / count << '\n';
}
```

- Initialise accumulators before the loop.
- After the loop, guard against division by zero when no data was processed.

For sliding windows or running averages, keep a container of recent items and update it each iteration.

## Nested loops and complexity

Nested loops multiply runtime (`O(n^2)`, `O(n*m)` etc.). Use them when traversing grids or combinations, but be mindful of performance for large datasets. Sometimes breaking out early or using STL algorithms provides a clearer path.

## Debugging tips

- Print loop variables at key points to track progress.
- Add safeguard counters that abort after a maximum number of iterations while debugging.
- For range-based loops, use `auto` references to avoid unintended copies: `for (const auto& item : data)`.

## Practice time

1. **FizzBuzz refactor:** Print numbers 1–100, substituting `Fizz`, `Buzz`, or `FizzBuzz` for multiples of 3, 5, or both. Implement it twice: once with a classic `for` loop and once with a `while` loop.
2. **Input validation menu:** Build a menu loop that keeps asking until the user chooses `Quit`. Handle invalid input by clearing the stream and reprompting.
3. **Running statistics:** Continuously read integers until `-1` appears. Track the minimum, maximum, and rolling average, then report them.
4. **Multiplication table:** Use nested `for` loops to print a formatted table for values 1 through 10.

## Self-check questions

1. When would you prefer `do-while` over `while`?
2. How does a range-based `for` differ from a classic `for`, and when might you still need indexes?
3. What causes an infinite loop, and what debugging strategies reveal it quickly?
4. How do `break` and `continue` change flow control differently?
5. Why should you reset the input stream after a failed extraction inside a loop?

Answer these before moving on—the next lessons combine loops with containers and algorithms.
