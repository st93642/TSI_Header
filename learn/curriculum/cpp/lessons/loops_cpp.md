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

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Loops & Iteration (Appendix — loops_cpp-appendix)

Compact reference for loop forms in C++ (traditional for, range-based for), iterator safety, and performance notes.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Form</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>for (i=0;...)</td><td>Classic index loops</td><td>Fine for arrays</td></tr>
    <tr><td>range-based for</td><td>Containers & ranges</td><td>Prefer for readability</td></tr>
    <tr><td>std::for_each</td><td>Functional style</td><td>Useful with algorithms</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: range-based loop

```cpp
#include <vector>
#include <iostream>
int main(){
    std::vector<int> v{1,2,3};
    for(auto& x : v) std::cout << x << '\n';
}
```

### Exercises (loops_cpp-appendix)

1. Convert a classic `for` loop into a range-based loop and measure readability changes.
2. Show a case where modifying a container during iteration invalidates iterators; explain correct alternatives.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Loop Correctness & Safety (Appendix II)

Notes on off-by-one errors, iterator safety, and a table of common loop traps.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Trap</th><th>Symptom</th><th>Fix</th></tr>
  </thead>
  <tbody>
    <tr><td>Off-by-one</td><td>Incorrect bounds</td><td>Prefer range-based loops or use size_t carefully</td></tr>
    <tr><td>Iterator invalidation</td><td>Crash/UB</td><td>Use erase-returning iterators or copy indices</td></tr>
    <tr><td>Concurrent mutation</td><td>Race</td><td>Protect with locks or avoid shared mutation</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix II — loops)

1. Create unit tests that catch off-by-one via boundary inputs.
2. Convert a raw pointer loop into a range-based for and compare results.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Loop Patterns — Deep Dive (Appendix II — External Links)

Examples showing `std::span`, range-based for, and micro-benchmarking loop performance.

```cpp
// Example: using std::span for safe subrange access (C++20)
#include <span>
#include <vector>
#include <iostream>

void print_span(std::span<int> s) {
    for (int v : s) std::cout << v << ' ';
}

int main() {
    std::vector<int> v{1,2,3,4,5};
    print_span({v.data(), 3});
}
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>Link</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Range-for</td><td><a href="https://en.cppreference.com/w/cpp/language/range-for">range-for</a></td><td>Readability + safety</td></tr>
    <tr><td>std::span</td><td><a href="https://en.cppreference.com/w/cpp/container/span">std::span</a></td><td>Lightweight view (C++20)</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix II)

1. Replace an index-based loop with `std::span` and add unit tests.
2. Micro-benchmark a loop with iterators vs indexing using `std::chrono` and report results.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Loops — range-for, iterator invalidation & perf (Appendix — loops_cpp-appendix2)

Notes on safe looping patterns, avoiding iterator invalidation, and micro-optimizations that matter in hot loops.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Loop</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>range-for</td><td>Readable loops</td><td>Prefer const_ref for large objects</td></tr>
    <tr><td>index-based</td><td>Performance-critical</td><td>Prefer raw loops on PODs</td></tr>
    <tr><td>erase in loop</td><td>Modifying container</td><td>Use erase-return idioms to avoid invalidation</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: erase-remove idiom

```cpp
auto it = std::remove_if(vec.begin(), vec.end(), [](int x){ return x < 0; });
vec.erase(it, vec.end());
```

### Exercises (Appendix — loops_cpp-appendix2)

1. Demonstrate iterator invalidation by erasing elements incorrectly, then fix it using the erase-remove idiom.
2. Benchmark a tight loop over `std::vector<int>` using indexes vs range-for and report timing.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Loops & Concurrency — Thread-safety and Ranges (Appendix — loops_cpp-appendix3)

Guidance for iterating data shared across threads and safe patterns when looping in concurrent contexts.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Shared container</td><td>Use mutex or copy-on-write</td><td>Prefer minimal critical sections</td></tr>
    <tr><td>Producer/consumer</td><td>Use condition_variable</td><td>Avoid busy-waiting</td></tr>
    <tr><td>Atomic counters</td><td>std::atomic</td><td>Use for simple counters</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: simple producer/consumer

```cpp
#include <queue>
#include <mutex>
#include <condition_variable>

std::queue<int> q;
std::mutex m;
std::condition_variable cv;

// producer
{
  std::lock_guard<std::mutex> lk(m);
  q.push(1);
}
cv.notify_one();
```

### Exercises (Appendix — loops_cpp-appendix3)

1. Implement a producer/consumer with `std::thread` and `condition_variable`, add tests asserting ordering guarantees.
2. Demonstrate a data race and then fix it by introducing proper synchronization and re-run tests.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->


<!-- Practical Appendix: Reference and further reading -->

### Practical Appendix
This appendix contains brief practical notes and quick references to complement the lesson content. It is intentionally short and safe: no code execution or large data dumps.

- Reference: Official documentation and language core references are excellent further reading sources. Follow the standard docs for authoritative examples.
- Quick tips:
  - Re-run the examples in a REPL to experiment with small changes.
  - Use small, focused test cases when validating behavior.
  - Prefer idiomatic standard-library helpers for clarity and maintainability.

Further reading and sources:
- Official language documentation (search for "official <LANG> docs" where <LANG> is the lesson's language).
- Standard library reference and API pages.
- For curriculum authors: keep examples minimal and include runnable snippets in fenced code blocks.

*End of Practical Appendix.*
