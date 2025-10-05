# Building Sequences with `std::vector`

`std::vector` is the go-to dynamic array in modern C++. It stores elements contiguously, grows automatically, and works seamlessly with the rest of the Standard Library. This lesson explores declaration, growth, iteration, algorithms, and performance-minded practices.

## Learning goals

- Declare vectors with different element types and initialisation styles.
- Add, remove, and access elements safely using bounds-aware methods.
- Understand capacity vs. size and when to reserve memory up front.
- Iterate with indices, iterators, and range-based loops.
- Combine vectors with algorithms from `<algorithm>` and `<numeric>`.

## Declaring vectors

Include `<vector>` and specify the element type inside angle brackets.

```cpp
#include <vector>

std::vector<int> scores{90, 82, 77};
std::vector<std::string> names{"Ada", "Janis"};
std::vector<double> values(5, 0.0); // five zeros
```

- Brace initialisers seed specific values.
- Constructor overloads create vectors of a chosen size with default values.
- For large vectors, prefer explicitly listing the size to avoid repeated `push_back` calls if the number of elements is known.

## Adding, accessing, and removing elements

- `push_back` appends to the end.
- `emplace_back` constructs elements in place without an extra copy.
- `operator[]` provides unchecked access; `.at(index)` performs bounds checking and throws on errors.
- `front()` and `back()` provide quick access to the first/last element.
- `pop_back()` removes the last element.

```cpp
scores.push_back(65);
scores.emplace_back(70);

int first = scores.front();
int last = scores.back();

if (!scores.empty()) {
    scores.pop_back();
}
```

Guard operations with `empty()` when the container might have zero elements.

## Size vs. capacity

- `size()` counts the number of elements currently stored.
- `capacity()` reveals how many elements can fit before a reallocation occurs.
- `reserve(n)` requests capacity without changing size, reducing reallocations during bulk inserts.
- `shrink_to_fit()` asks the vector to release unused capacity (implementation may ignore it).

```cpp
std::vector<int> data;
data.reserve(1000); // prevent repeated growth when adding many items
```

Understanding capacity prevents subtle performance cliffs when vectors grow gradually in tight loops.

## Iterating

### Index-based loops

```cpp
for (std::size_t i = 0; i < scores.size(); ++i) {
    std::cout << i << ": " << scores[i] << '\n';
}
```

### Range-based loops

```cpp
for (int value : scores) {
    sum += value;
}

for (int& value : scores) {
    value += 5; // mutate elements in place
}
```

### Iterator loops

```cpp
for (auto it = scores.begin(); it != scores.end(); ++it) {
    std::cout << *it << '\n';
}
```

Iterators resemble pointers; prefer them when interfacing with algorithms that expect iterator ranges.

## Algorithms and utilities

- `<algorithm>` hosts tools like `std::sort`, `std::reverse`, `std::find`, and `std::unique`.
- `<numeric>` adds `std::accumulate`, `std::reduce`, `std::inner_product`.

```cpp
#include <algorithm>
#include <numeric>

std::sort(scores.begin(), scores.end());
int sum = std::accumulate(scores.begin(), scores.end(), 0);
int above80 = std::count_if(scores.begin(), scores.end(), [](int s) { return s >= 80; });
```

Because vectors store elements contiguously, they work seamlessly with algorithms that expect random-access iterators.

## Working with multidimensional vectors

Vectors can hold other vectors to represent grids or matrices:

```cpp
std::vector<std::vector<int>> grid(3, std::vector<int>(4, 0));
grid[1][2] = 5;
```

Be mindful of performance when resizing nested vectors; reserve capacity for each row if you know the dimensions in advance.

## Common pitfalls

- Holding references or iterators to elements and then calling operations that reallocate (like `push_back`) invalidates them.
- Using `operator[]` without checking bounds can lead to undefined behaviour.
- Forgetting to reserve capacity when adding thousands of elements in a tight loop can cause repeated reallocations.

## Practice time

1. **Statistics dashboard:** Read integers into `std::vector<int>`, then report min, max, average, and median using algorithms.
2. **Grade curve:** Increase every score by five points using a range-based loop, but clamp values at 100 with `std::min`.
3. **Frequency table:** Read words into a vector, sort it, and remove duplicates with `std::unique`. Count occurrences of each unique word.
4. **Matrix operations:** Build a 3×3 matrix using `std::vector<std::vector<int>>` and compute row sums and column sums.

## Self-check questions

1. What is the difference between `size()` and `capacity()`?
2. When would you prefer `.at()` over `operator[]`?
3. How does `reserve` improve performance when inserting many elements?
4. Why do iterators become invalid after certain operations, and how can you deal with it?
5. How would you sort a vector of structs by a field using `std::sort`?

Master these patterns to wield vectors effectively before moving on to associative containers and algorithms.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Vectors — Performance and Patterns

This appendix provides tips for reserving capacity, using `emplace_back`, memory layout insights, and an HTML table summarising common operations.

### Reserve early

```cpp
std::vector<int> v;
v.reserve(1000);
for (int i=0;i<1000;++i) v.push_back(i);
```

### Emplace vs push

```cpp
struct S { std::string s; S(std::string v): s(std::move(v)){} };
std::vector<S> v;
v.emplace_back("hello"); // constructs in-place, avoids copy
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Operation</th><th>Method</th><th>Complexity</th></tr>
  </thead>
  <tbody>
    <tr><td>Append</td><td>push_back / emplace_back</td><td>Amortized O(1)</td></tr>
    <tr><td>Random access</td><td>operator[]</td><td>O(1)</td></tr>
    <tr><td>Resize</td><td>reserve / shrink_to_fit</td><td>Depends on implementation</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises

1. Implement a simple dynamic buffer that doubles capacity when full and compare timings with `std::vector` for large insertions.
2. Use `std::move` to transfer a large vector into a function efficiently.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: std::vector — Capacity & Growth

This appendix includes patterns to avoid reallocations, shrink-to-fit caveats, and a container comparison table.

```cpp
std::vector<int> v;
v.reserve(100); // avoid repeated reallocations
for (int i = 0; i < 100; ++i) v.push_back(i);
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Container</th><th>Access</th><th>When to prefer</th></tr>
  </thead>
  <tbody>
    <tr><td>std::vector</td><td>O(1)</td><td>Contiguous, random access</td></tr>
    <tr><td>std::list</td><td>O(n)</td><td>Frequent middle insert/erase</td></tr>
    <tr><td>std::deque</td><td>O(1)</td><td>Efficient push/pop both ends</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Practical Appendix)

1. Create a microbenchmark showing cost of repeated push_back without reserve vs with reserve.
2. Implement a small memory-usage report showing capacity vs size over time.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: std::vector — Resources (Appendix — External Links)

Authoritative references and common performance notes for `std::vector`.

- cppreference `std::vector`: [std::vector reference](https://en.cppreference.com/w/cpp/container/vector)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Doc</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Capacity & reserve</td><td><a href="https://en.cppreference.com/w/cpp/container/vector">vector</a></td><td>Use reserve to reduce reallocations</td></tr>
    <tr><td>Iterator validity</td><td><a href="https://en.cppreference.com/w/cpp/container/vector#Iterator_invalidity">iterator invalidation</a></td><td>Reallocation invalidates pointers/iterators</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Benchmark push_back with and without reserve for a large input set.
2. Show iterator invalidation by storing an iterator and triggering a reallocation.

<!-- markdownlint-enable MD010 -->

## Practical Appendix: External Tools & Examples (Appendix — External Tools — vectors_cpp-appendix)

Notes and quick recipes for `std::vector` usage, iterator invalidation, and performance tuning. Links point to the authoritative cppreference pages.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Why</th><th>Link</th></tr>
  </thead>
  <tbody>
    <tr><td>std::vector</td><td>Contiguous storage & APIs</td><td><a href="https://en.cppreference.com/w/cpp/container/vector">cppreference: vector</a></td></tr>
    <tr><td>Iterator invalidation</td><td>When iterators are invalidated</td><td>See documentation on member function effects at cppreference</td></tr>
    <tr><td>Reserve/Capacity</td><td>Avoid reallocations</td><td>Use reserve() when size known</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: reserve to avoid reallocations

```cpp
#include <vector>
#include <iostream>

int main(){
    std::vector<int> v;
    v.reserve(100); // avoid repeated reallocations
    for(int i=0;i<100;i++) v.push_back(i);
    std::cout << v.size() << '\n';
}
```

### Exercises (vectors_cpp-appendix)

1. Write a short benchmark comparing push_back with and without reserve; report allocation counts if your environment offers them (e.g., instrument allocator or use valgrind/heap profiling).
2. Demonstrate iterator invalidation by saving an iterator across a capacity-change operation and explain the result.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: std::vector — Performance & Diagnostics (Appendix — vectors_cpp-continued)

Notes and recipes to profile vector usage, avoid reallocations, and instrument allocations in exercises.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Problem</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Repeated push_back</td><td>reserve()</td><td>Pre-reserve to reduce reallocations</td></tr>
    <tr><td>Iterator invalidation</td><td>Avoid storing iterators across resizes</td><td>Recompute iterators after mutation</td></tr>
    <tr><td>Large moves</td><td>std::move</td><td>Transfer ownership cheaply</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Microbenchmark example (allocation count)

```cpp
#include <vector>
#include <chrono>
#include <iostream>

int main(){
    std::vector<int> v;
    v.reserve(1'000'000);
    auto t0 = std::chrono::high_resolution_clock::now();
    for(int i=0;i<1'000'000;++i) v.push_back(i);
    auto t1 = std::chrono::high_resolution_clock::now();
    std::cout << "Time: " << std::chrono::duration<double>(t1 - t0).count() << "s\n";
}
```

### Exercises (Appendix — vectors_cpp-continued)

1. Build the microbenchmark above and run with and without `reserve` to observe the timing difference.
2. Demonstrate iterator invalidation by storing an iterator before growing the vector and document the behaviour.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: std::vector — Capacity, Reserve & Move Semantics (Appendix — vectors_cpp-appendix-20251005)

Practical tips for using `std::vector` efficiently: `reserve`, `shrink_to_fit`, and understanding capacity vs size.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Operation</th><th>Method</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Reserve</td><td>v.reserve(n)</td><td>Avoid repeated reallocations</td></tr>
    <tr><td>Access</td><td>operator[] vs at()</td><td>`at` checks bounds</td></tr>
    <tr><td>Shrink</td><td>v.shrink_to_fit()</td><td>Non-binding request</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example

```cpp
std::vector<int> v;
v.reserve(1000);
for (int i = 0; i < 1000; ++i) v.push_back(i);
```

### Exercises (Appendix — vectors_cpp-appendix-20251005)

1. Benchmark push_back with and without reserve and report timings.
2. Demonstrate move semantics when pushing movable objects into a vector.

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
