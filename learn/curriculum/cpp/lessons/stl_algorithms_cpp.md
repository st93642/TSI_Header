# Solving Problems with STL Algorithms

The Standard Library ships a deep catalogue of algorithms that operate on iterator ranges. They eliminate boilerplate loops and communicate intent clearly. This lesson introduces the core patterns, shows how to compose algorithms, and warns against common hazards.

## Learning goals

- Choose the right algorithm for sorting, searching, transforming, and aggregating data.
- Understand iterator categories and why some algorithms require random-access iterators.
- Use lambdas and function objects to customise algorithm behaviour.
- Apply the erase-remove idiom to actually remove elements from containers.
- Recognise stable versus unstable algorithms and their effect on ordering.

## Iterator ranges: the universal interface

Algorithms accept a half-open range `[first, last)`—the first iterator points to the beginning, the last points one past the end. Containers expose these via `.begin()`/`.end()` or free functions `std::begin()`/`std::end()`.

```cpp
#include <algorithm>
#include <vector>

std::vector<int> values{9, 3, 7};
std::sort(values.begin(), values.end()); // ascending order
```

- Random-access iterators (vectors, deques, arrays) support full sorting and arithmetic.
- Forward iterators (forward_list) limit you to algorithms that do not need jumps, such as `std::find` or `std::for_each`.

## Transforming data with `std::transform`

Apply a function to each element and write the result into a destination range. Destinations can be the same container (in-place) or a different one if you ensure it has enough space.

```cpp
std::vector<int> doubled(values.size());
std::transform(values.begin(), values.end(), doubled.begin(), [](int n) {
    return n * 2;
});

std::transform(values.begin(), values.end(), values.begin(), [](int n) {
    return n + 10; // in-place add
});
```

## Counting, searching, and predicates

Predicate algorithms answer questions without writing loops manually.

```cpp
int evenCount = std::count_if(values.begin(), values.end(), [](int n) {
    return n % 2 == 0;
});

bool allPositive = std::all_of(values.begin(), values.end(), [](int n) {
    return n > 0;
});

bool anyNegative = std::any_of(values.begin(), values.end(), [](int n) {
    return n < 0;
});

auto it = std::find(values.begin(), values.end(), 7);
if (it != values.end()) {
    std::cout << "Found 7 at index " << std::distance(values.begin(), it) << '\n';
}
```

## Removing elements: erase-remove idiom

Algorithms do not modify container sizes. Pair `std::remove_if` with the container's `erase` to truly delete items.

```cpp
values.erase(std::remove_if(values.begin(), values.end(), [](int n) {
    return n < 0;
}), values.end());
```

This pattern compacts the vector by moving unwanted elements to the end and erasing them.

## Sorting variants and stability

- `std::sort`: fast, requires random-access iterators, does not preserve relative order of equal elements.
- `std::stable_sort`: preserves order among equal keys but may allocate more memory.
- `std::partial_sort`: sorts the first `n` elements, leaving the remainder unsorted.
- `std::nth_element`: places the nth element in its sorted position without fully sorting the range.

```cpp
std::stable_sort(people.begin(), people.end(), [](const Person& a, const Person& b) {
    return a.lastName < b.lastName;
});
```

## Aggregation with `<numeric>`

```cpp
#include <numeric>

int sum = std::accumulate(values.begin(), values.end(), 0);
int product = std::accumulate(values.begin(), values.end(), 1, std::multiplies<>{});
```

- `std::reduce` (C++17) can parallelise reductions for suitable iterators.
- `std::inner_product` computes dot products or combined aggregates.

## Partitioning and grouping

```cpp
auto pivot = std::partition(values.begin(), values.end(), [](int n) {
    return n % 2 == 0;
});
```

`pivot` points to the first element of the "false" group. Use this to separate matching and non-matching elements quickly.

## Using projections (C++20)

With ranges (C++20), algorithms accept projections—functions applied to each element before comparisons—letting you sort structures by one field without writing full comparators. If you are on a pre-C++20 compiler, achieve similar behaviour with lambdas that access the desired member.

## Common pitfalls

- Forgetting that algorithms operate on iterators, not containers; you must call `container.erase` yourself after `remove_if`.
- Passing invalid iterator ranges (e.g., `first` after `last`) leads to undefined behaviour.
- Assuming algorithms preserve order; only the stable variants guarantee that.
- Overwriting destination ranges accidentally when they are not sized appropriately.

## Practice time

1. **Data cleanup:** Read integers, remove duplicates using `std::sort` + `std::unique`, and remove negatives with the erase-remove idiom.
2. **Leaderboard:** Sort a vector of structs by score descending, then stable-sort by name to break ties predictably.
3. **Projection practice:** Sort a vector of filenames by length without altering the original relative order for files of equal length.
4. **Statistics pack:** Use `std::accumulate` to compute sum, `std::minmax_element` for min/max, and `std::count_if` for values above the average.

## Self-check questions

1. Why do most algorithms accept iterator pairs instead of whole containers?
2. How does the erase-remove idiom work, and why is it necessary?
3. When would you pick `std::stable_sort` over `std::sort`?
4. What iterator category does `std::sort` require, and which containers provide it?
5. How can lambdas make algorithm calls more expressive?

Once you can answer these, jump into the exercise to solidify your algorithm toolkit.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: STL Algorithms (Appendix)

A short appendix with a CI snippet to run algorithm-focused tests and a table summarizing common algorithms and trade-offs.

```yaml
# CI: run algorithm tests
name: STL-Algorithms-CI
on: [push]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Build
        run: mkdir -p build && cd build && cmake .. && cmake --build .
      - name: Run tests
        run: ./tests/algorithms_test || true
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Algorithm</th><th>Complexity</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>std::sort</td><td>O(n log n)</td><td>Default choice</td></tr>
    <tr><td>std::nth_element</td><td>O(n)</td><td>Partial ordering</td></tr>
    <tr><td>std::partition</td><td>O(n)</td><td>Stable variants exist</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix)

1. Add unit tests that exercise edge cases for `std::partition` and `std::nth_element`.
2. Add a benchmark harness and commit results as part of CI artifacts.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Algorithm Pitfalls (Appendix II)

Small notes about iterator invalidation, comparator correctness, and a concise table of common pitfalls.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Issue</th><th>Symptom</th><th>Mitigation</th></tr>
  </thead>
  <tbody>
    <tr><td>Invalid comparator</td><td>Undefined behavior</td><td>Test with edge cases</td></tr>
    <tr><td>Iterator invalidation</td><td>Use-after-erase</td><td>Capture indices or use erase-remove idiom</td></tr>
    <tr><td>Stability assumptions</td><td>Order changes</td><td>Use stable_sort if required</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix II — Deep Dive — stl_algorithms-1)

1. Add tests that intentionally pass invalid comparators and assert graceful failure modes.
2. Document when `std::stable_sort` is necessary in the project.

<!-- markdownlint-disable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: STL Algorithms — External Links (Appendix — External Links)

Authoritative references for algorithm complexity and example idioms.

- cppreference algorithms: [std::algorithm reference](https://en.cppreference.com/w/cpp/algorithm)
- Notes on execution policies and parallel algorithms: [execution](https://en.cppreference.com/w/cpp/experimental/execution)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Algorithm</th><th>Link</th><th>When to use</th></tr>
  </thead>
  <tbody>
    <tr><td>std::sort</td><td><a href="https://en.cppreference.com/w/cpp/algorithm/sort">sort</a></td><td>Use for general-purpose sorting</td></tr>
    <tr><td>std::transform</td><td><a href="https://en.cppreference.com/w/cpp/algorithm/transform">transform</a></td><td>Apply element-wise transformations</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Replace a manual loop with `std::transform` and add a small benchmark using `<chrono>`.
2. Try `std::execution::par` on a CPU-bound transform and note portability caveats.

<!-- markdownlint-disable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: STL Algorithms — Deep Dive (Appendix II — External Links)

Notes on algorithm complexity, stable vs unstable sorts, and execution policies for parallel algorithms.

```cpp
#include <algorithm>
#include <vector>
#include <execution>

// simple parallel transform (C++17/C++20 experimental)
std::vector<int> v = {1,2,3,4};
std::transform(std::execution::par, v.begin(), v.end(), v.begin(), [](int x){ return x*2; });
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Algorithm</th><th>Link</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>std::sort</td><td><a href="https://en.cppreference.com/w/cpp/algorithm/sort">sort</a></td><td>Not guaranteed stable</td></tr>
    <tr><td>execution policies</td><td><a href="https://en.cppreference.com/w/cpp/algorithm">execution</a></td><td>Portability varies</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix II — Deep Dive — stl_algorithms-2)

1. Apply `std::execution::par` to a CPU-bound transform and measure speedups across cores; note caveats.
2. Replace a custom loop with a suitable `std::algorithm` and assert behavior.

<!-- markdownlint-disable MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: STL Algorithms — Idioms & Tests (Appendix — stl_algorithms_cpp-appendix2)

Compact reference for common `<algorithm>` idioms, complexity notes, and testable examples.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Algorithm</th><th>Use</th><th>Complexity</th></tr>
  </thead>
  <tbody>
    <tr><td>std::sort</td><td>Sort a vector</td><td>O(n log n)</td></tr>
    <tr><td>std::find_if</td><td>Search with predicate</td><td>O(n)</td></tr>
    <tr><td>std::accumulate</td><td>Fold values</td><td>O(n)</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Quick examples

```cpp
#include <algorithm>
#include <numeric>

std::sort(vec.begin(), vec.end());
int sum = std::accumulate(vec.begin(), vec.end(), 0);
```

### Test tips

- Verify algorithm results with small deterministic inputs and edge cases (empty, single element).
- Use `<algorithm>` in combination with lambda expressions for concise logic.

### Exercises (Appendix — stl_algorithms_cpp-appendix2)

1. Implement a frequency map using `std::sort` + `std::unique` and test with sample data.
2. Use `std::partition` to separate even and odd numbers and add tests that assert partition invariants.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: STL Algorithms — partition, stable_sort & unique (Appendix — stl_algorithms_cpp-appendix3)

Concrete examples using common STL algorithms and notes on complexity and stability when choosing algorithms.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Algorithm</th><th>Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>std::partition</td><td>Partition elements</td><td>Useful for quick separation</td></tr>
    <tr><td>std::stable_sort</td><td>Maintain order</td><td>Use when stability matters</td></tr>
    <tr><td>std::unique</td><td>Remove consecutive duplicates</td><td>Often combined with sort</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```cpp
std::stable_sort(vec.begin(), vec.end(), cmp);
auto it = std::unique(vec.begin(), vec.end());
vec.erase(it, vec.end());

std::partition(vec.begin(), vec.end(), [](int x){ return x % 2 == 0; });
```

### Exercises (Appendix — stl_algorithms_cpp-appendix3)

1. Implement frequency counting using `std::sort` + `std::unique` and test with sample data.
2. Use `std::partition` to separate values by a predicate and assert partition invariants in tests.

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
