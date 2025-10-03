# Solving Problems with STL Algorithms

The Standard Library ships a deep catalogue of algorithms that operate on iterator ranges. They reduce boilerplate and make intent explicit once you recognise the patterns.

## Working with iterator pairs

Every algorithm accepts a begin/end iterator range. Containers expose them via `container.begin()` and `container.end()` or the shorthand `std::begin(container)` / `std::end(container)` helper functions.

```cpp
#include <algorithm>
#include <vector>

std::vector<int> values{9, 3, 7};
std::sort(values.begin(), values.end()); // ascending order
```

## Transforming data

`std::transform` applies a callable to every element and can write into the same container or a new destination. Combine it with lambdas to express the transformation inline.

```cpp
std::vector<int> doubled(values.size());
std::transform(values.begin(), values.end(), doubled.begin(), [](int n) {
    return n * 2;
});
```

## Counting and checking conditions

Algorithms like `std::count_if`, `std::all_of`, and `std::any_of` answer questions about a range without manual loops.

```cpp
int evenCount = std::count_if(values.begin(), values.end(), [](int n) {
    return n % 2 == 0;
});

bool allPositive = std::all_of(values.begin(), values.end(), [](int n) {
    return n > 0;
});
```

## Choosing stable vs. unstable operations

Many algorithms have stable variants (such as `std::stable_sort`) that preserve equal elements' original order. Prefer the stable family when element order matters beyond the sort key.

## Practice Time

1. Read a sequence of integers into a `std::vector<int>`.
2. Use `std::sort` to order the numbers ascending and display the result.
3. Produce a descending view using `std::sort` with `std::greater<int>{}` or views from &lt;ranges&gt; if available.
4. Count the number of even values with `std::count_if` and report the tally.
5. Verify whether every number is positive with `std::all_of` and present the result clearly.

When these algorithms feel comfortable, jump into the corresponding exercise to apply them end-to-end.
