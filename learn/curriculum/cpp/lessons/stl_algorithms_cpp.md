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
