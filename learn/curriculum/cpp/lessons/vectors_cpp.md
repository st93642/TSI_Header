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
