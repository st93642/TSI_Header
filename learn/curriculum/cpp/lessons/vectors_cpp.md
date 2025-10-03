# Building Sequences with `std::vector`

`std::vector` is the go-to dynamic array in modern C++. It stores elements contiguously, grows automatically, and exposes friendly helpers for iteration and algorithms.

## Declaring vectors

Include `<vector>` and provide a type parameter inside angle brackets:

```cpp
#include <vector>

std::vector<int> scores{90, 82, 77};
std::vector<std::string> names = {"Ada", "Janis"};
```

Use brace initialisers to seed values, or call the default constructor and push elements later.

## Adding and accessing elements

- `push_back` appends to the end.
- `operator[]` or `.at(index)` retrieves by position.
- `front()` and `back()` provide quick access to the first/last element.

```cpp
scores.push_back(65);
int first = scores.front();
int last = scores.back();
```

## Iterating

Use a ranged-for loop to walk every element and keep the code terse:

```cpp
for (int value : scores) {
    sum += value;
}
```

If you need the index, use a classic for loop or `std::size_t i = 0; i < scores.size(); ++i`.

## Algorithms

`<numeric>` provides `std::accumulate` for totals, and `<algorithm>` offers helpers like `std::sort`. Because vectors expose iterators, these algorithms compose nicely.

```cpp
#include <numeric>

int sum = std::accumulate(scores.begin(), scores.end(), 0);
```

## Practice Time

1. Read several integers into a `std::vector<int>`.
2. Print every value in order, separated by spaces, to confirm the container holds the data you expect.
3. Report the first value, the last value, the sum, and the average, formatting the average with one decimal place.
4. Use `std::vector` helpers (`size`, `front`, `back`) and `<numeric>` to make the implementation expressive.

When that feels comfortable, open the exercise to reinforce your vector workflow.
