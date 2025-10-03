# Organising Lookups with `std::map`

`std::map` keeps key-value pairs sorted by key and provides logarithmic lookup, insertion, and removal. It is perfect when you want deterministic ordering alongside efficient searches.

## Declaring maps

Include &lt;map&gt; and choose types for both the key and the value:

```cpp
#include <map>

std::map<std::string, int> fruitCounts;
std::map<char, double> grades{{'A', 4.0}, {'B', 3.0}};
```

Keys are unique. Writing to `map[key]` inserts the key if it does not already exist.

## Adding or updating entries

Use the subscript operator or `insert` to add items:

```cpp
fruitCounts["apple"] = 12;       // inserts or updates
fruitCounts.insert({"pear", 6});  // does nothing if the key exists
```

`operator[]` default-constructs missing values. For read-only access, prefer `at` which throws for missing keys.

## Iterating in order

`std::map` iterates in ascending key order. Ranged-for loops expose each key-value pair:

```cpp
for (const auto &entry : fruitCounts) {
    std::cout << entry.first << " -> " << entry.second << '\n';
}
```

To change values while iterating, drop the `const` on the reference.

## Counting and aggregating

Combine `map::size()` with &lt;numeric&gt; algorithms to compute summary metrics. `std::accumulate` works with iterator pairs, so you can sum the values by accumulating over the map and adding `entry.second`.

```cpp
#include <numeric>

auto total = std::accumulate(fruitCounts.begin(), fruitCounts.end(), 0,
    [](int sum, const auto &entry) { return sum + entry.second; });
```

## Practice Time

1. Read a handful of key/value pairs from the user and store them in a `std::map`.
2. Print each entry as `key -> value`, proving that the map keeps keys sorted.
3. Report how many unique keys you collected with `map::size()`.
4. Use `std::accumulate` (or a loop) to total the values and present the result to the user.
5. Consider how the map's ordering and logarithmic lookups help whenever you need quick searches.

When you are comfortable with maps, tackle the exercise to reinforce the pattern.
