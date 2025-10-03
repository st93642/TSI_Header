# Organising Lookups with `std::map`

`std::map` stores key-value pairs in sorted order and guarantees logarithmic-time lookups, insertions, and removals. This lesson explores map fundamentals, iteration patterns, comparison with other associative containers, and best practices for safe updates.

## Learning goals

- Declare maps with the proper key/value types and initialise them flexibly.
- Insert, update, and query entries without accidentally creating defaults.
- Iterate in sorted order and perform aggregations on values.
- Choose between `std::map`, `std::unordered_map`, and `std::multimap` for different workloads.
- Manage custom comparison rules when the default key ordering is not enough.

## Declaring and initialising maps

Include `<map>` and specify the key/value types:

```cpp
#include <map>
#include <string>

std::map<std::string, int> fruitCounts;
std::map<char, double> grades{{'A', 4.0}, {'B', 3.0}};
```

- Keys must be unique and comparable using `operator<` by default.
- Use brace initialisers to seed data. Missing keys will be inserted automatically if you access them with `operator[]`.

## Inserting and updating entries

- `map[key] = value;` inserts or overwrites. It default-constructs a value when the key is new.
- `at(key)` accesses without insertion and throws `std::out_of_range` if the key is missing.
- `insert({key, value})` adds only when the key does not exist, returning a pair with an iterator and a success flag.
- `emplace(key, value)` constructs the element in place without extra copies.

```cpp
fruitCounts["apple"] = 12;                 // insert or update
auto [pos, inserted] = fruitCounts.insert({"pear", 6});
if (!inserted) {
    pos->second += 3; // pear already existed; update value
}

try {
    int bananas = fruitCounts.at("banana");
} catch (const std::out_of_range&) {
    std::cout << "No bananas tracked yet.\n";
}
```

## Iterating in order

Maps maintain ascending key order (based on `operator<` or a custom comparator). Iterate with ranged-for loops:

```cpp
for (const auto& [fruit, count] : fruitCounts) {
    std::cout << fruit << " -> " << count << '\n';
}
```

Structured bindings (C++17) unpack keys and values elegantly. Omit `const` when you need to mutate values during iteration.

## Searching and erasing

- `find(key)` returns an iterator to the entry or `end()` if not found.
- `contains(key)` (C++20) returns a boolean.
- `erase(iterator)` or `erase(key)` removes entries.

```cpp
if (auto it = fruitCounts.find("apple"); it != fruitCounts.end()) {
    std::cout << "Apples tracked: " << it->second << '\n';
}

fruitCounts.erase("pear");
```

## Aggregating values

Combine map iteration with `<numeric>` to compute summaries:

```cpp
#include <numeric>

int total = std::accumulate(
    fruitCounts.begin(), fruitCounts.end(), 0,
    [](int sum, const auto& entry) { return sum + entry.second; });
```

You can also transform values into a vector for statistical analysis or export.

## Choosing the right associative container

- `std::map`: ordered, balanced tree, iterators stay valid after inserts, logarithmic complexity.
- `std::unordered_map`: hash table, average constant-time operations, no ordering, invalidates iterators on rehash.
- `std::multimap`: allows duplicate keys.

Choose based on whether deterministic ordering or maximal throughput matters most.

## Custom comparators

Provide a comparator type when you need non-standard ordering:

```cpp
#include <algorithm>
#include <cctype>

struct CaseInsensitiveLess {
    bool operator()(const std::string& lhs, const std::string& rhs) const {
        return std::lexicographical_compare(
            lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
            [](unsigned char a, unsigned char b) {
                return std::tolower(a) < std::tolower(b);
            });
    }
};

std::map<std::string, int, CaseInsensitiveLess> inventory;
```

Use `std::tolower` from `<cctype>` and guard conversions to avoid undefined behaviour.

## Practice time

1. **Inventory counter:** Read `item quantity` pairs until EOF, update a `std::map<std::string, int>`, and print totals sorted alphabetically.
2. **Frequency analysis:** Count character frequencies in a string using a map. Use `contains` or `find` to avoid default-inserting for read-only queries.
3. **Leaderboard:** Maintain a score map where inserting a new score updates the existing total if the player already exists. Print the map in reverse order using `std::map::reverse_iterator`.
4. **Case-insensitive lookup:** Implement the custom comparator above and verify that `"Apple"` and `"apple"` land in the same bucket.

## Self-check questions

1. What is the complexity of inserting into `std::map`, and why?
2. When would you prefer `at()` to `operator[]`?
3. How do you remove an entry safely while iterating?
4. What trade-offs push you toward `std::unordered_map` instead?
5. How does a custom comparator change iteration order, and what constraints must it satisfy?

Answer these before moving on to see how algorithms and other containers complement maps in real programs.
