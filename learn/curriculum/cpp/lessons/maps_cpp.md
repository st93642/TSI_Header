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

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: `std::map` in Real Projects

This appendix contains examples for initializing maps from files, serializing small maps for tests, and an HTML table comparing associative containers.

### Loading a map from a file

```cpp
#include <fstream>
#include <sstream>

std::map<std::string, int> load_counts(const std::string& path) {
    std::map<std::string, int> out;
    std::ifstream in(path);
    std::string line;
    while (std::getline(in, line)) {
        std::istringstream iss(line);
        std::string key; int value;
        if (iss >> key >> value) out[key] = value;
    }
    return out;
}
```

### Small CI snippet (CMake + Catch2)

```cmake
add_executable(map_tests tests/map_tests.cpp)
find_package(Catch2 REQUIRED)
target_link_libraries(map_tests PRIVATE Catch2::Catch2WithMain)
add_test(NAME map-tests COMMAND map_tests)
```

### Associative containers comparison (HTML)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Container</th><th>Ordering</th><th>Average Ops</th></tr>
  </thead>
  <tbody>
    <tr><td>std::map</td><td>Ordered</td><td>O(log n)</td></tr>
    <tr><td>std::unordered_map</td><td>Unordered</td><td>Average O(1)</td></tr>
    <tr><td>std::multimap</td><td>Ordered with duplicates</td><td>O(log n)</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises

1. Parse a two-column text file into a `std::map<std::string,int>` and then print entries sorted by value.
2. Compare `std::map` vs `std::unordered_map` performance for a generated workload of 10k keys.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Maps — Ordered vs Unordered

This appendix compares `std::map` and `std::unordered_map`, hash function choices, collision impacts, and when to use each.

```cpp
#include <unordered_map>
std::unordered_map<std::string,int> counts;
counts.reserve(1024);
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Map Type</th><th>Lookup</th><th>When to use</th></tr>
  </thead>
  <tbody>
    <tr><td>std::map</td><td>O(log n)</td><td>Ordered iteration, stable iterators</td></tr>
    <tr><td>std::unordered_map</td><td>Average O(1)</td><td>Fast lookup, no ordering</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Practical Appendix)

1. Benchmark `std::map` vs `std::unordered_map` with 1M keys (measure insert & lookup).
2. Implement a custom hash for a composite key and validate distribution with histograms.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Maps — Resources (Appendix — External Links)

Links and notes on `std::map` vs `std::unordered_map` with complexity notes.

- cppreference map: [std::map reference](https://en.cppreference.com/w/cpp/container/map)
- cppreference unordered_map: [std::unordered_map reference](https://en.cppreference.com/w/cpp/container/unordered_map)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Container</th><th>Link</th><th>Performance</th></tr>
  </thead>
  <tbody>
    <tr><td>std::map</td><td><a href="https://en.cppreference.com/w/cpp/container/map">map</a></td><td>Logarithmic time (ordered)</td></tr>
    <tr><td>std::unordered_map</td><td><a href="https://en.cppreference.com/w/cpp/container/unordered_map">unordered_map</a></td><td>Average constant time (hash-based)</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Replace a `std::map` with `std::unordered_map` and measure lookup times for large datasets.
2. Demonstrate how to provide a custom hash for a user-defined type and test correctness.

<!-- markdownlint-enable MD010 -->

## Practical Appendix: External Tools & Examples (Appendix — External Tools — maps_cpp-appendix)

Compact notes for `std::map`, lookup semantics, and example usages; references point to cppreference for `std::map`.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Why</th><th>Link</th></tr>
  </thead>
  <tbody>
    <tr><td>std::map</td><td>Ordered associative container</td><td><a href="https://en.cppreference.com/w/cpp/container/map">cppreference: map</a></td></tr>
    <tr><td>Accessors</td><td>operator[] vs at()</td><td>operator[] inserts if missing; at() throws</td></tr>
    <tr><td>Node handles</td><td>Extract/merge (C++17)</td><td>See node_handle APIs</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: basic usage

```cpp
#include <map>
#include <string>
#include <iostream>

int main(){
    std::map<std::string,int> m{{"a",1},{"b",2}};
    m["c"] = 3;
    for(const auto& [k,v] : m) std::cout << k << ":" << v << '\n';
}
```

### Exercises (maps_cpp-appendix)

1. Demonstrate the difference between `operator[]` and `at()` when accessing keys that do not exist.
2. Use `extract` and `merge` APIs to move nodes between two maps (C++17+).
<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Maps — Patterns & Tests (Appendix — maps_cpp-appendix2)

A compact set of recipes for testing map-based code, serialising small maps for fixtures, and choosing the right associative container.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Task</th><th>Pattern</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr><td>Load from file</td><td>parse lines into key/val</td><td>use std::istringstream</td></tr>
    <tr><td>Serialize</td><td>dump to JSON or key=value</td><td>use small writer or test fixture</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### CI snippet (CMake + Catch2)

```cmake
add_executable(map_tests tests/map_tests.cpp)
find_package(Catch2 REQUIRED)
target_link_libraries(map_tests PRIVATE Catch2::Catch2WithMain)
add_test(NAME map-tests COMMAND map_tests)
```

### Quick test example (Catch2)

```cpp
#include <catch2/catch.hpp>
#include <map>

TEST_CASE("map insert and find") {
    std::map<std::string,int> m;
    m["a"] = 1;
    REQUIRE(m.at("a") == 1);
}
```

### Exercises (Appendix — maps_cpp-appendix2)

1. Write a unit test that verifies reading a file into a map produces the expected sorted output.
2. Replace `std::map` with `std::unordered_map` in a benchmark and measure insert & lookup times.

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
