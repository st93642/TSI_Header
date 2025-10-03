# Lesson 11.1: Heaps and Priority Queues

> *Goal:* master binary heaps and `std::priority_queue` in modern C++, analyze their complexity, and learn how to integrate them into algorithmic workflows like Dijkstra's shortest path and streaming statistics.

## 1. Why heaps matter

Heaps power the operations that demand "give me the most important element right now" in logarithmic time. They sit behind schedulers, A* pathfinding, job queues, and every competitor to the classic stack/queue pair. Understanding heaps is essential before taking on graph algorithms or event-driven systems.

### 1.1 A real-world hook

Imagine you are building a ride-sharing dispatcher. Thousands of drivers bubble up availability updates every second. Riders expect the nearest driver instantly. A naive linear scan takes too long; a balanced tree is workable, but a heap gives you the sweet spot between speed and implementation effort.

### 1.2 Terminology snapshot

- **Heap property**: every parent either dominates or is dominated by its children.
- **Max-heap**: parents are greater-than or equal to children.
- **Min-heap**: parents are less-than or equal to children.
- **Complete binary tree**: every level is filled left-to-right (except possibly the last).
- **Priority queue**: a container adapter that surfaces the highest-priority element first.

### 1.3 Visualizing the structure

![Max-heap](https://www.programiz.com/sites/tutorial2program/files/maxheap_1.png)
![Min-heap](https://www.programiz.com/sites/tutorial2program/files/minheap_0.png)

These visuals from Programiz highlight that the tree shape is rigid (complete) but the values enforce the heap property. We'll re-use them when deriving array indices.

## 2. Array-backed binary heap theory

### 2.1 Index arithmetic

A complete binary tree maps cleanly into a contiguous array:

```text
index:     0   1   2   3   4   5   6
values:   42  29  18  14  7   18  12
```

- Parent of node `i`: `(i - 1) / 2` (integer division).
- Left child: `2 * i + 1`.
- Right child: `2 * i + 2`.
- No sentinel nodes, just contiguous storage.

Keep this mapping handy; it makes the core algorithms simple loops over indices.

### 2.2 Heapify (sift-down)

A heap emerges after repeatedly enforcing the heap property from the bottom up.

```cpp
void heapify(std::vector<int>& arr, std::size_t n, std::size_t index) {
    std::size_t largest = index;
    std::size_t left = 2 * index + 1;
    std::size_t right = 2 * index + 2;

    if (left < n && arr[left] > arr[largest]) {
        largest = left;
    }

    if (right < n && arr[right] > arr[largest]) {
        largest = right;
    }

    if (largest != index) {
        std::swap(arr[index], arr[largest]);
        heapify(arr, n, largest);
    }
}
```

This exact process mirrors the pseudocode from the Programiz "Heapify" section. Note the recursion: after swapping, you need to re-heapify the affected subtree.

### 2.3 Building a heap in O(n)

```cpp
void build_max_heap(std::vector<int>& arr) {
    if (arr.empty()) return;
    for (std::size_t i = arr.size() / 2; i-- > 0;) {
        heapify(arr, arr.size(), i);
    }
}
```

- Starting at `size / 2 - 1` gives the last non-leaf node.
- Complexity: `O(n)` despite the nested loop because shorter heapify cost near leaves.
- Works for both max-heap (using `>` comparisons) and min-heap (using `<`).

### 2.4 Sift-up for insertions

```cpp
void heap_insert(std::vector<int>& arr, int value) {
    arr.push_back(value);
    std::size_t index = arr.size() - 1;

    while (index > 0) {
        std::size_t parent = (index - 1) / 2;
        if (arr[parent] >= arr[index]) {
            break;
        }
        std::swap(arr[parent], arr[index]);
        index = parent;
    }
}
```

### 2.5 Remove top (delete max)

```cpp
int heap_extract_max(std::vector<int>& arr) {
    if (arr.empty()) {
        throw std::runtime_error("heap is empty");
    }
    int top = arr.front();
    arr.front() = arr.back();
    arr.pop_back();
    if (!arr.empty()) {
        heapify(arr, arr.size(), 0);
    }
    return top;
}
```

### 2.6 Algorithmic complexity summary

<!-- markdownlint-disable-next-line MD033 -->
<mark>Remember:</mark> complexity discusses the dominant term as the heap grows; the constants disappear because of the logarithmic structure of the tree.

- **Build (heapify bottom-up)** — $O(n)$. All leaf layers (roughly half the nodes) already satisfy the heap property, so only the upper levels require swaps.
- **Insert** — $O(\log n)$. We potentially travel from a leaf toward the root, climbing at most the height of the heap.
- **Extract max/min** — $O(\log n)$. After removing the top, we sift the last element downward level by level until the property is restored.
- **Peek top** — $O(1)$. Accessing the root is constant-time because it sits at index `0`.
- **Search arbitrary value** — $O(n)$. Without additional indexing, we may have to inspect every element.

<!-- markdownlint-disable-next-line MD033 -->
Because each level roughly doubles the node count (≈ $2^{h}$), the heap height `h` satisfies $h = \lfloor \log_{2} n \rfloor$. That logarithmic height is exactly why insertions and deletions do not explode in cost.

### 2.7 Mental walkthrough

1. Set an index `i` to the new node.
2. <!-- markdownlint-disable-next-line MD033 -->Compare its key with its parent at `i<sub>parent</sub>` and swap if the heap-order property is violated.
3. Repeat until you either reach the root or the parent key already dominates `i`.

This read-through gives beginners an intuitive script to follow before diving into code.

## 3. `std::priority_queue` deep dive

The C++ Standard Library supplies a heap-backed container adaptor in `<queue>`.

### 3.1 Template definition

```cpp
template <
    class T,
    class Container = std::vector<T>,
    class Compare = std::less<typename Container::value_type>
> class priority_queue;
```

Pulled from cppreference, the defaults give you a max-heap (largest element on `top()`). You can swap in `std::greater<T>` or a custom comparator to turn it into a min-heap or change ordering semantics.

### 3.2 Key member functions

- **Element access** — `top()` returns the highest-priority element without popping it.
- **Capacity** — `empty()` and `size()` answer state questions in constant time.
- **Modifiers** — `push`, `emplace`, and `pop` adjust the heap while maintaining ordering; `swap` exchanges adaptor state in $O(1)$.

<!-- markdownlint-disable-next-line MD033 -->
Whenever you write `push`, keep the formula `i<sub>parent</sub> = (i - 1) / 2` in mind so you can reason about the ascent; the matching children follow `i<sub>left</sub> = 2i + 1` and `i<sub>right</sub> = 2i + 2`.

<!-- markdownlint-disable-next-line MD033 -->
Also note how the breadth of each level expands: the number of nodes at depth `d` is at most $2^{d}$, so only near the leaves does the heap contain wide layers.

### 3.3 Underlying container options

- `std::vector<T>` (default): contiguous storage, fastest typical usage.
- `std::deque<T>`: works if you need stable references when pushing/popping.
- Requirements: random access iterators plus `front`, `push_back`, `pop_back` semantics.

### 3.4 Building min-heaps with comparators

```cpp
std::priority_queue<int, std::vector<int>, std::greater<int>> min_heap;
for (int value : {5, 1, 9, 3}) {
    min_heap.push(value);
}
// top() now returns 1
```

C++20 and later let you use class template argument deduction:

```cpp
std::priority_queue min_heap2(std::greater<int>{}, std::vector<int>{});
```

### 3.5 Custom comparator example from cppreference

```cpp
struct CompareXor {
    bool operator()(int left, int right) const {
        return (left ^ 1) < (right ^ 1);
    }
};

std::priority_queue<int, std::vector<int>, CompareXor> heap;
```

Use cases:

- Packets prioritized by bit-pattern features.
- Puzzle solvers that experiment with heuristics.

### 3.6 Exception safety and `constexpr`

As of C++26, many priority_queue member functions are `constexpr`. However, storing elements dynamically still prevents fully constexpr heaps unless all allocations can be resolved at compile time.

### 3.7 Common pitfalls

1. **Modifying top element directly**: altering `top()` without re-heapifying breaks invariants. Use `pop()` + `push()`.
2. **Iterating**: no direct iteration API—copy to a temporary container or build two heaps.
3. **Bulk updates**: prefer `std::make_heap`, `std::push_heap`, `std::pop_heap` on raw containers when you control the storage.

## 4. Implementing heaps manually vs using adapters

### 4.1 When to roll your own

- Need `decrease_key` or `update_key` (classical binary heap lacks direct support).
- Want to store additional metadata (timestamps, handles) tightly coupled with nodes.
- Need to export heap structure for visualization or instrumentation.

### 4.2 When to use `std::priority_queue`

- No need to touch arbitrary nodes.
- Priorities never need to be updated in place.
- Focus is on algorithm correctness, not custom infrastructure.

### 4.3 Alternative containers

- `std::multiset`: logarithmic insert/remove with ordered iteration.
- `boost::heap::fibonacci_heap`: advanced operations, including decrease-key.
- `std::ranges::heap` algorithms (C++23+): make/adapt heaps with ranges.

## 5. Algorithmic workflows

### 5.1 Dijkstra's shortest path with `std::priority_queue`

```cpp
struct Edge { std::size_t to; int weight; };
using Graph = std::vector<std::vector<Edge>>;

std::vector<int> dijkstra(const Graph& graph, std::size_t source) {
    const int INF = std::numeric_limits<int>::max();
    std::vector<int> distance(graph.size(), INF);
    distance[source] = 0;

    using Item = std::pair<int, std::size_t>; // (distance, node)
    auto cmp = [](const Item& lhs, const Item& rhs) {
        return lhs.first > rhs.first; // min-heap on distance
    };

    std::priority_queue<Item, std::vector<Item>, decltype(cmp)> pq(cmp);
    pq.push({0, source});

    while (!pq.empty()) {
        auto [dist, node] = pq.top();
        pq.pop();
        if (dist > distance[node]) {
            continue; // stale entry
        }
        for (const auto& edge : graph[node]) {
            int next = dist + edge.weight;
            if (next < distance[edge.to]) {
                distance[edge.to] = next;
                pq.push({next, edge.to});
            }
        }
    }

    return distance;
}
```

Key points:

- Push duplicates instead of updating in place.
- Skip stale entries by comparing popped distance with the recorded one.
- Complexity: `O((V + E) log V)`.

### 5.2 Top-k streaming statistics

```cpp
template <typename T>
std::vector<T> keep_top_k(typename std::vector<T>::const_iterator begin,
                          typename std::vector<T>::const_iterator end,
                          std::size_t k) {
    if (k == 0) return {};
    std::priority_queue<T, std::vector<T>, std::greater<T>> min_heap;

    for (auto it = begin; it != end; ++it) {
        if (min_heap.size() < k) {
            min_heap.push(*it);
        } else if (*it > min_heap.top()) {
            min_heap.pop();
            min_heap.push(*it);
        }
    }

    std::vector<T> result;
    result.reserve(min_heap.size());
    while (!min_heap.empty()) {
        result.push_back(min_heap.top());
        min_heap.pop();
    }
    std::reverse(result.begin(), result.end());
    return result;
}
```

Technique: keep a min-heap of the current top `k` elements and evict the smallest whenever a new contender arrives.

### 5.3 Event simulation scheduling

- Use a min-heap keyed by timestamp.
- Each event pops from the heap, processes, and pushes follow-up events.
- Complexity: `O(n log n)` for `n` events, assuming bounded fan-out.

## 6. Complexity reasoning and proof sketch

### 6.1 Heap insertion

- Each swap moves the inserted element up one level.
- Maximum height of a complete binary tree is `⌊log₂ n⌋`.
- Therefore, sift-up is `O(log n)`.

### 6.2 Extract max

- Removing the root, replacing with last element, and heapifying downward spends constant work at each level.
- Height again `O(log n)`.

### 6.3 Build heap from arbitrary array

- Half the elements are leaves: zero work.
- Quarter of elements one level above leaves: `O(1)` work each.
- Summing geometric series yields `O(n)`.

### 6.4 Priority queue adapter

`std::priority_queue` uses the same complexity: `push` and `pop` are logarithmic; `top` is constant-time.

## 7. Guided hands-on practice

### 7.1 Setup checklist

- Ensure you have a C++17 (or later) compiler available (`g++`, `clang++`, MSVC).
- Create a fresh file `heap_demo.cpp` for experiments.
- Add these headers to the top of your scratch file:

```cpp
#include <queue>
#include <vector>
#include <functional>
#include <iostream>
```

<!-- markdownlint-disable-next-line MD033 -->
- Launch your build task with <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>B</kbd> so you can iterate without leaving the editor.

### 7.2 Step-by-step exercise walkthrough

1. **Warm-up**: Build a `std::priority_queue<int>` with five integers. Print `top()`, then pop all elements to verify max-heap ordering.
2. **Min-heap variant**: Rebuild with `std::greater<int>` and observe ascending order pops.
3. **Custom struct**: Define `struct Task { int priority; std::string label; };` and push tasks with a comparator that returns true when `lhs.priority < rhs.priority` to create a max-heap by priority.
4. **Stale entries**: For Dijkstra-style logic, push duplicate entries with worse priorities and confirm you can detect stale nodes using an `if (dist > best)` guard.
5. **Bulk insertion**: Use the constructor `std::priority_queue<int>(begin, end)` and confirm the heap builds in linear time compared to pushing each item individually.
6. **Performance measurement** *(optional)*: Time insertion of one million integers with `push` vs `std::make_heap` over a vector followed by repeatedly calling `std::pop_heap`.

### 7.3 Self-check prompts

- When does `std::priority_queue` default to a max-heap? How do you invert it?
- Why is `std::priority_queue` missing iteration? How can you iterate nonetheless?
- In Dijkstra's algorithm, why are stale entries acceptable, and how do you guard against them?
- How do you implement decrease-key with only `std::priority_queue`? (Trick question: you simulate it by pushing duplicates.)

### 7.4 Stretch goal

Implement an indexable priority queue using a custom heap wrapper that stores `(key, handle)` pairs and supports a `decrease_key` method. Compare its interface with `boost::heap::d_ary_heap`.

## 8. Troubleshooting patterns

### 8.1 Heap obfuscated by duplicates

- Symptom: `top()` shows a value that logically should have been removed.
- Fix: ensure you pop stale entries triggered by older `push` operations.
- Diagnostic: log `(priority, payload)` when popping; duplicates indicate stale data.

### 8.2 Comparator confusion

- Symptom: `std::priority_queue` behaves like a min-heap when you expected a max-heap (or vice versa).
- Fix: remember the comparator defines whether `lhs` comes before `rhs`. Because the adapter returns the element considered *last* by the comparator, invert your logic accordingly.
- Diagnostic: unit test your comparator with a few sample pairs.

### 8.3 Heap becomes unordered after manual modifications

- Symptom: After mutating the internal container directly, priorities become inconsistent.
- Fix: never call `container()` or implicitly expose the underlying container. Instead, rebuild using `std::vector` + `<algorithm>` heap utilities where direct modification is necessary.

### 8.4 Memory pressure

- Symptom: Frequent `push`/`pop` cause reallocations.
- Fix: switch underlying container to `std::deque`, or call `c.reserve(expectedSize)` via a wrapper to preallocate.

## 9. Summary and next steps

- Heaps represent a tight marriage of theoretical guarantees and practical performance.
- `std::priority_queue` gives ready-to-use heap power with minimal ceremony.
- Manual heaps offer more control when you need custom operations like `decrease_key`.
- Algorithms like Dijkstra's or any top-k streaming scenario rely heavily on heaps.
- Up next in this module: lazy deletion strategies, indexed priority queues, and amortized analysis of advanced heap families.

## 10. Practice: hands-on drill

Follow these steps before taking the quiz:

1. Implement `keep_top_k` for integers and run it against test arrays of sizes 10, 1000, 100000. Record timing.
2. Modify the function to store `(score, id)` pairs. Sort the resulting vector by `score` descending.
3. Take your ride-sharing dispatcher scenario and write pseudo-code that matches driver priority updates using duplicates + stale detection.
4. Compare `std::priority_queue` with `std::multiset` in terms of API friction. Write a table in your notes summarizing insert/remove complexity and feature sets.
5. Sketch out (on paper or a whiteboard) how `heapify` walks through the array for the sample `[27, 15, 19, 8, 12, 32, 4]`.

## 11. Concept integration map

- Link **Section 2** (array-based heap mechanics) with **Section 5** (algorithmic workflows) by tracing how `heapify` underpins both the top-k helper and Dijkstra's queue maintenance.
- Tie the comparator guidance in **Section 3.4** back to the stale-entry strategy from **Section 5.1** so you can reason about min-heap adapters without guessing.
- Use the troubleshooting checklist in **Section 8** after each practice run from **Section 7** to debug misbehaving heaps without external references.
- Revisit the complexity recap in **Section 6** whenever you justify why a heap is the right abstraction for a new problem.

## 12. Reflection checklist

- [ ] I can explain in plain words how heapify maintains the heap property.
- [ ] I can implement a min-heap using `std::priority_queue` without searching online.
- [ ] I can justify the `O((V + E) log V)` runtime of Dijkstra's algorithm with a binary heap.
- [ ] I know why `std::priority_queue` lacks iterators and how to iterate safely anyway.
- [ ] I have practiced rewriting the comparator to represent business-specific priority.

## 13. Challenge questions

1. How would you extend a binary heap to support `decrease_key`? Sketch the data structure.
2. If you replace the comparator with one that uses `std::tie(priority, timestamp)`, what new capability do you gain?
3. Why might you choose a `d-ary` heap over a binary heap for Dijkstra on dense graphs?
4. With C++26 adding `constexpr` support to `std::priority_queue`, what compile-time use cases become possible?
5. Suppose you need to merge `k` sorted lists continuously. How does a heap help, and what is the complexity?

## 14. Quiz preparation pointers

The upcoming quiz asks conceptual and scenario-based questions. Make sure you:

- Revisit the definitions of `heapify`, `sift-up`, and `sift-down`.
- Memorize the index math for array-backed heaps.
- Understand the comparator inversion rule for `std::priority_queue`.
- Practice reading `std::priority_queue` constructor signatures without confusion.
- Distinguish between algorithmic guarantees (e.g., `push` is logarithmic) and usage caveats (no direct iteration).

Keep this lesson open as a reference while attempting the quiz. The diagrams and code fragments will help you reason through the answer choices.
