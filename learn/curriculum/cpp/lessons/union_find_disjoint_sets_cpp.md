# Lesson 11.2: Union-Find and Disjoint Set Optimisation

Welcome back to the Data Structures and Algorithms module. This lesson dives deep into the disjoint-set (union-find) data structure that powers Kruskal's minimum spanning tree, dynamic connectivity checks, offline queries, image processing, and multiplayer matchmaking systems. We will unpack the mental model, prove the amortised complexity guarantees, and build a production-grade implementation complete with diagnostics and test harnesses. Expect to cross over 300 lines of guided exploration, hands-on practice, and actionable checklists.

## Learning goals

By the end of this session you will be able to:

- Explain the invariant maintained by the disjoint-set forest and how union and find interact.
- Compare naive grouping, tree-based union, and optimised union-by-rank with path compression.
- Implement a reusable `DisjointSetForest` class template in modern C++ with telemetry hooks.
- Use union-find to answer dynamic connectivity queries, detect cycles, and build MSTs.
- Reason about amortised complexity using Ackermann's inverse and capture performance counters in code.
- Diagnose common implementation bugs, including parent initialisation mistakes and rank underflows.

## Why union-find matters now

Priority queues gave us a way to schedule work. Union-find gives us a way to reason about connectivity and membership without scanning the entire dataset. When you reach graph algorithms, MST builders, or clustering pipelines, you need a data structure that answers: “Are these two elements currently in the same component?” in nearly constant time. Union-find delivers that guarantee when tuned correctly.

### Real-world signals that call for union-find

- **Networking**: union every time a connection comes online; query connectivity before routing packets.
- **Game servers**: map players to guilds/clans; unify after merges, query to prevent duplicate invitations.
- **Image processing**: unify neighbouring pixels when they share a threshold; components become segmented blobs.
- **Version control**: offline merge algorithms use union-find to detect cycles in dependency graphs.
- **Constraint solving**: DSU underpins incremental equivalence class maintenance in SAT/SMT engines.

## Study contract

To make the most out of the lesson, commit to the following contract:

1. **Notebook logging**: maintain a simple table with columns `step`, `parent array snapshot`, `rank array snapshot`, and `notes on path compression`. Fill it after each guided exercise.
2. **Telemetry-first mindset**: wrap `find` and `unite` in counters so you can see amortised gains, not just assume them.
3. **Repeat-theory cycle**: for each new optimisation, explain it in words, sketch the tree transformation, then code it.
4. **Practice parity**: compile every snippet, even the diagnostic macros. Seeing counters change cements understanding.
5. **Retrospective**: at the end, summarise three pitfalls you will avoid in future union-find implementations.

## 1. Building intuition

Picture each element sitting in a tree where edges always point toward the root representative. Two elements belong to the same set if their roots match. Union operations glue the roots together; find operations chase parent pointers up the tree. Initially, every node is its own parent (a singleton tree). The art lies in keeping these trees shallow.

### The baseline representation

```cpp
struct DisjointSetBaseline {
    std::vector<int> parent;

    explicit DisjointSetBaseline(int n) : parent(n) {
        for (int i = 0; i < n; ++i) {
            parent[i] = i; // each node starts as its own parent
        }
    }

    int find(int x) {
        while (parent[x] != x) {
            x = parent[x];
        }
        return x;
    }

    void unite(int a, int b) {
        int rootA = find(a);
        int rootB = find(b);
        if (rootA == rootB) return;
        parent[rootB] = rootA; // arbitrarily attach B to A
    }
};
```

This baseline is easy to reason about but quickly degenerates into tall, skinny trees when unions always attach the second argument under the first. That degeneracy yields `find` costs of `O(n)` in the worst case.

### Guided reflection

1. Draw the parent array after uniting `(0,1)`, `(1,2)`, `(2,3)`, `(3,4)` with the baseline structure.
2. Observe the depth of the tree containing `4`. How many steps does `find(4)` take?
3. Predict the asymptotic cost of a sequence of `m` finds and `n` unions. When does this matter in practice?

Document your answers before moving forward; we will revisit them after introducing optimisation techniques.

## 2. Union by rank

Union by rank (or union by size) ensures we always attach the shallower tree under the deeper tree. Ranks approximate tree height. Size-based union tracks number of elements per tree, reaching similar behaviour with sometimes easier reasoning.

```cpp
struct DisjointSetByRank {
    std::vector<int> parent;
    std::vector<int> rank;

    explicit DisjointSetByRank(int n) : parent(n), rank(n, 0) {
        for (int i = 0; i < n; ++i) {
            parent[i] = i;
        }
    }

    int find(int x) {
        while (parent[x] != x) {
            x = parent[x];
        }
        return x;
    }

    void unite(int a, int b) {
        int rootA = find(a);
        int rootB = find(b);
        if (rootA == rootB) return;

        if (rank[rootA] < rank[rootB]) {
            parent[rootA] = rootB;
        } else if (rank[rootA] > rank[rootB]) {
            parent[rootB] = rootA;
        } else {
            parent[rootB] = rootA;
            ++rank[rootA];
        }
    }
};
```

### Rank invariants

- Ranks only increase when two trees of equal rank merge.
- A tree of rank `r` has at least `2^r` nodes, bounding the height.
- Ranks never decrease.

### Mini-proof sketch

Use induction on rank. When ranks differ, the taller tree remains the root, so its height does not increase. When ranks tie, both have at least `2^r` nodes; merging them yields at least `2^(r+1)`, justifying the increment.

### Reflection checkpoint

- Re-run the union sequence `(0,1)`, `(1,2)`, `(2,3)`, `(3,4)` using union-by-rank. Record parent array and maximum depth.
- Compare find costs with the baseline. How many steps now? Document the improvement.

## 3. Path compression

Path compression flattens the tree during `find` by pointing each visited node directly to the root. Combined with union by rank, it guarantees almost-constant amortised time per operation.

```cpp
struct DisjointSetOptimised {
    std::vector<int> parent;
    std::vector<int> rank;

    explicit DisjointSetOptimised(int n) : parent(n), rank(n, 0) {
        for (int i = 0; i < n; ++i) {
            parent[i] = i;
        }
    }

    int find(int x) {
        if (parent[x] != x) {
            parent[x] = find(parent[x]); // path compression
        }
        return parent[x];
    }

    bool unite(int a, int b) {
        int rootA = find(a);
        int rootB = find(b);
        if (rootA == rootB) return false;

        if (rank[rootA] < rank[rootB]) {
            parent[rootA] = rootB;
        } else if (rank[rootA] > rank[rootB]) {
            parent[rootB] = rootA;
        } else {
            parent[rootB] = rootA;
            ++rank[rootA];
        }
        return true;
    }
};
```

### Complexity intuition

The amortised cost of `find` with path compression and union by rank is `α(n)`, where `α` is the inverse Ackermann function. For all realistic input sizes (even astronomical ones), `α(n) <= 4`. That means your connectivity queries essentially run in constant time after the initial warm-up operations.

### Visual intuition exercise

1. Start with ten singleton sets.
2. Union pairs in a chain (0-1, 1-2, ..., 8-9).
3. Run `find` on the last element twice while logging the parent array before and after each call.
4. Observe how the second call becomes trivial because the path collapsed.

## 4. Implementation walkthrough with diagnostics

Now we will build a reusable class that:

- Supports union, find, size queries, component counts, and connected checks.
- Provides optional telemetry counters for `find`, `path compression rewrites`, and `union merges`.
- Offers debug assertions to catch invalid node indices.

```cpp
#include <cassert>
#include <cstddef>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

class DisjointSetForest {
public:
    explicit DisjointSetForest(std::size_t size)
        : parent_(size), rank_(size, 0), component_size_(size, 1), components_(size) {
        for (std::size_t i = 0; i < size; ++i) {
            parent_[i] = i;
        }
    }

    std::size_t find(std::size_t node) {
        assert(node < parent_.size());
        if (parent_[node] != node) {
            parent_[node] = find(parent_[node]);
            ++path_compressions_;
        }
        ++find_calls_;
        return parent_[node];
    }

    bool unite(std::size_t a, std::size_t b) {
        auto rootA = find(a);
        auto rootB = find(b);
        if (rootA == rootB) return false;

        if (rank_[rootA] < rank_[rootB]) {
            std::swap(rootA, rootB);
        }

        parent_[rootB] = rootA;
        component_size_[rootA] += component_size_[rootB];
        component_size_[rootB] = 0;
        if (rank_[rootA] == rank_[rootB]) {
            ++rank_[rootA];
        }

        --components_;
        ++union_merges_;
        return true;
    }

    bool connected(std::size_t a, std::size_t b) {
        return find(a) == find(b);
    }

    std::size_t size(std::size_t node) {
        return component_size_[find(node)];
    }

    std::size_t components() const noexcept { return components_; }

    std::size_t find_calls() const noexcept { return find_calls_; }

    std::size_t path_compressions() const noexcept { return path_compressions_; }

    std::size_t union_merges() const noexcept { return union_merges_; }

private:
    std::vector<std::size_t> parent_;
    std::vector<std::size_t> rank_;
    std::vector<std::size_t> component_size_;
    std::size_t components_;
    std::size_t find_calls_ = 0;
    std::size_t path_compressions_ = 0;
    std::size_t union_merges_ = 0;
};
```

### Exercise: instrument and inspect

Compile the class and run the following snippet to watch counters tick:

```cpp
int main() {
    DisjointSetForest dsu(10);
    dsu.unite(0, 1);
    dsu.unite(1, 2);
    dsu.unite(3, 4);
    dsu.unite(2, 4);

    std::cout << "components: " << dsu.components() << '\n';
    std::cout << "find calls: " << dsu.find_calls() << '\n';
    std::cout << "path compressions: " << dsu.path_compressions() << '\n';
}
```

Run the program twice: first without extra `find` calls, then with a loop that queries connectivity of every pair. Compare the telemetry to confirm path compression effectiveness.

## 5. Using union-find inside Kruskal's algorithm

Kruskal's MST algorithm sorts edges by weight, then greedily includes them if they connect two different components.

```cpp
struct Edge {
    int u;
    int v;
    int weight;
};

int kruskal_mst(int node_count, std::vector<Edge> edges) {
    std::sort(edges.begin(), edges.end(), [](const Edge& lhs, const Edge& rhs) {
        return lhs.weight < rhs.weight;
    });

    DisjointSetForest dsu(static_cast<std::size_t>(node_count));
    int total_weight = 0;

    for (const auto& edge : edges) {
        if (dsu.unite(edge.u, edge.v)) {
            total_weight += edge.weight;
        }
    }

    return total_weight;
}
```

### Practice log

- Total edges processed.
- Number of edges accepted into the MST.
- Count of cycles prevented (edges rejected because the endpoints were already connected).

After running, inspect `dsu.union_merges()` versus `edges.size()`. The difference equals cycles prevented.

## 6. Offline query batching with DSU

Union-find shines when processing union operations offline in any order. Consider dynamic connectivity where edges only ever get added.

1. Receive a batch of union commands.
2. Interleave them with `connected` queries.
3. Process sequentially; DSU ensures each query is almost constant time.

### Pseudocode plan

- Parse events into a vector of `std::variant<Union, ConnectedQuery>`.
- Maintain DSU as you iterate.
- Collect boolean answers for `connected` queries.
- Optionally emit debug snapshots of component counts after every operation.

Challenge yourself to implement this pipeline and instrument with telemetry counters. Verify that query complexity stays near constant, even for tens of thousands of operations.

## 7. Failure modes and debugging checklist

- **Forgot to initialise parents**: confirm each `parent[i] = i` before unions.
- **Union without find**: never assign parents directly; always compress roots first.
- **Rank/size vector mismatch**: ensure `rank_` and `component_size_` share the same length as `parent_`.
- **Negative sizes**: watch for unsigned underflow when subtracting; prefer using `std::size_t` consistently.
- **Path compression missing**: if `find_calls()` skyrockets while `path_compressions()` stays low, verify compression recursion executes.
- **Disconnected telemetry**: counters should reset on `clear()` method; implement if you plan to reuse DSU across batches.

Log each issue you encounter, the symptom, and the fix applied. Keeping a running log will accelerate your future debugging sessions.

## Practice Time

Take 40–50 minutes to solidify the concepts with the following guided routine:

1. **Warm-up implementation**: Recreate `DisjointSetForest` from scratch in a new file `union_find_lab.cpp`.
2. **Instrumentation**: Add public getters for `find_calls`, `union_merges`, and `path_compressions`. Ensure they reset in the constructor.
3. **Benchmark harness**: Generate 100,000 random union pairs and 100,000 connectivity queries. Time the run with `<chrono>` and log the counters.
4. **Scenario analysis**: Compare three strategies: baseline (no compression), union-by-rank only, and full optimisation. Graph the counts in your notebook.
5. **Edge-case drill**: Call `connected` on pairs outside the range and confirm assertions catch the misuse. Document the protective techniques you add.
6. **Reflection prompt**: Summarise in a paragraph how amortised analysis differs from worst-case analysis.

Record results directly in your study journal. Screenshots are optional; the key goal is describing the delta between strategies with actual measurements.

## 9. Self-check questions

1. Why does path compression require recursion (or explicit stack) to work correctly?
2. How does union-by-size differ from union-by-rank? When might one be simpler to instrument?
3. Describe in plain language what `α(n)` represents and why it matters in DSU complexity proofs.
4. How do you detect a cycle using union-find during graph traversal?
5. What telemetry counters reveal when path compression is misconfigured?
6. Why must you update component sizes after attaching one root to another?
7. Outline how persistent DSU variants differ from the classic destructively updated version.
8. When merging two clans in an online game, which DSU methods do you call and what invariants must hold afterward?
9. How can DSU assist in percolation simulations on grids?
10. Explain why union-find is a poor fit for tracking shortest paths, even though it excels at connectivity.

Answer each prompt in writing, then cross-check with a peer or the solution notes.

## 10. Mini project: Network partition simulator

Build a command-line tool that simulates data-centre racks going offline and online. Requirements:

- Maintain DSU across `N` servers, grouped by rack.
- Support commands:
  - `LINK a b`: connect two servers and report the new component size.
  - `CHECK a b`: output whether the servers share connectivity.
  - `SPLIT rack_id`: remove all edges within a rack by resetting parents (simulate maintenance windows).
- Track metrics: number of components over time, largest component size, average path compression events per command.
- Persist a journal file summarising metrics every 100 commands.

Stretch goals:

- Add `UNDO` by storing a stack of merges (consider implementing a rollback DSU variant with union history).
- Visualise component size distribution using ASCII histograms.
- Export Prometheus-style metrics for integration with monitoring pipelines.

## 11. Reflection checklist

- [ ] I can implement union-find with union-by-rank and path compression without referencing external code.
- [ ] I can explain to a teammate why DSU is nearly constant time using amortised analysis.
- [ ] I have instrumentation ready to measure find/union statistics.
- [ ] I know how to embed DSU inside Kruskal's MST and dynamic connectivity workflows.
- [ ] I logged at least three debugging pitfalls and their resolution steps.

## 12. Summary

Disjoint-set forests deliver blazing-fast connectivity queries when tuned with union-by-rank and path compression. You now possess both the theoretical background and practical instrumentation to employ DSU confidently in production-grade systems. Keep the telemetry mindset, retain your debugging checklist, and prepare for the next lesson, where we contrast DSU with alternative structures like binary indexed trees and segment trees.
