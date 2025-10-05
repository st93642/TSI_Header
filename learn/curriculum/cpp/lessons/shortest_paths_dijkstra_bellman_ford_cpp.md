# Lesson 11.4: Shortest Paths Algorithms (Dijkstra and Bellman-Ford)

## Introduction

In the world of graph algorithms, finding the shortest path between two points is a fundamental problem with countless real-world applications. Whether you're navigating a road network, routing data packets through the internet, or optimizing supply chains, shortest path algorithms provide the mathematical foundation for efficient decision-making.

This lesson explores two cornerstone algorithms for computing shortest paths in weighted graphs: Dijkstra's algorithm and the Bellman-Ford algorithm. While both solve similar problems, they handle different types of graphs and have distinct computational characteristics.

## Learning Objectives

By the end of this lesson, you will be able to:

- Understand the shortest path problem in weighted graphs
- Implement and analyze Dijkstra's algorithm for graphs with non-negative weights
- Implement and analyze the Bellman-Ford algorithm for graphs with negative weights
- Compare the strengths and limitations of both algorithms
- Apply these algorithms to solve real-world optimization problems

## The Shortest Path Problem

### Problem Definition

Given a weighted graph G = (V, E) with vertices V and edges E, where each edge has a weight w(u,v), the shortest path problem seeks to find a path from a source vertex s to a destination vertex t that minimizes the sum of edge weights along the path.

### Types of Shortest Path Problems

1. **Single-Source Shortest Paths (SSSP)**: Find shortest paths from one source to all other vertices
2. **Single-Pair Shortest Path**: Find shortest path between two specific vertices
3. **All-Pairs Shortest Paths**: Find shortest paths between all pairs of vertices

### Graph Representations

Before diving into algorithms, let's review how graphs can be represented in C++:

```cpp
// Adjacency List with weights
using Graph = vector<vector<pair<int, int>>>; // {neighbor, weight}

// Example graph representation
Graph createExampleGraph() {
    Graph g(5);
    g[0] = {{1, 4}, {2, 1}};     // 0 -> 1 (weight 4), 0 -> 2 (weight 1)
    g[1] = {{2, 2}, {3, 5}};     // 1 -> 2 (weight 2), 1 -> 3 (weight 5)
    g[2] = {{1, 2}, {3, 8}, {4, 10}}; // 2 -> 1, 3, 4
    g[3] = {{4, 2}};             // 3 -> 4 (weight 2)
    g[4] = {};                   // 4 has no outgoing edges
    return g;
}
```

## Dijkstra's Algorithm

### Overview

Dijkstra's algorithm, developed by Edsger W. Dijkstra in 1956, solves the single-source shortest paths problem for graphs with non-negative edge weights. The algorithm uses a greedy approach, always selecting the vertex with the smallest tentative distance from the source.

### Key Properties

- **Works only with non-negative weights**: All edge weights must be ≥ 0
- **Greedy strategy**: Always processes the closest unprocessed vertex
- **Optimal for non-negative graphs**: Guaranteed to find correct shortest paths

### Algorithm Steps

1. Initialize distances: dist[s] = 0, dist[v] = ∞ for all other vertices
2. Create a priority queue containing all vertices
3. While priority queue is not empty:
   - Extract vertex u with minimum distance
   - For each neighbor v of u:
     - If dist[v] > dist[u] + w(u,v):
       - Update dist[v] = dist[u] + w(u,v)
       - Update priority queue

### C++ Implementation

```cpp
#include <vector>
#include <queue>
#include <limits>

using namespace std;

const int INF = numeric_limits<int>::max();

vector<int> dijkstra(const Graph& g, int source) {
    int n = g.size();
    vector<int> dist(n, INF);
    dist[source] = 0;

    // Min-heap: {distance, vertex}
    priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> pq;
    pq.push({0, source});

    while (!pq.empty()) {
        auto [cost, u] = pq.top();
        pq.pop();

        // Skip if we already found a better path
        if (cost > dist[u]) continue;

        for (auto [v, w] : g[u]) {
            if (dist[v] > dist[u] + w) {
                dist[v] = dist[u] + w;
                pq.push({dist[v], v});
            }
        }
    }

    return dist;
}
```

### Time Complexity Analysis

- **Binary Heap Implementation**: O((V + E) log V)
- **Fibonacci Heap**: O(E + V log V) - theoretically better but complex to implement
- **Space Complexity**: O(V) for distance array and priority queue

### Example Execution

Consider this graph:

```text
  (0) --4-- (1) --5-- (3)
   | \       |       /
   |1 \      |2     /2
   |   \     |     /
  (2) --8----(4)
```

Starting from vertex 0:

**Initial state:**

- dist[0] = 0, dist[1] = ∞, dist[2] = ∞, dist[3] = ∞, dist[4] = ∞

**Step 1:** Process vertex 0

- Update dist[1] = 4, dist[2] = 1
- Priority queue: {1,2}, {4,1}

**Step 2:** Process vertex 2 (smallest distance)

- Update dist[1] = min(4, 1+8) = 4 (no change)
- Update dist[4] = 1+10 = 11
- Priority queue: {4,1}, {11,4}

**Step 3:** Process vertex 1

- Update dist[3] = 4+5 = 9
- Priority queue: {9,3}, {11,4}

**Step 4:** Process vertex 3

- Update dist[4] = min(11, 9+2) = 9+2 = 11 (no change)

**Final distances:** [0, 4, 1, 9, 11]

## Bellman-Ford Algorithm

### Bellman-Ford Overview

The Bellman-Ford algorithm, developed by Richard Bellman and Lester Ford, solves the single-source shortest paths problem for graphs that may contain negative edge weights. Unlike Dijkstra's algorithm, Bellman-Ford can detect negative cycles and handle negative weights correctly.

### Bellman-Ford Key Properties

- **Handles negative weights**: Works with graphs containing negative edge weights
- **Detects negative cycles**: Can identify if a graph contains negative-weight cycles
- **Dynamic programming approach**: Uses relaxation over multiple iterations

### Bellman-Ford Algorithm Steps

1. Initialize distances: dist[s] = 0, dist[v] = ∞ for all other vertices
2. Relax all edges |V| - 1 times:
   - For each edge (u,v) with weight w:
     - If dist[v] > dist[u] + w:
       - dist[v] = dist[u] + w
3. Check for negative cycles:
   - If any distance can still be updated, graph contains negative cycle

### Bellman-Ford C++ Implementation

```cpp
vector<int> bellmanFord(const Graph& g, int source) {
    int n = g.size();
    vector<int> dist(n, INF);
    dist[source] = 0;

    // Relax all edges |V| - 1 times
    for (int i = 0; i < n - 1; ++i) {
        for (int u = 0; u < n; ++u) {
            if (dist[u] == INF) continue;
            for (auto [v, w] : g[u]) {
                if (dist[v] > dist[u] + w) {
                    dist[v] = dist[u] + w;
                }
            }
        }
    }

    // Check for negative cycles
    for (int u = 0; u < n; ++u) {
        if (dist[u] == INF) continue;
        for (auto [v, w] : g[u]) {
            if (dist[v] > dist[u] + w) {
                // Negative cycle detected
                return {}; // Return empty vector to indicate negative cycle
            }
        }
    }

    return dist;
}
```

### Bellman-Ford Time Complexity Analysis

- **Time Complexity**: O(V × E) - polynomial time
- **Space Complexity**: O(V) for distance array
- **Worst case**: Dense graphs with many edges

### Handling Negative Cycles

If a graph contains a negative cycle reachable from the source, the algorithm can detect this during the final check. In such cases, shortest paths are undefined because you can reduce the path cost arbitrarily by traversing the cycle.

## Comparison of Algorithms

### Dijkstra vs Bellman-Ford

Aspect: Edge Weights

- Dijkstra: Non-negative only
- Bellman-Ford: Can be negative

Aspect: Negative Cycles

- Dijkstra: Cannot detect
- Bellman-Ford: Can detect

Aspect: Time Complexity

- Dijkstra: O((V+E) log V)
- Bellman-Ford: O(V × E)

Aspect: Space Complexity

- Dijkstra: O(V)
- Bellman-Ford: O(V)

Aspect: Optimality

- Dijkstra: Greedy optimal
- Bellman-Ford: Dynamic programming

### Visual Comparison

```
Dijkstra's Algorithm Flow:
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Initialize    │ -> │  Priority Queue │ -> │   Extract Min   │
│  dist[source]=0 │    │   with source   │    │   Process node  │
│  others=∞       │    │                 │    │   Update neighbors│
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
    Greedy Choice          Relaxation of            No more nodes
   (closest node)           edges found               to process
```

```text
Bellman-Ford Algorithm Flow:
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Initialize    │ -> │   Relax ALL     │ -> │   Check for     │
│  dist[source]=0 │    │   edges V-1     │    │   negative      │
│  others=∞       │    │   times         │    │   cycles        │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
   Dynamic Programming      Systematic edge          Detect infinite
   approach with multiple    relaxation in all        negative loops
   iterations               directions
```

### Algorithm Visualization

**Dijkstra's Algorithm Visualization:**
Imagine a wavefront expanding from the source node, always choosing the closest unvisited node to explore next. The algorithm maintains a "frontier" of nodes being considered, similar to how light spreads through a medium.

```text
Source Node (0)
     │
     │ Weight 4
     ▼
Node 1 ─── Weight 2 ─── Node 2
     │                    │
     │ Weight 5           │ Weight 8
     ▼                    ▼
Node 3 ◄── Weight 2 ─── Node 4
     ▲                    │
     │                    │
     └──── Weight 10 ─────┘
```

Figure 1: Example graph for shortest path algorithms. Source: GeeksforGeeks

**Bellman-Ford Algorithm Visualization:**
Think of it as a systematic wave that relaxes all edges simultaneously, allowing information to propagate through the graph in all directions, even backwards through negative edges.

```text
Iteration 1: Source influences immediate neighbors
Iteration 2: Neighbors influence their neighbors
Iteration 3: Second-degree neighbors get updated
...
Iteration V-1: All possible paths considered
```

Figure 2: Bellman-Ford relaxation process over multiple iterations. Source: Programiz

### Detailed Algorithm Diagrams

**Dijkstra's Step-by-Step Execution:**

```text
Initial State:
Nodes: A(0) ──4──> B(∞) ──2──> C(∞)
             │         │
             1         3
             ▼         ▼
             D(∞)     E(∞)

Step 1: Process A (distance 0)
- Update B: min(∞, 0+4) = 4
- Update D: min(∞, 0+1) = 1
Priority Queue: D(1), B(4)

Step 2: Process D (closest unvisited)
- Update E: min(∞, 1+3) = 4
Priority Queue: B(4), E(4)

Step 3: Process B
- Update C: min(∞, 4+2) = 6
Priority Queue: E(4), C(6)

Step 4: Process E
- No better paths found
Priority Queue: C(6)

Step 5: Process C
- Algorithm complete

Final distances: A=0, B=4, C=6, D=1, E=4
```

Figure 3: Dijkstra's algorithm execution trace. Source: Wikipedia

**Bellman-Ford Relaxation Process:**

```text
Graph: A ──4──> B ──(-2)──> C ──1──> D
       │       │           │
       3       2           5
       ▼       ▼           ▼
       E       F           G

Iteration 1 (Relax all edges):
A->B: dist[B] = min(∞, 0+4) = 4
A->E: dist[E] = min(∞, 0+3) = 3
B->F: dist[F] = min(∞, 4+2) = 6
B->C: dist[C] = min(∞, 4+(-2)) = 2
C->D: dist[D] = min(∞, 2+1) = 3
C->G: dist[G] = min(∞, 2+5) = 7

Iteration 2:
B->F: dist[F] = min(6, 4+2) = 6 (no change)
B->C: dist[C] = min(2, 4+(-2)) = 2 (no change)
C->D: dist[D] = min(3, 2+1) = 3 (no change)
C->G: dist[G] = min(7, 2+5) = 7 (no change)
E->F: dist[F] = min(6, 3+2) = 5 (better!)

Iteration 3:
F->G: dist[G] = min(7, 5+5) = 7 (no change)
...continues until V-1 iterations

Final: A=0, B=4, C=2, D=3, E=3, F=5, G=7
```

Figure 4: Bellman-Ford systematic edge relaxation. Source: GeeksforGeeks

### Key Differences Illustrated

**Why Dijkstra Fails with Negative Edges:**

```text
Graph with negative edge: A --(-2)--> B --(1)--> C
Shortest path: A -> B -> C = -2 + 1 = -1

Dijkstra might process A first, then C (distance 3), then B (distance 1)
But once B is processed with distance 1, it never gets updated to -1
Result: Dijkstra gives wrong answer of 3 instead of -1
```

**Bellman-Ford Handles Negative Edges:**

```text
Same graph: A --(-2)--> B --(1)--> C

Iteration 1: A->B: dist[B] = -2, A->C: dist[C] = 3
Iteration 2: B->C: dist[C] = -2 + 1 = -1 (better than 3)
Result: Correct shortest path of -1
```

### When to Use Each Algorithm

- **Use Dijkstra when:**

  - All edge weights are non-negative
  - Performance is critical
  - Graph is dense (many edges)

- **Use Bellman-Ford when:**

  - Edge weights can be negative
  - Need to detect negative cycles
  - Graph size is small to medium

## Applications

### Real-World Examples

1. **Network Routing**: OSPF protocol uses Dijkstra-like algorithms
2. **GPS Navigation**: Finding fastest routes considering traffic
3. **Currency Exchange**: Arbitrage detection (negative cycles indicate profit opportunities)
4. **Supply Chain Optimization**: Minimizing transportation costs
5. **Game AI**: Pathfinding in strategy games

### Currency Arbitrage Detection

One fascinating application of Bellman-Ford is detecting arbitrage opportunities in currency exchange markets. By modeling exchange rates as edge weights, negative cycles represent profitable trading loops.

```cpp
// Detect arbitrage opportunities in currency exchange
bool hasArbitrage(const vector<vector<double>>& rates) {
    int n = rates.size();
    vector<double> dist(n, 0.0); // Start with 1 unit of each currency
    dist[0] = 1.0;

    // Convert rates to negative log for shortest path
    Graph g(n);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (rates[i][j] > 0) {
                double weight = -log(rates[i][j]);
                g[i].push_back({j, weight});
            }
        }
    }

    // Run Bellman-Ford
    auto result = bellmanFord(g, 0);
    return result.empty(); // Empty means negative cycle (arbitrage)
}
```

### Traffic Routing with Negative Weights

Bellman-Ford can handle traffic scenarios where certain routes have penalties (modeled as negative weights) to discourage usage:

```cpp
// Example: Road network with traffic penalties
// Some roads have negative weights (penalties) to discourage heavy traffic
Graph createTrafficGraph() {
    Graph g(6);
    // Normal roads (positive weights = travel time)
    g[0] = {{1, 5}, {2, 10}};     // Highway vs local road
    g[1] = {{2, 3}, {3, 8}};      // Fast connection
    g[2] = {{3, 2}, {4, 6}};      // Alternative routes
    g[3] = {{4, 4}, {5, 7}};      // Final segments
    g[4] = {{5, 2}};              // Direct access
    g[5] = {};                    // Destination

    // Add traffic penalty (negative weight) to discourage main highway
    // This forces Bellman-Ford to find alternative routes
    g[0].push_back({3, -15});     // Heavily congested highway

    return g;
}
```

Figure 5: Traffic routing with penalty edges. Bellman-Ford finds optimal routes avoiding congested paths. Source: Adapted from real-world routing systems.

### Network Protocol Implementation

**OSPF (Open Shortest Path First)** uses Dijkstra's algorithm for routing table calculations:

```cpp
// Simplified OSPF-like routing with Dijkstra
class Router {
private:
    int id;
    Graph network;  // Full network topology
    vector<int> routingTable;

public:
    Router(int routerId, const Graph& fullNetwork)
        : id(routerId), network(fullNetwork) {}

    void updateRoutingTable() {
        // Use Dijkstra to compute shortest paths from this router
        routingTable = dijkstra(network, id);
    }

    int getNextHop(int destination) {
        // Find which neighbor to forward packets to
        for (auto [neighbor, weight] : network[id]) {
            if (routingTable[neighbor] + weight == routingTable[destination]) {
                return neighbor;
            }
        }
        return -1; // No route
    }
};
```

Figure 6: OSPF network with Dijkstra-based routing. Each router maintains shortest path tree. Source: Network protocol documentation.

## Common Pitfalls and Optimizations

### Common Mistakes

1. **Using Dijkstra with negative weights**: Leads to incorrect results
2. **Not handling disconnected graphs**: Some vertices may remain unreachable
3. **Integer overflow**: Use long long for large graphs
4. **Not checking for negative cycles**: Can cause infinite loops in practice

### Optimizations

1. **Early termination**: Stop Bellman-Ford if no updates in an iteration
2. **Bidirectional Dijkstra**: Search from both source and target
3. **A* Algorithm**: Use heuristics for better performance
4. **Fibonacci heaps**: Theoretical improvement for Dijkstra

### Advanced Concepts

**Negative Weight Cycle Detection:**
Bellman-Ford's ability to detect negative cycles is crucial for many applications. A negative cycle means you can reduce path cost indefinitely by traversing the cycle.

```cpp
// Enhanced Bellman-Ford with cycle detection and reporting
pair<vector<int>, vector<int>> bellmanFordWithCycle(const Graph& g, int source) {
    int n = g.size();
    vector<int> dist(n, INF);
    vector<int> predecessor(n, -1);
    dist[source] = 0;

    // Relax edges V-1 times
    for (int iter = 0; iter < n - 1; ++iter) {
        for (int u = 0; u < n; ++u) {
            if (dist[u] == INF) continue;
            for (auto [v, w] : g[u]) {
                if (dist[v] > dist[u] + w) {
                    dist[v] = dist[u] + w;
                    predecessor[v] = u;
                }
            }
        }
    }

    // Check for negative cycles and find cycle if exists
    vector<int> cycle;
    for (int u = 0; u < n; ++u) {
        if (dist[u] == INF) continue;
        for (auto [v, w] : g[u]) {
            if (dist[v] > dist[u] + w) {
                // Negative cycle detected, reconstruct it
                cycle = reconstructCycle(predecessor, v);
                return {dist, cycle};
            }
        }
    }

    return {dist, cycle}; // Empty cycle means no negative cycle
}

vector<int> reconstructCycle(const vector<int>& pred, int start) {
    vector<int> cycle;
    vector<bool> visited(pred.size(), false);
    int current = start;

    // Follow predecessors until we detect a cycle
    while (!visited[current] && current != -1) {
        visited[current] = true;
        current = pred[current];
    }

    // Extract the cycle
    if (current != -1) {
        int cycleStart = current;
        do {
            cycle.push_back(current);
            current = pred[current];
        } while (current != cycleStart);
        reverse(cycle.begin(), cycle.end());
    }

    return cycle;
}
```

Figure 7: Negative cycle detection and reconstruction. Bellman-Ford identifies problematic cycles in weighted graphs. Source: Algorithm analysis literature.

**Algorithm Comparison Summary:**

<!-- markdownlint-disable MD033 MD010 -->
<table>
    <thead>
        <tr><th>Feature</th><th>Dijkstra</th><th>Bellman-Ford</th></tr>
    </thead>
    <tbody>
        <tr><td>Edge Weights</td><td>≥ 0 only</td><td>Any weights</td></tr>
        <tr><td>Negative Cycles</td><td>Cannot detect</td><td>Detects and reports</td></tr>
        <tr><td>Time Complexity</td><td>O((V+E) log V)</td><td>O(V × E)</td></tr>
        <tr><td>Space Complexity</td><td>O(V)</td><td>O(V)</td></tr>
        <tr><td>Best For</td><td>Dense graphs, non-negative weights</td><td>Sparse graphs, negative weights possible</td></tr>
        <tr><td>Applications</td><td>GPS, network routing, games</td><td>Currency arbitrage, constraint systems</td></tr>
    </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->
Figure 8: Comprehensive algorithm comparison table. Choose based on graph properties and requirements. Source: Combined from multiple algorithm references.

## Self-Check Questions

1. Why can't Dijkstra's algorithm handle negative edge weights?
2. How does Bellman-Ford detect negative cycles?
3. What is the time complexity difference between Dijkstra and Bellman-Ford?
4. When would you choose one algorithm over the other?
5. How can shortest path algorithms be applied to currency arbitrage?

## Practice Time

Implement both Dijkstra's and Bellman-Ford algorithms in C++. Test them on graphs with:

- Non-negative weights
- Negative weights
- Negative cycles

Compare their performance and correctness on different graph types.

## Summary

Shortest path algorithms are essential tools in computer science and optimization. Dijkstra's algorithm provides efficient solutions for graphs with non-negative weights, while Bellman-Ford handles the more general case of negative weights and can detect problematic negative cycles.

Understanding when to use each algorithm and their trade-offs is crucial for solving real-world problems efficiently. These algorithms form the foundation for many advanced graph algorithms and optimization techniques.

## Next Steps

In the next lesson, we'll explore minimum spanning trees and algorithms like Kruskal's and Prim's, which solve different but related optimization problems on graphs.

## References

- Cormen, T. H., et al. "Introduction to Algorithms"
- Wikipedia: Shortest Path Problem
- Programiz: Dijkstra's and Bellman-Ford tutorials
- GeeksforGeeks: Graph algorithms
