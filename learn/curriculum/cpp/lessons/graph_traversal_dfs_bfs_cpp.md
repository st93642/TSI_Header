# Lesson 11.3: Graph Traversal with DFS and BFS

## Why Graph Traversal Matters

Graphs are fundamental data structures that model relationships between entities. From social networks to transportation systems, graphs represent connections in the real world. Traversing graphs—systematically visiting each node—is essential for solving problems like finding paths, detecting cycles, or exploring connected components.

In this lesson, we'll master two cornerstone algorithms: Depth-First Search (DFS) and Breadth-First Search (BFS). These techniques form the backbone of graph algorithms and are crucial for advanced problem-solving in competitive programming, system design, and data analysis.

## Graph Representations

Before diving into traversal, let's understand how to represent graphs in memory. The choice of representation affects algorithm efficiency and implementation complexity.

### Adjacency List

The adjacency list is the most common and efficient representation for sparse graphs:

```cpp
#include <vector>
#include <list>

class Graph {
private:
    int vertices;
    std::vector<std::list<int>> adjList;

public:
    Graph(int v) : vertices(v), adjList(v) {}

    void addEdge(int u, int v) {
        adjList[u].push_back(v);
        // For undirected graph, add: adjList[v].push_back(u);
    }
};
```

- **Space Complexity**: O(V + E), where V is vertices and E is edges
- **Pros**: Memory efficient for sparse graphs, easy to iterate neighbors
- **Cons**: Slower edge existence checks compared to matrix

### Adjacency Matrix

For dense graphs or when you need fast edge lookups:

```cpp
#include <vector>

class Graph {
private:
    int vertices;
    std::vector<std::vector<bool>> adjMatrix;

public:
    Graph(int v) : vertices(v), adjMatrix(v, std::vector<bool>(v, false)) {}

    void addEdge(int u, int v) {
        adjMatrix[u][v] = true;
        // For undirected: adjMatrix[v][u] = true;
    }

    bool hasEdge(int u, int v) {
        return adjMatrix[u][v];
    }
};
```

- **Space Complexity**: O(V²)
- **Pros**: O(1) edge existence checks, simple implementation
- **Cons**: Wastes space for sparse graphs

## Depth-First Search (DFS)

DFS explores as far as possible along each branch before backtracking. It's like traversing a maze by always going forward until you hit a dead end, then retracing your steps.

### Recursive DFS Implementation

```cpp
#include <vector>
#include <iostream>

class Graph {
private:
    int vertices;
    std::vector<std::list<int>> adjList;
    std::vector<bool> visited;

public:
    Graph(int v) : vertices(v), adjList(v), visited(v, false) {}

    void addEdge(int u, int v) {
        adjList[u].push_back(v);
    }

    void DFS(int start) {
        visited[start] = true;
        std::cout << start << " ";

        for (int neighbor : adjList[start]) {
            if (!visited[neighbor]) {
                DFS(neighbor);
            }
        }
    }

    void DFSTraversal(int start) {
        std::fill(visited.begin(), visited.end(), false);
        DFS(start);
        std::cout << std::endl;
    }
};
```

### How DFS Works

1. Mark the current node as visited
2. Print/explore the current node
3. Recursively visit all unvisited neighbors
4. Backtrack when no unvisited neighbors remain

### Iterative DFS with Stack

For environments with limited recursion depth or to avoid stack overflow:

```cpp
#include <stack>

void DFSIterative(int start) {
    std::vector<bool> visited(vertices, false);
    std::stack<int> stk;

    stk.push(start);
    visited[start] = true;

    while (!stk.empty()) {
        int current = stk.top();
        stk.pop();
        std::cout << current << " ";

        for (int neighbor : adjList[current]) {
            if (!visited[neighbor]) {
                visited[neighbor] = true;
                stk.push(neighbor);
            }
        }
    }
    std::cout << std::endl;
}
```

## Breadth-First Search (BFS)

BFS explores nodes level by level, visiting all neighbors before moving to the next depth. It's ideal for finding shortest paths in unweighted graphs.

### BFS Implementation

```cpp
#include <queue>

void BFS(int start) {
    std::vector<bool> visited(vertices, false);
    std::queue<int> q;

    visited[start] = true;
    q.push(start);

    while (!q.empty()) {
        int current = q.front();
        q.pop();
        std::cout << current << " ";

        for (int neighbor : adjList[current]) {
            if (!visited[neighbor]) {
                visited[neighbor] = true;
                q.push(neighbor);
            }
        }
    }
    std::cout << std::endl;
}
```

### BFS Level-by-Level

To process nodes level by level:

```cpp
void BFSLevels(int start) {
    std::vector<bool> visited(vertices, false);
    std::queue<int> q;
    std::vector<int> level(vertices, -1);

    visited[start] = true;
    q.push(start);
    level[start] = 0;

    while (!q.empty()) {
        int current = q.front();
        q.pop();
        std::cout << "Level " << level[current] << ": " << current << std::endl;

        for (int neighbor : adjList[current]) {
            if (!visited[neighbor]) {
                visited[neighbor] = true;
                q.push(neighbor);
                level[neighbor] = level[current] + 1;
            }
        }
    }
}
```

## Applications and Use Cases

### Finding Connected Components

```cpp
int countConnectedComponents() {
    std::vector<bool> visited(vertices, false);
    int components = 0;

    for (int i = 0; i < vertices; ++i) {
        if (!visited[i]) {
            DFS(i); // or BFS(i)
            components++;
        }
    }
    return components;
}
```

### Shortest Path in Unweighted Graph

BFS naturally finds shortest paths:

```cpp
std::vector<int> shortestPath(int start, int target) {
    std::vector<bool> visited(vertices, false);
    std::vector<int> parent(vertices, -1);
    std::queue<int> q;

    visited[start] = true;
    q.push(start);

    while (!q.empty()) {
        int current = q.front();
        q.pop();

        if (current == target) break;

        for (int neighbor : adjList[current]) {
            if (!visited[neighbor]) {
                visited[neighbor] = true;
                parent[neighbor] = current;
                q.push(neighbor);
            }
        }
    }

    // Reconstruct path
    std::vector<int> path;
    for (int at = target; at != -1; at = parent[at]) {
        path.push_back(at);
    }
    std::reverse(path.begin(), path.end());
    return path;
}
```

### Cycle Detection

DFS can detect cycles:

```cpp
bool hasCycleDFS(int node, int parent, std::vector<bool>& visited) {
    visited[node] = true;

    for (int neighbor : adjList[node]) {
        if (!visited[neighbor]) {
            if (hasCycleDFS(neighbor, node, visited)) {
                return true;
            }
        } else if (neighbor != parent) {
            return true; // Back edge found
        }
    }
    return false;
}

bool hasCycle() {
    std::vector<bool> visited(vertices, false);
    for (int i = 0; i < vertices; ++i) {
        if (!visited[i]) {
            if (hasCycleDFS(i, -1, visited)) {
                return true;
            }
        }
    }
    return false;
}
```

## Time and Space Complexity

- **DFS**: O(V + E) time, O(V) space (recursion stack)
- **BFS**: O(V + E) time, O(V) space (queue)
- Both algorithms are linear in the size of the graph

## Choosing Between DFS and BFS

- **Use DFS when**:
  - Memory is limited (uses less space)
  - You need to explore deep paths first
  - Detecting cycles in directed graphs
  - Topological sorting

- **Use BFS when**:
  - Finding shortest paths in unweighted graphs
  - Level-order processing
  - Memory allows wider exploration

## Common Pitfalls

1. **Forgetting to mark nodes visited**: Leads to infinite loops
2. **Not handling disconnected graphs**: May miss components
3. **Recursion depth limits**: Use iterative for large graphs
4. **Directed vs Undirected**: Edge addition differs

## Self-Check Questions

1. What is the time complexity of DFS and BFS?
2. When would you choose BFS over DFS?
3. How does DFS detect cycles in undirected graphs?
4. What data structures do DFS and BFS use?
5. How do you find connected components in a graph?

## Practice Time

Complete the following quiz to reinforce your understanding of graph traversal algorithms.

The quiz will test your knowledge of DFS, BFS, their implementations, and applications.
