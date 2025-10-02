# Chapter 19: Containers and Algorithms

## Learning goals

- Select the appropriate STL container for a workload (sequence, associative, unordered).
- Traverse containers with iterators and understand iterator categories.
- Apply standard algorithms safely, including removing elements and sorting.

## Preparation checklist

- Set up `~/tsi_cpp/ch19_containers_algorithms`.
- Create sample datasets (CSV files or generated data) to feed into containers offline.
- Review earlier notes on iterators; this chapter layers on algorithm usage.

## 19.1 Sequence containers

Implement `sequence_demo.cpp` showcasing `std::vector`, `std::deque`, and `std::list`. Measure insertion/removal performance qualitatively by logging operations. Document differences in your journal.

## 19.2 Associative and unordered containers

Build `associative_demo.cpp` that stores mappings with `std::map`, `std::unordered_map`, and `std::set`. Demonstrate lookups, insertions, and iteration order.

## 19.3 Iterators and algorithms

Write `algorithm_tour.cpp` using `std::find`, `std::count_if`, `std::sort`, `std::transform`, and `std::accumulate`. Show how to compose algorithms with lambda expressions from Chapter 18.

## 19.4 Remove-erase idiom

Create `cleanup.cpp` demonstrating the remove-erase idiom on `std::vector`. Explain why algorithms do not erase elements themselves and how `erase` completes the operation.

## 19.5 Parallel algorithms (optional)

If your compiler supports C++17 parallel policies, experiment with `std::sort(std::execution::par, ...)`. Note CPU usage and include a caution about verifying thread safety offline.

## 19.6 Lab: academic analytics dashboard

1. Load course records into `std::vector<CourseRecord>`.
2. Use algorithms to compute aggregates: highest score, pass rate, sorted lists.
3. Store derived views in `std::map<std::string, std::vector<CourseRecord>>` by department.
4. Use iterators and algorithms to update dashboards after adding or removing records.
5. Print summary tables with `std::format` (or `std::ostringstream` if `format` is unavailable) to keep dependencies minimal offline.

## 19.7 Self-check prompts

- When is `std::list` preferable to `std::vector`?
- What does iterator invalidation mean, and which algorithms can trigger it?
- How does the remove-erase idiom work step by step?

## 19.8 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Iterator invalidation crash | Erasing elements while iterating | Use iterator-returning erase or the remove-erase idiom. |
| Unordered container iteration order unexpected | Containers are unordered by design | Do not rely on iteration order; use ordered containers if needed. |
| Parallel algorithm runtime errors | Compiler lacks parallel execution support | Guard parallel calls with feature detection or compile-time flags. |

## Wrap-up

- [ ] You used multiple container types and recorded their trade-offs.
- [ ] You chained standard algorithms with custom predicates.
- [ ] You applied the remove-erase idiom to clean up a container.

Next steps: revisit earlier chapters to reinforce skills, or branch into advanced topics such as concurrency and ranges while maintaining the same offline workflow.
