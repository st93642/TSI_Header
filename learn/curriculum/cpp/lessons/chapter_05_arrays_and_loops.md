# Chapter 5: Arrays and Loops

## Learning goals

- Represent sequences using raw arrays alongside `std::array` and `std::vector`.
- Practise each loop form (`for`, range-based `for`, `while`, `do/while`) with meaningful stopping conditions.
- Analyse memory layout and indexing to avoid off-by-one mistakes.

## Preparation checklist

- Create `~/tsi_cpp/ch5_arrays_loops` to keep experiments isolated.
- Collect sample datasets such as exam scores or sensor readings stored in a text file.
- Sketch a table that maps indices to values so you can visualise memory layout while offline.

## 5.1 Raw arrays and initialisation

Write `scores.cpp` that declares arrays using braced initialisers. Print sizes with `std::size(array)` (available since C++17) and deliberately iterate too far to witness undefined behaviour. Document the outcome and list strategies to avoid magic numbers in future code.

## 5.2 Range-based loops

Create `range_loop.cpp` that iterates over a container using references. Demonstrate both const references for read-only access and mutable references for in-place updates. Compare the implementation with a classic index-based `for` loop and record the differences in your notebook.

## 5.3 Multidimensional arrays

Build `matrix.cpp` to store a seating chart in a two-dimensional array. Use nested loops to populate and display the matrix, and annotate comments explaining row-major ordering. For extra practice, contrast with `std::vector<std::vector<int>>` to see how dynamic dimensions change the code.

## 5.4 Dynamic sequences with `std::vector`

Construct `vector_stats.cpp` that reads values from standard input, pushes them into a `std::vector`, and computes minimum, maximum, and average statistics. Use range-based loops to keep the code concise. Save the results to a file so you can review them later without rerunning the program.

## 5.5 Lab: attendance tracker

1. Create `attendance_tracker.cpp`.
2. Store weekday attendance counts in a `std::array<int, 5>`.
3. Compute running totals with an index-based `for` loop, then repeat using a range-based loop to verify both approaches.
4. Append weekend make-up sessions using `std::vector<int>` to demonstrate how dynamic containers complement fixed arrays.
5. Build and run with your standard compile command.
6. Present results in aligned columns using `std::setw` to practise formatted output.

## 5.6 Self-check prompts

- When do you prefer `std::array` over a raw array?
- How does a `do/while` loop differ from a `while` loop in the way the guard is evaluated?
- Why is `std::size(array)` safer than hard-coded bounds?

## 5.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Crash or garbage output | Indexing past array bounds | Double-check loop conditions and use `std::size`. |
| Range loop fails to modify elements | Iterating by value instead of reference | Change `for (auto value : arr)` to `for (auto& value : arr)`. |
| Vector retains data between runs | Container never cleared | Call `vec.clear()` or reinitialise the container. |

## Wrap-up

- [ ] You iterated over arrays with both index-based and range-based loops.
- [ ] You built a multidimensional example and described row-major layout.
- [ ] You combined `std::array` and `std::vector` in a single program.

Chapter 6 transitions from contiguous arrays to pointers, references, and manual memory management. Keep your seating chart handy—you will trace the same memory using pointer arithmetic next.
