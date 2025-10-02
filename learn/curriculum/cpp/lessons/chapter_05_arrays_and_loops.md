# Chapter 5: Arrays and Loops

## Learning goals

- Represent sequences using raw arrays alongside `std::array` and `std::vector`.
- Practise each loop form (`for`, range-based `for`, `while`, `do/while`) with meaningful stopping conditions.
- Analyse memory layout and indexing to avoid off-by-one mistakes.
- Tie array storage to pointer semantics so you spot dangerous conversions early.

## Concept map

arrays -> loops -> algorithms
std::array -> deterministic memory
std::vector -> dynamic growth
loop guards -> data safety

Try sketching the map in your notebook. Add arrows whenever you discover how a loop traverses a container or where a pointer sneaks in. This mirrors the diagrams in *Beginning C++17* (see ref.txt) that connect storage types, loop forms, and guard conditions.

## Preparation checklist

- Create `~/tsi_cpp/ch5_arrays_loops` to keep experiments isolated.
- Collect sample datasets such as exam scores or sensor readings stored in a text file.
- Sketch a table that maps indices to values so you can visualise memory layout while offline.
- Prepare a blank two-column log so you can record both *iteration strategy* and *observed output* during tests.

## 5.1 Raw arrays, bounds, and pointer equivalence

Example: rolling_average.cpp
int scores[]{72, 77, 88, 91, 85};
size_t count{std::size(scores)};
double sum{};
for (size_t i{}; i < count; ++i)
  sum += scores[i];
double average{sum / count};

Recreate the reference-book discussion where pointer arithmetic mirrors indexing. After compiling the example, repeat the loop with *(scores + i) to confirm that the same elements are read. Record both versions in your offline logbook. When curiosity strikes, add a deliberate off-by-one mistake and document the crash or garbage output—this echoes the cautionary tale around prime calculations in ref.txt.

Refined caution: the reference text shows how `average10(double array[10])` creates a false expectation. Even though the signature advertises ten elements, the array parameter collapses to a pointer. Capture this in your notes by writing "array parameter collapses to a pointer → false expectation about enforced length." Then, refactor the prototype to take a reference wrapper or pass the size explicitly.

## 5.2 Safer aggregates with `std::array`

Translate the raw-array example into an `std::array<int, 5>` so the compiler keeps track of the length. Highlight how `std::array -> deterministic memory` in the concept map protects against mismatched bounds. While working offline, run a quick diagnostic where you call `scores.at(index)` to witness the bounds-checking exception.

## 5.3 Dynamic lifetimes with `std::vector`

Example: temperature_log.cpp
std::vector&lt;double&gt; temps{};
double reading{};
while (std::cin >> reading && reading != 1000)
  temps.push_back(reading);

This mirrors the temperature recorder walkthrough in ref.txt, minus the shared pointer scaffolding. After capturing readings, compute minimum, maximum, and average with a range-based for loop and log the results. Note how `std::vector -> dynamic growth` makes it trivial to extend beyond the fixed sizes you rehearsed in the previous section.

## 5.4 Loop patterns side by side

Gather the canonical loops in a single worksheet:

- *Index-based for* to sweep deterministic spans.
- *Range-based for* with `auto&` to mutate elements efficiently.
- *While* to react to incoming data.
- *Do/while* for menu-driven confirmation.

Example: pointer_report.cpp
do {
  std::cout << "Enter pointer offset (negative to quit): ";
  long offset{};
  std::cin >> offset;
  if (offset < 0) break;
  for (size_t i{}; i < std::size(scores); ++i)
    std::cout << *(scores + i) << ' ';
  std::cout << '\n';
} while (true);

This mash-up echoes the pointer arithmetic narrative from ref.txt, where subtracting two pointers yields a `std::ptrdiff_t`. The range-based for loop keeps the code efficient when you only need read-only traversal. Capture the pointer difference experiments in your log—the book stresses that only pointers referencing the same array can be meaningfully compared.

## 5.5 Multidimensional layouts

Rebuild the beans yield example from the reference material using a two-dimensional array. Sketch a miniature grid in your notes:

    row 0:  1.0  2.0  3.0  4.0
    row 1:  5.0  6.0  7.0  8.0
    row 2:  9.0 10.0 11.0 12.0

Write `matrix_yield.cpp` that feeds the grid into a `yield()` function and accumulates totals with nested loops. Replace any "magic number 4" with `std::size(row)` to stay faithful to the book's warning about hidden constants. Extend the experiment by swapping the inner loop for a range-based variant and compare readability.

## 5.6 Offline drills and do/while rehearsal

Plan a do/while rehearsal where you emulate kiosk-style retry prompts. Build `retry_input.cpp` that keeps asking for valid temperature offsets until the user types *done*. Record the flow in an offline logbook drill, annotating each branch that entered the loop body at least once. Conclude with a reflection on when the guard should live at the bottom rather than the top of the loop.

For extra credit, blend in a Fahrenheit-to-Celsius conversion like the ref.txt range-based example. Use an array of readings, apply a reference loop to mutate in place, and note that the range-based for loop keeps the code efficient while avoiding accidental copies.

## 5.7 Lab: attendance tracker

1. Create `attendance_tracker.cpp`.
2. Store weekday attendance counts in a `std::array<int, 5>`.
3. Compute running totals with an index-based `for` loop, then repeat using a range-based loop to verify both approaches.
4. Append weekend make-up sessions using `std::vector<int>` to demonstrate how dynamic containers complement fixed arrays.
5. Build and run with your standard compile command.
6. Present results in aligned columns using `std::setw` to practise formatted output.
7. Document pointer arithmetic experiments that show the starting address, the address after adding an offset, and the difference measured in `std::ptrdiff_t`.

## 5.8 Troubleshooting reference

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Crash or garbage output | Indexing past array bounds | Double-check loop conditions and use `std::size`. |
| Range loop fails to modify elements | Iterating by value instead of reference | Change `for (auto value : arr)` to `for (auto& value : arr)`. |
| Vector retains data between runs | Container never cleared | Call `vec.clear()` or reinitialise the container. |
| Function ignores declared array length | Relies on pointer decay | Pass an explicit size or use `std::span`/references. |

## 5.9 Self-check prompts

- When do you prefer `std::array` over a raw array?
- How does a `do/while` loop differ from a `while` loop in the way the guard is evaluated?
- Why is `std::size(array)` safer than hard-coded bounds?
- How do you protect against the false expectation created by signatures such as `average10(double array[10])`?

## Wrap-up

- [ ] You iterated over arrays with both index-based and range-based loops.
- [ ] You built a multidimensional example and described row-major layout.
- [ ] You combined `std::array` and `std::vector` in a single program.
- [ ] You recorded offline drills covering pointer arithmetic, range-based conversions, and do/while rehearsal.

Chapter 6 transitions from contiguous arrays to pointers, references, and manual memory management. Keep your seating chart handy—you will trace the same memory using pointer arithmetic next.
