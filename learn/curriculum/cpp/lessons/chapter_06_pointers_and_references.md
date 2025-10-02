# Chapter 6: Pointers and References

## Learning goals

- Trace how raw pointers, references, and smart pointers represent memory addresses and ownership.
- Practise pointer arithmetic safely, documenting every offset with `std::ptrdiff_t`.
- Compare pointer-to-const, const pointer, and reference parameters when designing reusable functions.
- Manage lifetimes with `std::shared_ptr`, `std::unique_ptr`, and `std::weak_ptr` so that dynamic data is released deterministically.

## Concept map

pointers -> addresses -> indirection
references -> aliases -> safety
smart pointers -> RAII -> ownership
pointer arithmetic -> offsets -> std::ptrdiff_t

Recreate the map in your notebook before you code. The reference text emphasises that raw pointers excel at indirection, references behave as stable aliases, and smart pointers wrap ownership inside RAII blocks. Add your own arrows as you encounter new relationships.

## Preparation checklist

- Set up `~/tsi_cpp/ch6_pointers_references` with subfolders `raw`, `smart`, and `analysis` for organised experiments.
- Keep coloured pens ready to mark pointer diagrams from the reference text—red for addresses, blue for values, green for ownership.
- Review the Chapter 5 array worksheets, because many examples reuse the same buffers but swap in pointer traversal.
- Prepare an offline journal page with two columns labelled *address observed* and *meaning in program*.

## 6.1 Drawing the pointer diary

Example: pointer_diary.cpp
int samples[]{10, 20, 30, 40, 50};
int\* cursor{samples};
for (size_t i{}; i < std::size(samples); ++i)
  std::cout << "samples[" << i << "] = " << \*(cursor + static_cast&lt;long&gt;(i)) << '\n';

Paraphrasing the opening pages of ref.txt, note that a pointer stores an address and that indirection retrieves the value. Log each loop iteration in your journal: record the numeric address, the dereferenced value, and a sketch showing the pointer hopping across the array. Compare the results with direct indexing (`samples[i]`) to reinforce the equivalence.

## 6.2 Subtracting pointers the safe way

Example: pointer_offsets.cpp
long numbers[]{10, 20, 30, 40, 50, 60};
long\* p_first{&numbers[1]};
long\* p_last{&numbers[5]};
std::ptrdiff_t span{p_last - p_first};

The reference chapter explains that pointer subtraction is only meaningful when both pointers refer to the same array. Write assertions that fail if you subtract pointers from different allocations. Capture the computed `std::ptrdiff_t` in your notebook and convert it to a human explanation: “difference of 4 elements.” Repeat with `p_first - p_last` to observe the negative span and confirm that the sign indicates direction.

## 6.3 Pointer constness and references

Retype the cautionary example from ref.txt that distinguishes pointer-to-const and const pointer:

const double\* reading_ptr{nullptr};
double\* const fixed_anchor{nullptr};

Add a third sample showing a reference alias:

double reading{3.5};
double& reading_ref{reading};

Discuss in your offline notes how `const double*` prevents modifying the value, whereas `double* const` prevents reseating the pointer. Echo the book’s reminder that a reference acts as an alias: assigning through `reading_ref` changes `reading`, but you can never redirect the reference to a different variable. Extend the experiment by calling a function that takes a parameter by reference and demonstrate that the caller’s variable updates.

## 6.4 Shared ownership temperature log

Example: lifetime_tracker.cpp
std::vector&lt;double&gt; morning{45.5, 50.0, 48.2};
auto records = std::make_shared&lt;std::vector&lt;double&gt;&gt;(morning);
records->push_back(57.0);

Rebuild the chapter’s weather recorder scenario, but replace the book’s `shared_ptr<vector<double>>` pseudocode with a fully working offline version that logs the addresses returned by `get()`. Pair it with a short loop:

for (const auto& value : *records)
  std::cout << value << ' ';

Write down where ownership changes hands and when the control block reaches zero. Simulate a second observer using `std::weak_ptr` to inspect the temperature log: demonstrate `lock()` returning an empty pointer once all shared owners reset.

## 6.5 Reference-powered adapters

Example: reference_aliases.cpp
double data[]{1.0, 2.0, 3.0};
double& alias{data[0]};
alias += 2.5;

Use this short clip to reinforce the book’s analogy that a reference behaves like a dereferenced const pointer. Track the address of `alias` and show that `&alias == &data[0]`. Then extend the example with functions that accept `const double&` to emphasise how range-based loops can avoid needless copies.

## 6.6 Smart pointer roster

Example: ownership_roster.cpp
auto lone_image = std::make_unique&lt;Image&gt;("diagram.ppm");
auto shared_scene = std::make_shared&lt;Scene&gt;();
auto hint = std::weak_ptr&lt;Scene&gt;(shared_scene);

Following the reference text, log constructor and destructor messages to visualise RAII. Reset `shared_scene` and record how the weak pointer notices expiration. Combine the roster with the temperature log from Section 6.4 to ensure your understanding of shared ownership stays grounded in the previous example.

## 6.7 Lab: lab reservation manager

1. Create `lab_reservation.cpp`.
2. Define a `Reservation` struct that holds the room name, capacity, and contact.
3. Manage an expandable array of reservations with `std::unique_ptr&lt;Reservation[]&gt;` and resize when needed.
4. Implement `add_reservation`, `find_reservation`, and `cancel_reservation` using raw pointers retrieved via `get()` but never storing them globally.
5. Instrument each function with log statements that record the addresses you traverse and the `std::ptrdiff_t` offsets applied.
6. Compile and run, then capture the lifecycle events in your offline journal to verify there are no leaks.

## 6.8 Troubleshooting reference

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Crash on dereference | Pointer not initialised | Set pointers to `nullptr` and check before use. |
| Memory leak | Missing `delete` or `reset()` | Wrap resources in RAII types such as `std::unique_ptr`. |
| Dangling reference | Returning reference to local variable | Return by value or extend the lifetime appropriately. |
| Invalid pointer subtraction | Operands reference different arrays | Restrict arithmetic to the same underlying array. |

## 6.9 Self-check prompts

- In your own words, how does `std::ptrdiff_t` express pointer distance?
- When would you prefer a reference parameter over a pointer parameter?
- What signals that it is time to use `std::weak_ptr` in a shared ownership graph?
- How do you decide between reseating a pointer and modifying the underlying value?

## Wrap-up

- [ ] You wrote pointer diary entries that document addresses, values, and pointer differences.
- [ ] You contrasted pointer-to-const, const pointer, and reference parameters in runnable examples.
- [ ] You modelled shared, unique, and weak ownership paths for a temperature log.
- [ ] You validated pointer arithmetic using `std::ptrdiff_t` and same-array guarantees.

Chapter 7 transitions to text management with character arrays, `std::string`, and string views. Carry forward your journal habits so pointer reasoning continues to feel concrete.
