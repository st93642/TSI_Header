# Chapter 6: Pointers and References

## Learning goals

- Distinguish between pointer types (raw, smart, pointer-to-const, const pointer).
- Navigate arrays via pointers and interpret pointer arithmetic.
- Allocate, manage, and release dynamic memory while avoiding leaks.

## Preparation checklist

- Create `~/tsi_cpp/ch6_pointers_references`.
- Gather scratch paper to sketch pointer diagrams for each experiment.
- Review the array examples from Chapter 5; you will reuse them as pointer targets.

## 6.1 Pointer basics

Write `pointer_basics.cpp` that declares pointers to built-in types, demonstrates the address-of (`&`) and dereference (`*`) operators, and prints addresses with `std::cout`. Annotate each printout so you can trace which variable owns the pointed-to value.

## 6.2 Pointer arithmetic and arrays

Create `pointer_arithmetic.cpp` to iterate over an integer array using pointer arithmetic (`ptr + i`). Compare the output with `*(arr + i)` to confirm both approaches read the same element. Print raw addresses via `std::uintptr_t` to visualise how the pointer advances.

## 6.3 Smart pointers

Build `smart_pointers.cpp` demonstrating `std::unique_ptr`, `std::shared_ptr`, and `std::weak_ptr`. Show how RAII ownership works by logging constructor and destructor calls. Include examples of `reset()` and scope blocks that release resources automatically.

## 6.4 References vs pointers

Implement `references.cpp` that contrasts references with pointers. Provide functions that accept parameters by pointer, by reference, and by const reference. Note how call sites differ and which forms allow reassignment or modification.

## 6.5 Lab: lab reservation manager

1. Create `lab_reservation.cpp`.
2. Represent booking entries with a struct containing room name, capacity, and `std::string` contact.
3. Store reservations in a dynamically allocated array managed by `std::unique_ptr<Reservation[]>`.
4. Implement helper functions `add_reservation`, `find_reservation`, and `cancel_reservation` that manipulate the array through raw pointers returned by `get()`.
5. Compile and run, verifying there are no leaks by reviewing your logs (and, if available offline, running a memory checker such as `valgrind`).
6. Summarise how smart pointers simplified cleanup in your notes.

## 6.6 Self-check prompts

- Why must `delete[]` match allocations created with `new[]`?
- When would you use `std::weak_ptr` to reference an object?
- How do references differ from pointers with respect to reassignment?

## 6.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Crash on dereference | Pointer not initialised | Set pointers to `nullptr` and check before use. |
| Memory leak | Missing `delete` or `reset()` | Wrap resources in RAII types such as `std::unique_ptr`. |
| Dangling reference | Returning reference to local variable | Return by value or extend the lifetime appropriately. |

## Wrap-up

- [ ] You traced pointer arithmetic with drawings and verified outputs manually.
- [ ] You implemented a smart-pointer-based RAII helper.
- [ ] You articulated differences between pointers and references.

Chapter 7 turns to text processing with `std::string` and string views. Keep your pointer diagrams—they help when reasoning about contiguous character storage.
