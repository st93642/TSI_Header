# Lesson 4.3: Dynamic Memory Allocation

Dynamic memory lets you request storage at runtime using `new`, `delete`, and modern smart pointers. *Beginning C++17* dedicates Chapter 10 (“Pointers and Dynamic Memory”) and Chapter 12 (“Managing Memory”) to these techniques—follow those sections closely, especially the examples on resource acquisition and release.

## What You'll Learn

- Allocating and releasing raw dynamic memory (`new`, `delete`, `new[]`, `delete[]`)
- Common ownership pitfalls: leaks, double-delete, dangling pointers
- Preferencing RAII and smart pointers (`std::unique_ptr`, `std::make_unique`) over manual `delete`
- Managing dynamic arrays with `std::vector` vs. raw pointers
- Exception safety considerations when mixing `new` and user-defined code

## 1. Raw Dynamic Allocation (Chapter 10)

```cpp
int* value {new int{42}};    // allocate single int
int* buffer {new int[5]{}}; // allocate array of 5 ints

// ... use value and buffer ...

delete value;      // free single allocation
delete[] buffer;   // free array allocation
```

Rules:

- Every `new` must eventually pair with a corresponding `delete`.
- Use `delete[]` for arrays; mixing `delete`/`delete[]` is undefined behavior.
- Set pointers to `nullptr` after deleting to avoid dangling references.

## 2. Smart Pointers (Chapter 12, “Using Smart Pointers”)

Modern C++ favors RAII wrappers that release memory automatically:

```cpp
auto buffer = std::make_unique<double[]>(count);
// buffer deletes itself when leaving scope
```

`std::unique_ptr` owns a resource exclusively, while `std::shared_ptr` allows shared ownership (with reference counting). Prefer `unique_ptr` for single ownership, and resist raw pointers unless interacting with legacy APIs.

## 3. Exception Safety

If an exception is thrown between `new` and `delete`, a memory leak occurs unless you use RAII. Smart pointers and containers ensure resources release automatically even during stack unwinding.

## 4. Choosing the Right Tool

- Use `std::vector` or other containers for resizable sequences.
- Use `std::array` for fixed-size compile-time arrays.
- Reserve raw `new`/`delete` for low-level work or teaching purposes; wrap the logic in helper classes if you must manage custom lifetimes.

## 5. Common Pitfalls

- Forgetting to free memory (leak)
- Freeing memory twice (double-delete)
- Accessing memory after it’s freed (dangling pointer)
- Throwing exceptions without RAII (leads to leaks)

## Practice Ideas

1. Allocate a dynamic array sized by user input, fill it with values, compute statistics, then free it.
2. Use `std::make_unique<double[]>(count)` to store sensor readings and convert them to percentages.
3. Wrap a raw resource in a simple RAII struct that deletes the resource in its destructor.

## Upcoming Exercise

You will implement `summarize_dynamic_scores()`, which allocates a dynamic array sized by user input, copies values into it, computes totals, and then releases the memory. Focus on:

- Guarding the allocation size before calling `new`
- Using a `std::unique_ptr<int[]>` wrapper to prevent leaks (as recommended in Chapter 12)
- Computing sum and average with the dynamically allocated buffer
- Formatting output with the exact blueprint required by the tests before returning the string

## References

- *Beginning C++17*, Chapter 10 “Pointers and Dynamic Memory”
- *Beginning C++17*, Chapter 12 “Managing Memory”
- cppreference.com: [`std::unique_ptr`](https://en.cppreference.com/w/cpp/memory/unique_ptr) and [dynamic memory](https://en.cppreference.com/w/cpp/language/new)
- ISO C++ Core Guidelines: R.3 (“A resource handle type must acquire, own, and release the resource”), R.1 (“Manage resources automatically using RAII”)
