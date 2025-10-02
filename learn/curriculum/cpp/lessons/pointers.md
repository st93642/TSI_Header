# Lesson 4.2: Pointers

Pointers hold memory addresses, giving you direct control over objects and arrays. This lesson distills the essential rules for declaring, dereferencing, and manipulating pointers so you can rely on a single, self-contained source.

## What You'll Learn

- Declaring pointers and pointer-to-const
- Dereferencing pointers safely to access pointed-to values
- Pointer arithmetic with arrays (`ptr + index`, `*(ptr + index)`)
- Null pointers, initialization, and avoiding dangling pointers
- Passing pointers to functions vs. references

## 1. Declaring and Initializing Pointers

```cpp
int value {42};
int* pointerToValue {&value};
const int* pointerToConst {&value};
```

Key rules:

- Always initialize pointers—use `nullptr` instead of raw zero.
- `const int*` prevents modification through the pointer; `int* const` keeps the pointer address fixed.

## 2. Dereferencing and Pointer Safety

- Use `*pointer` to access the object being pointed to.
- Dereferencing a null or dangling pointer leads to undefined behavior—guard with checks.
- Prefer references when you can guarantee a valid object; use pointers when the object may be absent or when you need to reseat to a different target.

## 3. Pointer Arithmetic

When you have contiguous storage (arrays), you can advance a pointer:

```cpp
const int values[] {10, 20, 30};
const int* current {values};

std::cout << *current;     // 10
++current;                 // move to next element
std::cout << *current;     // 20
```

Pointer arithmetic respects element size—`current + 2` advances by two integers.

## 4. Passing Pointers to Functions

- Use pointers to share buffers, allow null, or emulate optional values.
- Document ownership: who allocates memory and who deletes it?
- Prefer `span`, iterators, or references when ownership and size are clear.

## 5. Avoiding Pitfalls

- Never return the address of a local variable (it goes out of scope).
- Reset pointers after `delete` (or better, use smart pointers—covered later).
- Distinguish between `const int*` (data is const) and `int* const` (pointer is const).

## Practice Ideas

1. Count histogram buckets using pointer arithmetic over an `int` array.
2. Implement a function that copies values from one buffer to another using `const double*` and `double*`.
3. Traverse a C-style string to find its length without using `std::strlen`.

## Upcoming Exercise

You will create `describe_buffer()`, which accepts two pointers (`const int* begin`, `const int* end`) and returns a formatted string summarizing count, minimum, maximum, and total. Focus on:

- Guarding against `begin == end` by reporting zero elements
- Iterating strictly with pointer arithmetic (no indexing or std::vector)
- Tracking statistics while advancing the pointer one element at a time
- Returning a string that exactly matches the blueprint used by the automated tests

## References

- cppreference.com: [Pointers](https://en.cppreference.com/w/cpp/language/pointer)
- ISO C++ Core Guidelines: P.1 (“Express ideas directly in code”) and ES.47 (“Use std::array or vector instead of built-in arrays when you can”)
