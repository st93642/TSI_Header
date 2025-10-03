# Pointers and Memory Management in C

Pointers are the gateway to C's power, enabling direct memory manipulation, dynamic allocation, and efficient data structures. This lesson demystifies pointers, addresses, and memory lifecycle, from basic dereferencing to advanced patterns like function pointers and manual memory management. You'll learn to avoid common pitfalls like dangling pointers and leaks while building robust, performant code.

## Learning goals

- Understand pointer syntax, declaration, and initialization.
- Differentiate between stack and heap memory allocation.
- Use pointers for pass-by-reference, arrays, and strings.
- Implement dynamic memory with `malloc`, `free`, and error handling.
- Debug pointer-related issues using addresses and memory inspection.
- Apply pointers in real-world scenarios like linked lists and callbacks.

## 1. What are pointers?

Pointers store memory addresses, allowing indirect access to data. They enable efficient operations but require careful management to prevent errors.

```c
#include <stdio.h>

int main(void)
{
    int x = 42;
    int *ptr = &x;  // ptr points to x

    printf("x = %d\n", x);
    printf("Address of x: %p\n", (void*)&x);
    printf("*ptr = %d\n", *ptr);  // dereference

    *ptr = 100;  // modify through pointer
    printf("x = %d\n", x);

    return 0;
}
```

### Checkpoint: Basic pointer operations

1. Declare an int variable and a pointer to it.
2. Print the variable's value, address, and dereferenced pointer.
3. Modify the variable via the pointer and print again.

## 2. Pointer types and void pointers

Pointers have types matching the data they point to. Void pointers can point to any type but require casting.

```c
#include <stdio.h>

int main(void)
{
    int i = 10;
    double d = 3.14;
    char c = 'A';

    int *ip = &i;
    double *dp = &d;
    char *cp = &c;

    void *vp = &i;  // generic pointer
    printf("i via void: %d\n", *(int*)vp);

    return 0;
}
```

### Checkpoint: Typed pointers

1. Create pointers to int, float, char.
2. Use void pointer to access each, with proper casting.

## 3. Pointers and arrays

Arrays decay to pointers in expressions. Pointer arithmetic allows traversal.

```c
#include <stdio.h>

int main(void)
{
    int arr[] = {1, 2, 3, 4, 5};
    int *p = arr;  // p points to arr[0]

    for (int i = 0; i < 5; ++i)
    {
        printf("arr[%d] = %d, *(p+%d) = %d\n", i, arr[i], i, *(p + i));
    }

    return 0;
}
```

### Checkpoint: Array traversal

1. Use a pointer to iterate through an array of ints.
2. Print each element using pointer arithmetic.

## 4. Pointers to pointers

Multi-level pointers enable complex data structures.

```c
#include <stdio.h>

int main(void)
{
    int x = 5;
    int *p = &x;
    int **pp = &p;

    printf("x = %d\n", x);
    printf("*p = %d\n", *p);
    printf("**pp = %d\n", **pp);

    return 0;
}
```

### Checkpoint: Double pointers

1. Create a pointer to a pointer to an int.
2. Modify the int through the double pointer.

## 5. Dynamic memory allocation

Use `malloc` and `free` for heap memory.

```c
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    int *arr = malloc(5 * sizeof(int));
    if (arr == NULL)
    {
        return 1;
    }

    for (int i = 0; i < 5; ++i)
    {
        arr[i] = i * 10;
        printf("%d ", arr[i]);
    }
    printf("\n");

    free(arr);
    return 0;
}
```

### Checkpoint: Dynamic array

1. Allocate an array of 10 ints dynamically.
2. Fill with values and print.
3. Free the memory.

## 6. Memory leaks and dangling pointers

Always free allocated memory. Avoid using freed pointers.

```c
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    int *p = malloc(sizeof(int));
    *p = 42;
    printf("%d\n", *p);
    free(p);
    // p is now dangling; don't use it

    return 0;
}
```

### Checkpoint: Safe allocation

1. Allocate memory, use it, then free.
2. Set pointer to NULL after free.

## 7. Pointers in functions

Pass pointers for modification.

```c
#include <stdio.h>

void swap(int *a, int *b)
{
    int temp = *a;
    *a = *b;
    *b = temp;
}

int main(void)
{
    int x = 1, y = 2;
    swap(&x, &y);
    printf("x=%d, y=%d\n", x, y);
    return 0;
}
```

### Checkpoint: Pass by reference

1. Write a function to swap two ints using pointers.
2. Test it in main.

## 8. Strings and pointers

Strings are char arrays/pointers.

```c
#include <stdio.h>
#include <string.h>

int main(void)
{
    char *str = "Hello";
    printf("%s\n", str);

    char arr[] = "World";
    printf("%s\n", arr);

    return 0;
}
```

### Checkpoint: String manipulation

1. Use pointers to iterate through a string.
2. Print each character.

## 9. Function pointers

Pointers to functions enable callbacks.

```c
#include <stdio.h>

int add(int a, int b) { return a + b; }
int mul(int a, int b) { return a * b; }

int main(void)
{
    int (*op)(int, int) = add;
    printf("%d\n", op(3, 4));

    op = mul;
    printf("%d\n", op(3, 4));

    return 0;
}
```

### Checkpoint: Function pointer

1. Define function pointers for arithmetic ops.
2. Call via pointer.

## 10. Common pitfalls

- Null pointer dereference
- Uninitialized pointers
- Buffer overflows
- Memory leaks

### Checkpoint: Debug pointers

1. Identify and fix pointer errors in sample code.

## 11. Mini project: Simple linked list

Implement a linked list using pointers.

1. Define a struct for nodes.
2. Functions to add, remove, print.
3. Manage memory properly.

### Success criteria

- Compiles without warnings.
- Handles empty list.
- Frees all memory.

## 12. Guided practice challenges

1. Pointer arithmetic on arrays.
2. Dynamic 2D arrays.
3. String reversal with pointers.
4. Callback for sorting.

## 13. Self-check questions

1. What is the difference between & and *?
2. When to use malloc vs. arrays?
3. How to avoid memory leaks?
4. What are function pointers used for?

## Recap and next steps

Pointers unlock C's potential. Practice safe memory management, then explore advanced topics like structs and file I/O.
