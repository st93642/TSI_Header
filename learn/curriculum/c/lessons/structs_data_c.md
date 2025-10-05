# Structs and Data Structures in C

Structs enable grouping related data into cohesive units, forming the foundation of complex data structures. This lesson explores struct definition, initialization, and manipulation, progressing to unions, enums, and basic data structures like linked lists. You'll learn to design type-safe abstractions, manage memory for structs, and implement algorithms that operate on structured data.

## Learning goals

- Define and use structs to encapsulate related data fields.
- Initialize structs with designated initializers and handle nested structs.
- Pass structs to functions by value and reference.
- Implement unions for memory-efficient variant types.
- Use enums for readable constant definitions.
- Build and manipulate linked lists using structs and pointers.
- Apply structs in real-world scenarios like records and trees.

## 1. Introducing structs

Structs bundle variables of different types into a single unit.

```c
#include <stdio.h>

struct Person {
    char name[50];
    int age;
    float height;
};

int main(void)
{
    struct Person p = {"Alice", 30, 5.5};
    printf("Name: %s, Age: %d, Height: %.1f\n", p.name, p.age, p.height);
    return 0;
}
```

### Checkpoint: Define and initialize a struct

1. Define a struct for a Book with title, author, pages.
2. Initialize and print its fields.

## 2. Struct operations

Access fields with dot operator. Use pointers for efficiency.

```c
#include <stdio.h>

struct Point {
    int x, y;
};

void move_point(struct Point *p, int dx, int dy)
{
    p->x += dx;
    p->y += dy;
}

int main(void)
{
    struct Point pt = {10, 20};
    move_point(&pt, 5, 5);
    printf("Point: (%d, %d)\n", pt.x, pt.y);
    return 0;
}
```

### Checkpoint: Struct pointer access

1. Create a struct Rectangle with width, height.
2. Write a function to calculate area using a pointer.

## 3. Designated initializers

Specify fields explicitly.

```c
struct Employee {
    char name[50];
    int id;
    float salary;
};

struct Employee e = {.name = "Bob", .id = 123, .salary = 50000.0};
```

### Checkpoint: Designated init

1. Initialize a struct with fields out of order.

## 4. Nested structs

Structs can contain other structs.

```c
struct Address {
    char street[100];
    char city[50];
};

struct Person {
    char name[50];
    struct Address addr;
};
```

### Checkpoint: Nested struct

1. Define Person with Address, initialize and access.

## 5. Unions for variant types

Unions share memory for different types.

```c
union Data {
    int i;
    float f;
    char str[20];
};

int main(void)
{
    union Data d;
    d.i = 42;
    printf("Int: %d\n", d.i);
    d.f = 3.14;
    printf("Float: %.2f\n", d.f);
    return 0;
}
```

### Checkpoint: Union usage

1. Create a union for int or char array, demonstrate.

## 6. Enums for constants

Enums define named constants.

```c
enum Color { RED, GREEN, BLUE };

int main(void)
{
    enum Color c = GREEN;
    printf("Color: %d\n", c);
    return 0;
}
```

### Checkpoint: Enum definition

1. Define enum for days of week, use in switch.

## 7. Typedefs for aliases

Simplify type names.

```c
typedef struct {
    int x, y;
} Point;

Point p = {1, 2};
```

### Checkpoint: Typedef struct

1. Use typedef for a struct, create instance.

## 8. Linked lists

Dynamic data structure using structs and pointers.

```c
#include <stdio.h>
#include <stdlib.h>

typedef struct Node {
    int data;
    struct Node *next;
} Node;

Node* create_node(int data)
{
    Node *n = malloc(sizeof(Node));
    if (n) {
        n->data = data;
        n->next = NULL;
    }
    return n;
}

void print_list(Node *head)
{
    while (head) {
        printf("%d ", head->data);
        head = head->next;
    }
    printf("\n");
}

int main(void)
{
    Node *head = create_node(1);
    head->next = create_node(2);
    print_list(head);
    // Free memory
    return 0;
}
```

### Checkpoint: Simple linked list

1. Create a list with 3 nodes, print, free.

## 9. Memory management for structs

Always free dynamically allocated structs.

```c
void free_list(Node *head)
{
    while (head) {
        Node *temp = head;
        head = head->next;
        free(temp);
    }
}
```

### Checkpoint: Free list

1. Implement and call free_list.

## 10. Struct arrays

Arrays of structs for multiple records.

```c
struct Student students[10];
```

### Checkpoint: Array of structs

1. Define array of 5 structs, initialize, print.

## 11. Bit fields

Pack data into bits.

```c
struct Flags {
    unsigned int flag1 : 1;
    unsigned int flag2 : 1;
};
```

### Checkpoint: Bit field

1. Define struct with bit fields, set and check.

## 12. Mini project: Address book

Implement a simple address book using linked list of structs.

1. Define struct for Contact with name, phone.
2. Functions to add, print, free.
3. Main to demonstrate.

### Success criteria

- Compiles cleanly.
- Handles memory properly.
- Prints contacts correctly.

## 13. Guided practice challenges

1. Struct for complex numbers, operations.
2. Union for different data types.
3. Enum for menu options.
4. Linked list insertion and deletion.
5. Struct with function pointers.

## 14. Self-check questions

1. Difference between struct and union?
2. When to use typedef?
3. How to free a linked list?
4. Advantages of designated initializers?

## Practical Appendix: Structs & Data Layout (Appendix — structs_data_c-appendix)

Notes on struct packing, alignment, and portability concerns for C lessons that use data structures.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Concern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Alignment</td><td>Padding between members</td><td>Use `offsetof` and `sizeof` to inspect</td></tr>
    <tr><td>Packing</td><td>Reduce size</td><td>Use `#pragma pack` carefully</td></tr>
    <tr><td>Endianness</td><td>Binary formats</td><td>Document and account for endian conversions</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (structs_data_c-appendix)

1. Write a small program that prints `sizeof` for a struct and the offset of each member using `offsetof`.
2. Demonstrate how `#pragma pack` affects layout and discuss portability trade-offs.

## Recap and next steps

Structs empower you to model real-world entities. Next, explore file I/O to persist data.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Structs & Data — Alignment, Packing & Init (Appendix — structs_data_c-appendix-20251005-01)

Practical notes about struct layout, padding surprises, and safe initialization patterns in C.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Advice</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Padding</td><td>Order fields by size</td><td>Reduces memory footprint and improves cache locality</td></tr>
    <tr><td>Packing</td><td>Use pragmas carefully</td><td>May break ABI portability</td></tr>
    <tr><td>Init</td><td>Use designated initializers (C99)</td><td>Clearer & safer than positional init</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: designated initializer

```c
struct Point { double x, y; };
struct Point p = {.x = 0.0, .y = 0.0};
```

### Exercises (Appendix — structs_data_c-appendix-20251005-01)

1. Reorder fields in a struct to minimize padding and measure `sizeof` before/after.
2. Use designated initializers to create clear example data and add checks.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->


<!-- Practical Appendix: Reference and further reading -->

### Practical Appendix
This appendix contains brief practical notes and quick references to complement the lesson content. It is intentionally short and safe: no code execution or large data dumps.

- Reference: Official documentation and language core references are excellent further reading sources. Follow the standard docs for authoritative examples.
- Quick tips:
  - Re-run the examples in a REPL to experiment with small changes.
  - Use small, focused test cases when validating behavior.
  - Prefer idiomatic standard-library helpers for clarity and maintainability.

Further reading and sources:
- Official language documentation (search for "official <LANG> docs" where <LANG> is the lesson's language).
- Standard library reference and API pages.
- For curriculum authors: keep examples minimal and include runnable snippets in fenced code blocks.

*End of Practical Appendix.*
