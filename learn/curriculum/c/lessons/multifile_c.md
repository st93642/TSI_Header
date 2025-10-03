# Multi-file Programs and Compilation in C

C programs can span multiple files for modularity and reusability. This lesson covers separating code into headers and sources, compiling multi-file projects, and linking object files. You'll learn to create libraries, manage dependencies, and use build tools like make.

## Learning goals

- Separate declarations from definitions using headers.
- Compile multiple source files into object files.
- Link object files to create executables.
- Use header guards and proper inclusion.
- Create static libraries.
- Manage build processes with Makefiles.

## 1. Why multi-file programs?

Modularity, reusability, faster compilation.

### Checkpoint: Benefits

1. List advantages of multi-file programs.

## 2. Header files

Declare functions, types, macros.

```c
// math.h
#ifndef MATH_H
#define MATH_H

int add(int a, int b);
double square(double x);

#endif
```

### Checkpoint: Create header

1. Write header for string functions.

## 3. Source files

Implement functions.

```c
// math.c
#include "math.h"

int add(int a, int b) {
    return a + b;
}

double square(double x) {
    return x * x;
}
```

### Checkpoint: Implement

1. Implement functions from header.

## 4. Main file

Use functions.

```c
// main.c
#include <stdio.h>
#include "math.h"

int main(void) {
    printf("%d\n", add(1, 2));
    printf("%.2f\n", square(3.0));
    return 0;
}
```

### Checkpoint: Main

1. Write main using header functions.

## 5. Compilation

gcc -c to compile to .o, then link.

```bash
gcc -c math.c -o math.o
gcc -c main.c -o main.o
gcc math.o main.o -o program
```

### Checkpoint: Compile

1. Compile multi-file program.

## 6. Include paths

-I for headers.

```bash
gcc -I./include -c main.c
```

### Checkpoint: Include path

1. Use -I to include headers from subdir.

## 7. Static libraries

Archive .o files.

```bash
ar rcs libmath.a math.o
gcc main.o -L. -lmath -o program
```

### Checkpoint: Library

1. Create and link static library.

## 8. Makefiles

Automate builds.

```makefile
CC = gcc
CFLAGS = -Wall -g

program: main.o math.o
    $(CC) $^ -o $@

%.o: %.c
    $(CC) $(CFLAGS) -c $< -o $@

clean:
    rm -f *.o program
```

### Checkpoint: Makefile

1. Write Makefile for project.

## 9. Dependencies

Track changes.

### Checkpoint: Depend

1. Update Makefile with dependencies.

## 10. Common issues

Multiple definitions, missing includes.

### Checkpoint: Debug

1. Fix compilation errors.

## 11. Mini project: Calculator library

Create calculator as library.

1. Header with ops.
2. Source with implementations.
3. Main using library.
4. Makefile.

### Success criteria

- Compiles and runs.
- Modular structure.

## 12. Guided practice challenges

1. Separate string utils.
2. Build with shared libs.
3. Cross-compilation.
4. Dependency management.
5. Build scripts.

## 13. Self-check questions

1. Why use headers?
2. Difference between .c and .h?
3. How to create libraries?
4. Purpose of Makefiles?

## Recap and next steps

Multi-file programs enable large projects. Next, explore error handling and debugging.
