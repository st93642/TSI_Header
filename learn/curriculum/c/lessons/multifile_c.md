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

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Build & Debug Resources (Appendix — External Links)

Authoritative tools for multi-file C projects and debugging.

- Valgrind manual: [Valgrind manual](https://valgrind.org/docs/manual/manual.html)
- Build guidance: use Make/CMake; see [CMake](https://cmake.org/)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Link</th><th>Use</th></tr>
  </thead>
  <tbody>
    <tr><td>Valgrind</td><td><a href="https://valgrind.org/docs/manual/manual.html">Valgrind manual</a></td><td>Memory/debugging</td></tr>
    <tr><td>CMake</td><td><a href="https://cmake.org/">CMake</a></td><td>Cross-platform builds</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Run the project under Valgrind and capture a leak report; add suppression rules for known third-party leaks.
2. Convert the Makefile to a minimal CMakeLists.txt and verify builds on Linux and macOS.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Multi-file Builds & Debugging (Appendix — MultiFile — multifile_c-appendix)

Notes on organizing multi-file C programs, compilation units, linker usage, and common pitfalls.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Area</th><th>Why</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Compilation Units</td><td>Separate translation units for clarity</td><td>Compile each `.c` to `.o` and link</td></tr>
    <tr><td>Linking</td><td>Combine object files</td><td>Use `gcc -o prog a.o b.o`</td></tr>
    <tr><td>Header Hygiene</td><td>Avoid duplicate definitions</td><td>Use include guards or `#pragma once`</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example Makefile snippet

```makefile
CC=gcc
CFLAGS=-g -O0 -Wall -std=c11
SRCS=main.c module.c
OBJS=$(SRCS:.c=.o)

all: lesson_bin

lesson_bin: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f $(OBJS) lesson_bin
```

### Exercises (multifile_c-appendix)

1. Split a simple one-file example into `module.c` / `module.h` and update the Makefile to build separate objects.
2. Demonstrate a linker error caused by missing `-lm` (math library) and document the fix.

## Further Reading: Build Systems & Profiling (Appendix — multifile_c-further)

A few links and tips for profiling multi-file C programs and integrating with CI.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Use</th><th>Link</th></tr>
  </thead>
  <tbody>
    <tr><td>Valgrind</td><td>Memory & profiling</td><td><a href="https://valgrind.org/docs/manual/manual.html">Valgrind Manual</a></td></tr>
    <tr><td>gprof / perf</td><td>Profiling</td><td>Use for hotspots analysis</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Multi-file C — Headers, Linking & Build Tips (Appendix — multifile_c-appendix2)

Practical guidance for organizing C code across multiple source and header files, compilation commands, and simple Makefile patterns.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Header guards</td><td>#ifndef / #define</td><td>Prevent multiple inclusion</td></tr>
    <tr><td>Compilation</td><td>cc -c file.c</td><td>Produce object files then link</td></tr>
    <tr><td>Linking</td><td>cc file.o other.o -o prog</td><td>Order matters for static libs</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example header guard

```c
#ifndef MYLIB_H
#define MYLIB_H

void helper(void);

#endif /* MYLIB_H */
```

### Simple Makefile

```makefile
CC = cc
CFLAGS = -Wall -Wextra -O2

all: prog

prog: main.o util.o
	$(CC) $(CFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o prog
```

### Exercises (Appendix — multifile_c-appendix2)

1. Split a single-file C program into `main.c` and `util.c` with `util.h` and provide a Makefile; ensure the program builds and runs.
2. Add a simple unit test using a small test runner (or harness) that links with the compiled objects.

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
