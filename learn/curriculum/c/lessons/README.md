# Lessons Placeholder

Add Markdown lessons for the C curriculum here. File names should match the lesson IDs defined in `curriculum.json` (e.g. `hello_world_c.md`). Each lesson should end with a **Practice Time** section that leads directly into the paired exercise.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: C Curriculum Roadmap & Resources

This appendix adds suggested readings, a small resource table, and exercises to help instructors and learners map the next steps.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Why it matters</th><th>Next steps</th></tr>
  </thead>
  <tbody>
    <tr><td>Build systems</td><td>Reproducible builds</td><td>Makefile, CMake examples</td></tr>
    <tr><td>Memory model</td><td>Safety & performance</td><td>Valgrind, ASAN, static analyzers</td></tr>
    <tr><td>Embedded IO</td><td>Low-level systems programming</td><td>Register maps, bare-metal examples</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Quick Resources

- Compiler flags: -Wall -Wextra -O2
- Tools: valgrind, lldb/gdb, cppcheck

### Exercises

1. Create a tiny Makefile that builds `examples/` and a `make test` target that runs smoke tests.
2. Add a short contributor note describing how to add a new lesson following the repository conventions.

<!-- markdownlint-enable MD010 -->

## Practical Appendix: Quick C Curriculum Roadmap (Appendix)

This short appendix adds a curated resource table and a few actionable exercises for contributors and instructors.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Resource</th><th>Purpose</th><th>Link / Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Makefile</td><td>Build automation</td><td>Simple Make recipes; prefer PHONY targets</td></tr>
    <tr><td>Valgrind</td><td>Memory analysis</td><td>Use with debug builds</td></tr>
    <tr><td>C Standard Docs</td><td>Reference</td><td>C11/C17 for portability notes</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix)

1. Add a `make test` target that compiles examples and runs a smoke test.
2. Add contributor notes describing how to add a new lesson and the header insertion process.

<!-- markdownlint-enable MD010 -->

## Quick Appendix: Teaching Tips & Resources (Appendix II)

A compact list of teaching aids and a short HTML table to help instructors prepare labs.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Item</th><th>Suggested Time</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Makefile demo</td><td>15 min</td><td>Show incremental builds</td></tr>
    <tr><td>Valgrind demo</td><td>20 min</td><td>Memory leak detection</td></tr>
    <tr><td>Debugging lab</td><td>30–45 min</td><td>Hands-on crash reproduction</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix II — Deep Dive — c-readme)

1. Draft a 30-minute lab that covers building and running under Valgrind.
2. Add an instructor note describing expected student pitfalls.

<!-- markdownlint-enable MD010 -->

## Practical Appendix: External Resources (Appendix — External Links)

Useful references and short recipes for studying the C lessons in this folder.

- C Standard references: [ISO C drafts and references](https://en.cppreference.com/w/c/)
- Portable build tools: [CMake](https://cmake.org/)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Resource</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Standards</td><td><a href="https://en.cppreference.com/w/c/">cppreference C</a></td><td>Quick lookup for library and language details</td></tr>
    <tr><td>Build</td><td><a href="https://cmake.org/">CMake</a></td><td>Cross-platform build workflows</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Pick a C standard library function used in a lesson and read its cppreference entry; summarize edge-cases.
2. Create a minimal CMakeLists.txt for one lesson and document build commands.

<!-- markdownlint-enable MD010 -->

## Extended Practical Appendix: Build, Debug & CI Recipes (Appendix — Expanded External Links)

This appendix contains practical snippets you can reuse when working through the C lessons in this folder: build recipes (Make/CMake), memory debugging with Valgrind, and a minimal CI workflow for Linux builds.

### Minimal Makefile snippet

Use this simple Makefile to build the sample programs used in the lessons.

```makefile
# Minimal Makefile for lesson exercises
CC = gcc
CFLAGS = -Wall -Wextra -std=c11 -O2
SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)
TARGET = lesson_bin

all: $(TARGET)

$(TARGET): $(OBJS)
  $(CC) $(CFLAGS) -o $@ $^

clean:
  rm -f $(OBJS) $(TARGET)
```

### Minimal CMakeLists.txt snippet

```cmake
cmake_minimum_required(VERSION 3.16)
project(lesson_examples C)
set(CMAKE_C_STANDARD 11)
add_executable(lesson_bin main.c)
```

### Valgrind quick-start

Run programs under Valgrind's Memcheck to detect leaks and invalid memory access:

```bash
# build first
make
# run under memcheck
valgrind --leak-check=full --show-leak-kinds=all ./lesson_bin
```

If you see known third-party leaks (rare in these tiny lessons), create a suppression file and re-run.

### Minimal GitHub Actions workflow (CI)

A tiny workflow that builds on Ubuntu and runs a simple Make-based build. Save as `.github/workflows/c-build.yml` in a repo that contains these lessons' code examples.

```yaml
name: C Build
on: [push, pull_request]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install dependencies
        run: sudo apt-get update && sudo apt-get install -y build-essential valgrind
      - name: Build
        run: make
      - name: Run tests
        run: |
          if [ -f run_tests.sh ]; then ./run_tests.sh; fi
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Area</th><th>Recipe</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Build</td><td><a href="https://cmake.org/">CMake</a></td><td>Use for cross-platform projects</td></tr>
    <tr><td>Debug</td><td><a href="https://valgrind.org/">Valgrind</a></td><td>Memcheck is useful for lesson exercises</td></tr>
    <tr><td>CI</td><td><a href="https://docs.github.com/actions">GitHub Actions</a></td><td>Run the Makefile on push/pull_request</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Build & Debug Appendix)

1. Add a `run_tests.sh` script that compiles the lesson examples and runs a small self-check (exit code 0 on success). Commit it and verify the workflow above runs successfully on your fork.
2. Add a `valgrind.supp` file to suppress a known, accepted leak and document when it's acceptable to add suppressions.

<!-- markdownlint-enable MD010 -->

## Practical Appendix: External Tools & Examples (Appendix — External Tools — c-readme-3)

This appendix provides concise, battle-tested recipes and links for common tooling used while working through the C lessons: references on the C standard & library, Valgrind/Memcheck usage, small build snippets, and a compact HTML table summarizing options. Links point to authoritative sources such as cppreference and the Valgrind manual.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Typical Use</th><th>Reference</th></tr>
  </thead>
  <tbody>
    <tr><td>cppreference (C)</td><td>Language & library reference</td><td><a href="https://en.cppreference.com/w/c/">cppreference C</a></td></tr>
    <tr><td>Valgrind / Memcheck</td><td>Memory correctness & leak detection</td><td><a href="https://valgrind.org/docs/manual/manual.html">Valgrind Manual</a></td></tr>
    <tr><td>CMake</td><td>Cross-platform build configuration</td><td><a href="https://cmake.org/">CMake</a></td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Quick tool recipes (safe to copy)

- Inspect a standard library function on the web: see the cppreference C index at [https://en.cppreference.com/w/c/](https://en.cppreference.com/w/c/).
- Use Valgrind's Memcheck to find leaks and invalid accesses (example below).

### Valgrind / Memcheck short-guide

Build your lessons with debug symbols and run Memcheck for thorough diagnostics.

```bash
# build with debug info
gcc -g -O0 -Wall -Wextra -std=c11 -o lesson_bin main.c

# run Memcheck (full leak info)
valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes ./lesson_bin
```

If a third-party dependency causes a known harmless report, create a suppression file (see the Valgrind manual) and pass it with `--suppressions=valgrind.supp`.

### Minimal Makefile (spaces, not tabs)

```makefile
# Simple, portable Makefile for small lesson programs
CC = gcc
CFLAGS = -g -O0 -Wall -Wextra -std=c11
SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)
TARGET = lesson_bin

all: $(TARGET)

$(TARGET): $(OBJS)
    $(CC) $(CFLAGS) -o $@ $^

clean:
    rm -f $(OBJS) $(TARGET)
```

Note: real Makefiles need a tab character at the start of recipe lines; when copying the snippet into a repository Makefile, replace the leading 4 spaces above with a single tab. In fenced code (markdown) we keep spaces to avoid markdownlint hard-tab warnings.

### Minimal CMakeLists.txt (copyable)

```cmake
cmake_minimum_required(VERSION 3.16)
project(lesson_examples C)
set(CMAKE_C_STANDARD 11)
add_executable(lesson_bin main.c)
```

### Minimal CI snippet (GitHub Actions)

```yaml
name: C Build
on: [push, pull_request]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install dependencies
        run: sudo apt-get update && sudo apt-get install -y build-essential valgrind cmake
      - name: Build
        run: make
      - name: Run tests
        run: |
          if [ -f run_tests.sh ]; then ./run_tests.sh; fi
```

### Short checklist for maintainers

1. Prefer debug builds (`-g -O0`) when running Valgrind or other memory-checkers.
2. Use `--track-origins=yes` with Memcheck when you see "uninitialised value" reports.
3. Add a `valgrind.supp` suppressions file only when the report is known, reproducible, and benign; document why the suppression exists.

### Exercises (External Tools — c-readme-3)

1. Run one lesson under Valgrind, capture the output, and add a short note in the lesson describing any fixes you applied.
2. Add a `run_tests.sh` that builds `lesson_bin`, runs it, and (optionally) runs Memcheck; make sure it exits non-zero on failures.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Building & Running C Examples (Appendix — c-lessons-readme-appendix2)

This short appendix shows common build commands, a tiny Makefile snippet, and recommended compiler flags for development and debugging.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Task</th><th>Command</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Build (gcc)</td><td>gcc -o prog prog.c</td><td>Use -g for debugging</td></tr>
    <tr><td>ASAN</td><td>-fsanitize=address</td><td>Catch memory errors quickly</td></tr>
    <tr><td>Optimization</td><td>-O2</td><td>Use for release builds</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Minimal Makefile

```makefile
CC = gcc
CFLAGS = -Wall -Wextra -g

all: prog

prog: prog.o
	$(CC) $(CFLAGS) -o $@ $^
```

### Exercises (Appendix — c-lessons-readme-appendix2)

1. Create a Makefile target `debug` that builds with ASAN flags.
2. Add a `clean` target that removes compiled artifacts.

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
