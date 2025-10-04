# Error Handling and Debugging in C

Robust C programs handle errors gracefully and are debuggable. This lesson covers error codes, assertions, logging, and debugging techniques using gdb. You'll learn to validate inputs, handle system errors, and trace program execution.

## Learning goals

- Use errno and perror for system errors.
- Implement custom error codes and return values.
- Apply assertions for debugging.
- Log messages at different levels.
- Debug with gdb: breakpoints, watchpoints, backtraces.
- Handle signals and exceptions.

## 1. Error handling basics

Check return values, set errno.

```c
#include <stdio.h>
#include <errno.h>

int main(void) {
    FILE *fp = fopen("nonexistent", "r");
    if (fp == NULL) {
        perror("fopen");
        return 1;
    }
    fclose(fp);
    return 0;
}
```

### Checkpoint: perror

1. Try fopen invalid file, print error.

## 2. Custom error codes

Define enums for errors.

```c
typedef enum {
    SUCCESS = 0,
    INVALID_INPUT,
    FILE_ERROR
} ErrorCode;

ErrorCode process(int x) {
    if (x < 0) return INVALID_INPUT;
    return SUCCESS;
}
```

### Checkpoint: Enum errors

1. Define error codes, use in function.

## 3. Assertions

Assert conditions in debug builds.

```c
#include <assert.h>

int divide(int a, int b) {
    assert(b != 0);
    return a / b;
}
```

### Checkpoint: Assert

1. Add assert to prevent division by zero.

## 4. Logging

Print debug info.

```c
#define LOG(level, msg) printf("[%s] %s\n", level, msg)

int main(void) {
    LOG("INFO", "Starting");
    // code
    LOG("ERROR", "Failed");
}
```

### Checkpoint: Log

1. Define LOG macro, use in program.

## 5. Debugging with gdb

Compile with -g, run gdb.

```bash
gcc -g program.c -o program
gdb program
```

Commands: break, run, next, print, backtrace.

### Checkpoint: gdb

1. Set breakpoint, run, inspect variables.

## 6. Valgrind for memory

Check leaks.

```bash
valgrind --leak-check=full ./program
```

### Checkpoint: Valgrind

1. Run valgrind on program with malloc/free.

## 7. Signal handling

Catch signals.

```c
#include <signal.h>

void handler(int sig) {
    printf("Signal %d\n", sig);
}

int main(void) {
    signal(SIGINT, handler);
    while(1);
}
```

### Checkpoint: Signal

1. Handle SIGINT, print message.

## 8. Input validation

Sanitize inputs.

```c
int safe_atoi(const char *str, int *result) {
    char *end;
    *result = strtol(str, &end, 10);
    return *end == '\0';
}
```

### Checkpoint: Validate

1. Write function to safely convert string to int.

## 9. Error propagation

Pass errors up.

```c
ErrorCode func1() {
    if (fail) return ERROR;
    return SUCCESS;
}

int main(void) {
    if (func1() != SUCCESS) {
        // handle
    }
}
```

### Checkpoint: Propagate

1. Chain error returns.

## 10. Debugging tips

Use printf, avoid undefined behavior.

### Checkpoint: Tips

1. List debugging strategies.

## 11. Mini project: Safe calculator

Calculator with error handling.

1. Validate inputs.
2. Handle division by zero.
3. Log operations.

### Success criteria

- Handles all errors.
- Logs appropriately.

## 12. Guided practice challenges

1. File error recovery.
2. Network error handling.
3. Multi-threaded error sync.
4. Custom assert macro.
5. Debug logging system.

## 13. Self-check questions

1. Difference between perror and strerror?
2. When to use assert?
3. gdb commands for breakpoints?
4. How to check memory leaks?

## Recap and next steps

Error handling makes programs reliable. Next, explore advanced C features.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Debugging & Tools (Appendix — Debug Tools — error_debug_c)

This appendix lists quick debugging recipes for C lessons: using gdb, AddressSanitizer (ASAN), and Valgrind for memory and runtime issues. Links point to Valgrind and standard debugging tools.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Primary Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>gdb</td><td>Interactive debugging</td><td>Use `break`, `run`, `backtrace`, `print`</td></tr>
    <tr><td>ASAN</td><td>Fast memory bugs at runtime</td><td>Compile with `-fsanitize=address -g`</td></tr>
    <tr><td>Valgrind</td><td>Deep Memcheck reporting</td><td>Use when ASAN isn't available or for detailed leak reports</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### gdb quick recipe

```bash
# compile with debug symbols
gcc -g -O0 -o lesson_bin main.c
# run under gdb
gdb --args ./lesson_bin arg1 arg2
# inside gdb:
# (gdb) break main
# (gdb) run
# (gdb) bt
# (gdb) print var
```

### ASAN quick recipe

Compile and run to get immediate diagnostics:

```bash
gcc -fsanitize=address -g -O1 -o lesson_bin main.c
./lesson_bin
```

ASAN outputs stack traces and allocation info for invalid accesses.

### Exercises (error_debug_c)

1. Reproduce a small heap-use-after-free bug, show the ASAN output, and fix the code.
2. Run the fixed program under Valgrind and add a short note about differences in output between ASAN and Valgrind.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Debugging Tools — Recipes (Appendix — error_debug_c-appendix2)

Compact recipes for using gdb, ASAN, and Valgrind in exercises and CI, plus notes on interpreting core outputs.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Command</th><th>When to use</th></tr>
  </thead>
  <tbody>
    <tr><td>gdb</td><td>gdb --args ./program</td><td>Interactive stepping and backtraces</td></tr>
    <tr><td>ASAN</td><td>gcc -fsanitize=address -g</td><td>Fast detection of invalid memory</td></tr>
    <tr><td>Valgrind</td><td>valgrind --leak-check=full</td><td>Deep leak analysis</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### ASAN vs Valgrind note

- ASAN is fast and provides immediate diagnostic output during runtime.
- Valgrind is slower but can provide more detailed leak tracing on some platforms.

### Quick gdb recipe

```bash
# compile
gcc -g -O0 program.c -o program
# run
gdb --args ./program arg1 arg2
# inside gdb
# (gdb) break main
# (gdb) run
# (gdb) bt
# (gdb) print var
```

### Exercises (Appendix — error_debug_c-appendix2)

1. Produce a small use-after-free example and show ASAN output; then fix it and verify no leaks with Valgrind.
2. Create a CI job (optional) that compiles with `-fsanitize=address` and runs the binary as a smoke test.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
