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
