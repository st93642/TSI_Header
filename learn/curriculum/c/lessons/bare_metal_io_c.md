<!-- Practical Appendix: Reference and further reading -->

<!-- markdownlint-disable MD013 -->
### Practical Appendix

This appendix contains brief practical notes and quick references to complement the lesson content. It is intentionally short and safe: no code execution or large data dumps.
- Further reading and sources:

- Official language documentation (search for `official <LANG> docs` where `<LANG>` is the lesson's language).
- Standard library reference and API pages.
# Your First C Programs

Welcome to C! Instead of diving straight into operating system details, this lesson focuses on the classic workflow every beginner follows: write a small program, compile it with `gcc`, run it, and gradually add variables, formatted output, and user input. Every section is self-contained and ends with a checkpoint so you can confirm you understand the idea before moving on.

## 1. Understand the shape of a C program

A C program is built from functions. The entry point is a function named `main` that always returns an `int`.

```c
#include <stdio.h>

int main(void)
{
   printf("Hello, C!\n");
   return 0;
}
```

- `#include <stdio.h>` pulls in declarations for `printf` and other standard input/output helpers.
- `int main(void)` means “main returns an integer and takes no arguments.”
- `return 0;` signals to the operating system that the program completed successfully.

### Checkpoint: Create `hello.c`

1. Save the snippet above as `hello.c`.
2. Run `gcc hello.c -o hello`.
3. Execute `./hello` and confirm the message `Hello, C!` appears on its own line.

If you mistype the code, `gcc` will show an error with the offending line number. Fix the issue and recompile until the program runs.

## 2. Compile and run with confidence

You will repeat the edit → compile → run cycle constantly. Make it comfortable early on.

```bash
gcc -Wall -Wextra -Werror hello.c -o hello
./hello
echo $?
```

- `-Wall -Wextra -Werror` enables helpful warnings and promotes them to errors so you notice potential mistakes immediately.
- `echo $?` prints the exit status of the last command; a value of `0` confirms success.

### Checkpoint: Trigger and clear a warning

1. Change the code so `printf` is written without the final semicolon.
2. Run the `gcc` command again and observe the compiler’s diagnostic.
3. Fix the code and recompile until the build succeeds without warnings.

Learning to read compiler messages early saves hours of debugging later.

## 3. Print richer messages with `printf`

`printf` supports format specifiers so you can weave numbers and text together.

```c
#include <stdio.h>

int main(void)
{
   const char *program = "Transport and Telecommunication Institute";
   int year = 2025;

   printf("Welcome to %s!\n", program);
   printf("We are starting the %d academic year.\n", year);
   printf("Keep coding!\n");

   return 0;
}
```

- `%s` inserts a string, `%d` inserts an integer, and `\n` moves to the next line.
- Variables can be declared as `const` when their values never change.

### Checkpoint: Format practice

1. Add a floating-point variable named `goalGPA` with the value `3.8`.
2. Print the message `Target GPA: 3.8` using `printf("Target GPA: %.1f\n", goalGPA);`.
3. Recompile and run to confirm the number shows one digit after the decimal point.

## 4. Work with variables and calculations

You can combine integers and doubles to compute useful information.

```c
#include <stdio.h>

int main(void)
{
   int completedCredits = 32;
   int totalCredits = 180;
   int remainingCredits = totalCredits - completedCredits;
   double completionPercentage = (completedCredits / (double)totalCredits) * 100.0;

   printf("Completed credits: %d\n", completedCredits);
   printf("Remaining credits: %d\n", remainingCredits);
   printf("Completion percentage: %.1f%%\n", completionPercentage);

   return 0;
}
```

- Cast one operand to `double` so the division preserves the fractional part.
- `%%` prints a literal percent sign.

### Checkpoint: Experiment with numbers

Change `completedCredits` to another value and rerun the program. Verify that `remainingCredits` and `completionPercentage` update automatically. If the percentage becomes `0.0`, ensure you kept the `(double)` cast.

## 5. Read input with `scanf`

Programs often need user data. `scanf` reads values from standard input.

```c
#include <stdio.h>

int main(void)
{
   int studyDays = 0;
   int minutesPerDay = 0;

   printf("Enter study days in a week: ");
   scanf("%d", &studyDays);

   printf("Enter minutes per day: ");
   scanf("%d", &minutesPerDay);

   int totalMinutes = studyDays * minutesPerDay;
   double averagePerDay = (studyDays > 0) ? (totalMinutes / (double)studyDays) : 0.0;

   printf("Total minutes: %d\n", totalMinutes);
   printf("Average per day: %.1f\n", averagePerDay);

   return 0;
}
```

- Each `scanf` needs the address of the variable (`&studyDays`).
- Use the ternary operator to avoid dividing by zero when the user enters `0` days.

### Checkpoint: Try sample input

Run the program and enter `5` and `120`. The output should show `Total minutes: 600` and `Average per day: 120.0`. Repeat with different numbers to see how the calculations change.

## 6. Mini project: Student snapshot

You now have everything needed for the exercise: declarations, calculations, formatted output, and basic input.

1. Create a program that prints a multi-line overview of a student: name, programme, total credits, completed credits, remaining credits, and completion percentage.
2. Extend it by asking the user for the number of study days and minutes per day, then compute the total study time for the week.
3. Present all results using neatly formatted `printf` lines similar to the examples above.

### Success criteria

- The program compiles cleanly with `gcc -Wall -Wextra -Werror`.
- All numeric values display with clear labels (for example, `Completion percentage: 17.8%`).
- When run with different study schedules, the totals update automatically without touching the code.

## 7. Accept command-line arguments

Many console utilities read data from command-line arguments instead of prompting users interactively. Update `main` to accept parameters and convert them to numbers with `strtol` or `strtod`.

```c
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
   if (argc != 3)
   {
      fprintf(stderr, "Usage: %s <credits> <goal-gpa>\n", argv[0]);
      return 1;
   }

   int credits = (int)strtol(argv[1], NULL, 10);
   double goal = strtod(argv[2], NULL);

   printf("Credits: %d\n", credits);
   printf("Goal GPA: %.2f\n", goal);

   return 0;
}
```

- `argc` counts the arguments; `argv[0]` is the programme name.
- Use `fprintf(stderr, ...)` for error messages so they appear even when standard output is redirected.
- Always validate the argument count before reading values to avoid accessing out-of-range memory.

### Checkpoint: Convert your snapshot

1. Modify the student snapshot mini project so it accepts completed and total credits as command-line inputs.
2. Provide defaults (for example, fall back to interactive prompts) when arguments are missing.
3. Test the programme both with and without arguments and confirm output stays accurate.

## 8. Organise logic with helper functions

Even small programmes benefit from breaking work into functions. Helper functions improve reusability and make unit testing possible.

```c
#include <stdio.h>

int remaining_credits(int total, int completed)
{
   return total - completed;
}

double completion_percent(int total, int completed)
{
   if (total == 0)
   {
      return 0.0;
   }
   return (completed / (double)total) * 100.0;
}

int main(void)
{
   int total = 180;
   int completed = 42;

   printf("Remaining: %d\n", remaining_credits(total, completed));
   printf("Percent: %.1f%%\n", completion_percent(total, completed));

   return 0;
}
```

- Place helper functions above `main` or declare prototypes at the top of the file.
- Keep functions focused: single responsibility, descriptive names, and explicit parameter types.
- Later lessons will show how to move helpers into separate translation units for larger projects.

### Checkpoint: Extract repeated work

1. Identify a block of code in `main` that performs a calculation and converts it into a helper function.
2. Add documentation comments above each helper describing inputs, outputs, and failure cases.
3. Compile with `-Wall` to ensure you declared prototypes correctly and that no implicit conversions occur.

## 9. Split programmes across multiple source files

Once helpers grow numerous, place them in separate `.c` files with corresponding headers. Compile each file then link the results.

```bash
gcc -Wall -Wextra -Werror progress.c calculations.c -o progress
```

Example layout:

```c
/* calculations.h */
#ifndef CALCULATIONS_H
#define CALCULATIONS_H

int remaining_credits(int total, int completed);
double completion_percent(int total, int completed);

#endif
```

```c
/* calculations.c */
#include "calculations.h"

int remaining_credits(int total, int completed)
{
   return total - completed;
}

double completion_percent(int total, int completed)
{
   if (total == 0)
   {
      return 0.0;
   }
   return (completed / (double)total) * 100.0;
}
```

```c
/* progress.c */
#include <stdio.h>
#include "calculations.h"

int main(void)
{
   printf("Remaining: %d\n", remaining_credits(180, 42));
   printf("Percent: %.1f%%\n", completion_percent(180, 42));
   return 0;
}
```

- Header guards (`#ifndef`, `#define`, `#endif`) prevent double inclusion errors.
- Keep shared declarations in headers and implementations in `.c` files to clarify dependencies.
- Recompile both translation units whenever you modify shared headers.

### Checkpoint: Modularise your snapshot

1. Move calculations into `calculations.c` and include a header in `main`.
2. Create a `io_helpers.c` file that wraps `scanf` prompts, returning `int` success status codes.
3. Build the project with a single `gcc` command that lists every source file, confirming no undefined reference errors occur.

## 10. Explore debugging workflows

Compiler warnings help, but deeper issues require debugging tools. Start with `printf`-style logging, then graduate to `gdb` or `lldb`.

```c
#include <stdio.h>

int main(void)
{
   int target = 100;
   int guess = 0;

   printf("DEBUG: target=%d\n", target);

   do
   {
      printf("Enter a guess: ");
      if (scanf("%d", &guess) != 1)
      {
         printf("Invalid input.\n");
         return 1;
      }

      if (guess < target)
      {
         printf("Too low.\n");
      }
      else if (guess > target)
      {
         printf("Too high.\n");
      }

   } while (guess != target);

   printf("Correct!\n");
   return 0;
}
```

- Prefix debug messages so you can filter them out with tools like `grep`.
- Compile with `-g` to include debug symbols, then run `gdb ./program` to step through execution line by line.
- Set breakpoints (`break main`), run the programme, inspect variables (`print guess`), and continue (`next`, `continue`) to understand the control flow.

### Checkpoint: Trace a bug interactively

1. Introduce an intentional bug (for example, a missing `else`) and use `gdb` to locate the faulty branch.
2. Practice the `backtrace` command to view the call stack when your programme hits an error.
3. Remove debug prints once the issue is resolved to keep output clean.

## 11. Automate builds with `make`

As projects grow, manual compilation becomes cumbersome. A simple `Makefile` captures build rules and dependencies.

```make
CC = gcc
CFLAGS = -Wall -Wextra -Werror -std=c11 -g

all: progress

progress: progress.o calculations.o
   $(CC) $(CFLAGS) $^ -o $@

progress.o: progress.c calculations.h
   $(CC) $(CFLAGS) -c $<

calculations.o: calculations.c calculations.h
   $(CC) $(CFLAGS) -c $<

clean:
   rm -f progress *.o
```

- `$@` expands to the target name, `$^` to all prerequisites, and `$<` to the first prerequisite.
- The tab characters before commands are required; configure your editor accordingly.
- Run `make` to compile only the files that changed, and `make clean` to remove build artefacts.

### Checkpoint: Script your workflow

1. Create a `Makefile` for the snapshot project and verify `make` builds without errors.
2. Add a `run` target that depends on `progress` and executes the programme with sample arguments.
3. Document in a README how classmates should build and run your project using the new targets.

## 12. Capstone: Intake survey assistant

Combine everything into a richer console app that supports both interactive prompts and command-line arguments.

1. Accept optional arguments for student name, total credits, completed credits, and target GPA. If arguments are missing, fall back to interactive prompts.
2. Move calculations into dedicated source files, compile with a Makefile, and ensure each function has a prototype in a header.
3. Add debug logging controlled by a `--debug` flag; when enabled, print intermediate values using `fprintf(stderr, ...)`.
4. Provide a summary report that includes completion percentage, weekly study plan, and a status message derived from conditional checks.
5. Exit with non-zero status codes on invalid input, and print usage instructions to assist future users.

### Stretch goals

- Persist survey results to a CSV file by opening it with `fopen` in append mode.
- Integrate a lightweight unit test harness that reuses helper functions without running the entire interactive flow.
- Experiment with sanitiser builds (`-fsanitize=address`) to catch memory bugs introduced while expanding the codebase.

When these boxes are checked, you are ready for the accompanying exercises that reinforce the same ideas in a guided setting.


<!-- Practical Appendix: Reference and further reading -->

<!-- markdownlint-disable MD013 -->
### Practical Appendix
This appendix contains brief practical notes and quick references to complement the lesson content. It is intentionally short and safe: no code execution or large data dumps.

- Reference: Official documentation and language core references are excellent further reading sources. Follow the standard docs for authoritative examples.
- Quick tips:
  - Re-run the examples in a REPL to experiment with small changes.
  - Use small, focused test cases when validating behavior.
  - Prefer idiomatic standard-library helpers for clarity and maintainability.

Further reading and sources:
- Official language documentation (search for `official <LANG> docs` where `<LANG>` is the lesson's language).
- Standard library reference and API pages.
- For curriculum authors: keep examples minimal and include runnable snippets in fenced code blocks.

*End of Practical Appendix.*

<!-- markdownlint-enable MD013 -->