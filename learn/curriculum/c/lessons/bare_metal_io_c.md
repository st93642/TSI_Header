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

When these boxes are checked, you are ready for the accompanying exercises that reinforce the same ideas in a guided setting.
