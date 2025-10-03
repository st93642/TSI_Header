# Variables and Calculations in C

You have already printed messages and captured basic input. The next step is learning how C stores data, performs arithmetic, and keeps results consistent. This lesson introduces the primitive types you will use daily, shows how to combine them with operators, and highlights the conversions you must apply before calculations go wrong.

## 1. Recognise common primitive types

C provides a small set of core types that map closely to hardware capabilities. Choosing the right one reduces bugs and keeps memory usage predictable.

```c
#include <stdio.h>

int main(void)
{
    int    enrolled = 1800;      /* whole numbers */
    double passRate = 0.87;       /* fractions */
    char   grade    = 'A';        /* single character */

    printf("Students: %d\n", enrolled);
    printf("Pass rate: %.2f\n", passRate);
    printf("Final grade: %c\n", grade);

    return 0;
}
```

- `int` is the workhorse for counting, indexing, and logic branches.
- `double` stores fractional values; it is safer than `float` for real-world calculations.
- `char` represents a single byte. Use single quotes around characters.

### Checkpoint: Identify the type

1. Create `types_demo.c`, copy the snippet above, and compile it.
2. Change `grade` from `'A'` to `'B'`. Observe that the format specifier `%c` still prints the letter.
3. Add a comment explaining when you would replace `double passRate` with `int passRate`.

## 2. Declare and initialise variables responsibly

C does not guarantee default values—reading an uninitialised variable leads to undefined behaviour. Always assign a starting value before using a variable.

```c
#include <stdio.h>

int main(void)
{
    int creditsPerSemester = 30;       /* named constants make intent clear */
    int semestersCompleted = 2;
    int totalCredits = creditsPerSemester * semestersCompleted;

    printf("Total credits earned: %d\n", totalCredits);

    return 0;
}
```

- Initialise variables on the same line whenever you can.
- Use descriptive names; the compiler ignores whitespace, but humans need context.
- Group related variables to make future refactors safer.

### Checkpoint: Refactor with new data

1. Introduce a new student `int semestersCompleted = 3;` and recompute `totalCredits`.
2. Add `int creditsRequired = 180;` and derive `int creditsRemaining`.
3. Print the remaining credits with a clear label (for example, `"Credits remaining: 90"`).

## 3. Manage constants with `const` and macros

Use `const` for values that should not change after initialisation. Prefer `const` over macros for typed constants; reserve macros for compile-time switches or array sizes needed before compilation.

```c
#include <stdio.h>

#define MAX_STUDENTS 2400

int main(void)
{
    const double TUITION_FEE = 1800.00;
    int enrolledStudents = 1725;

    double tuitionRevenue = enrolledStudents * TUITION_FEE;
    int availableSeats = MAX_STUDENTS - enrolledStudents;

    printf("Tuition revenue: %.2f EUR\n", tuitionRevenue);
    printf("Seats remaining: %d\n", availableSeats);

    return 0;
}
```

- `const` integrates with the type system; the compiler can warn when you try to mutate the value.
- `#define` inserts the literal text during preprocessing. Write macro names in uppercase to signal the difference.
- Mix both to balance flexibility and safety.

### Checkpoint: Spot the mutation

1. Try assigning `TUITION_FEE = 0;` and recompile; note the compiler error.
2. Replace `MAX_STUDENTS` with a lowercase identifier and notice how the macro still expands, reinforcing why uppercase is conventional.
3. Update the program to print a warning when `availableSeats` reaches zero.

## 4. Combine values with arithmetic operators

All the standard operators work in C, but their behaviour depends on the types involved. Integer division truncates; mixing types without a cast often leads to incorrect results.

```c
#include <stdio.h>

int main(void)
{
    int totalMinutes = 2500;
    int studyDays = 7;

    int minutesPerDay = totalMinutes / studyDays;          /* integer division */
    double preciseMinutes = totalMinutes / (double)studyDays; /* promoted to double */

    int remainder = totalMinutes % studyDays;               /* modulo */

    printf("Minutes per day: %d\n", minutesPerDay);
    printf("Precise minutes: %.2f\n", preciseMinutes);
    printf("Leftover minutes: %d\n", remainder);

    return 0;
}
```

- The cast `(double)studyDays` promotes the divisor so the division keeps its fractional component.
- `%` returns the remainder, useful for scheduling or chunking tasks.
- Combine operators with parentheses to clarify evaluation order.

### Checkpoint: Track study blocks

1. Ask the user for `totalMinutes` and `studyDays` using `scanf`.
2. Compute `hoursPerDay` by dividing by `60.0` after casting to `double`.
3. Print a report that shows minutes per day, hours per day (one decimal place), and leftover minutes.

## 5. Convert between types deliberately

Implicit conversions follow strict rules: arithmetic on integers produces integers, and promotions never reduce precision. When you need a different result type, cast explicitly and store the value in a clearly named variable.

```c
#include <stdio.h>

int main(void)
{
    int assignmentsSubmitted = 47;
    int assignmentsTotal = 52;

    double completionRatio = assignmentsSubmitted / (double)assignmentsTotal;
    double completionPercent = completionRatio * 100.0;

    printf("Completion ratio: %.4f\n", completionRatio);
    printf("Completion percent: %.2f%%\n", completionPercent);

    return 0;
}
```

- Cast one operand before dividing to avoid truncation.
- Multiplying by `100.0` keeps the result in floating-point so formatting works as expected.
- When combining integers and doubles, store the output in a `double` even if you plan to round later.

### Checkpoint: Choose the right container

1. Record the number of completed labs, quizzes, and projects using `int` variables.
2. Compute the total tasks and the ratio of each category to the whole.
3. Print the ratios as percentages, formatted to one decimal place.

## 6. Mini project: Degree progress calculator

Bring everything together by building a structured report for a fictional student.

1. Store immutable programme data (`totalCredits`, `creditsPerSemester`, tuition per credit) using `const` or macros where appropriate.
2. Read the students name, completed credits, and average weekly study minutes.
3. Calculate remaining credits, estimated semesters left, and the difference between integer division and floating-point estimates.
4. Print a multi-line summary that includes tuition projections and a warning if the timetable requires more than 40 hours (2,400 minutes) per week.

### Success criteria

- The programme compiles with `gcc -Wall -Wextra -Werror` and produces no warnings.
- All monetary values format with two decimal places; percentages format with one.
- Edge cases (zero credits completed, extremely high weekly study minutes) still produce meaningful output and warnings.

## Recap and next steps

You can now:

- Pick the correct primitive type for integers, floating-point numbers, and characters.
- Initialise variables, guard constants, and prevent accidental mutation.
- Combine arithmetic operators with explicit casts to control the result type.

Next, tackle the companion exercise to reinforce these ideas by building a reusable progress report function, then move on to control flow so your programmes can react to real-world scenarios.
