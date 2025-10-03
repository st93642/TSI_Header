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

## 7. Inspect type sizes with `sizeof`

Every C implementation defines the width of primitive types, but the exact number of bytes can vary between platforms. Use `sizeof` to confirm assumptions and document them in your programme.

```c
#include <stdio.h>
#include <stdint.h>

int main(void)
{
    printf("Size of char: %zu byte\n", sizeof(char));
    printf("Size of short: %zu bytes\n", sizeof(short));
    printf("Size of int: %zu bytes\n", sizeof(int));
    printf("Size of long: %zu bytes\n", sizeof(long));
    printf("Size of long long: %zu bytes\n", sizeof(long long));
    printf("Size of double: %zu bytes\n", sizeof(double));
    printf("Size of int32_t: %zu bytes\n", sizeof(int32_t));

    return 0;
}
```

- Include `<stdint.h>` when you need fixed-width types such as `int32_t` or `uint8_t` for portable data structures.
- `%zu` prints an unsigned size value; never cast a `size_t` to an `int` just for display purposes.
- Record the results in comments if your project must guarantee a particular memory layout.

### Checkpoint: Inventory the platform

1. Extend the programme to measure `float`, `long double`, and `_Bool`.
2. Print a header line that labels the columns, then align the output using `printf("%-12s %zu\n", ...)`.
3. Compare the byte counts with your compiler documentation and note any differences.

## 8. Control output formatting with precision and width

`printf` offers rich formatting controls so your reports stay readable. Combine width, precision, and flags to align values and show signs consistently.

```c
#include <stdio.h>

int main(void)
{
    double tuition = 1835.5;
    double balance = -420.25;

    printf("%-20s %10.2f\n", "Tuition (EUR)", tuition);
    printf("%-20s %+10.2f\n", "Account balance", balance);
    printf("%-20s %010.2f\n", "Zero-padded", tuition);

    return 0;
}
```

- `%-20s` left-aligns the string within 20 characters.
- `%+10.2f` reserves 10 characters, shows the sign, and keeps two digits after the decimal point.
- `%010.2f` pads with zeros on the left; use it sparingly for financial statements but it is helpful when matching fixed-format specifications.

### Checkpoint: Align a progress table

1. Create arrays for course names and achieved percentages.
2. Loop through them, printing each row with aligned columns and two digits after the decimal.
3. Add a column that shows the raw points out of 100 with zero padding for single-digit scores.

## 9. Guard against overflow and truncation

Small mistakes in type selection can overflow integers or drop fractional precision. Build safeguards into your code.

```c
#include <stdio.h>
#include <limits.h>

int main(void)
{
    int creditsPerSemester = 30;
    int semestersNeeded = 12;

    long long projectedCredits = (long long)creditsPerSemester * semestersNeeded;

    if (projectedCredits > INT_MAX)
    {
        printf("Projected credits exceed int range!\n");
    }
    else
    {
        printf("Projected credits fit in int: %lld\n", projectedCredits);
    }

    return 0;
}
```

- Cast before multiplying so the calculation occurs in a wider type.
- Compare against limits from `<limits.h>` and `<float.h>` to detect when values might not fit.
- When performing financial calculations, prefer integer cents (e.g., store `189900` for `1899.00`) to avoid floating-point rounding surprises.

### Checkpoint: Stress-test your assumptions

1. Simulate a university with `50000` students and an `unsigned long` tuition per student. Determine whether the total fits inside a 64-bit integer.
2. Add assertions (`#include <assert.h>`) that fail fast when totals exceed acceptable boundaries.
3. Document in comments what corrective action the programme should take when limits are hit.

## 10. Link variables to memory addresses

Understanding how variables map to memory helps you debug pointer issues later. Use the address-of operator (`&`) alongside `sizeof` to build intuition.

```c
#include <stdio.h>

int main(void)
{
    int cohort = 42;
    double retention = 0.9125;
    char initial = 'T';

    printf("cohort value: %d at %p (%zu bytes)\n", cohort, (void *)&cohort, sizeof cohort);
    printf("retention value: %.4f at %p (%zu bytes)\n", retention, (void *)&retention, sizeof retention);
    printf("initial value: %c at %p (%zu bytes)\n", initial, (void *)&initial, sizeof initial);

    return 0;
}
```

- Cast pointers to `(void *)` when printing with `%p`; it is the only portable format specifier for addresses.
- Note how neighbouring variables often appear at increasing addresses due to stack allocation. The exact addresses change per run, but relative ordering is educational.
- Capture screenshots or notes from your experiments—they become invaluable when debugging pointer arithmetic later in the course.

### Checkpoint: Trace layout changes

1. Rearrange the declarations (`double` first, then `int`, etc.) and rerun the programme. Observe how padding affects addresses.
2. Add an `int array[4]` and print the address of each element in a loop.
3. Explain why consecutive elements differ by exactly `sizeof(int)` bytes.

## 11. Capstone extension: Academic analytics dashboard

Combine the previous steps into a more ambitious console report.

1. Read the number of students, average credits completed, and tuition per credit as doubles to accommodate decimal inputs.
2. Store immutable thresholds (`MIN_COMPLETION`, `MAX_COMPLETION`, etc.) as `const double` values near the top of `main`.
3. Calculate totals using widened types (`double` or `long double`) and guard every multiplication with overflow checks.
4. Produce a formatted table with headers, aligned numeric columns, and conditional warnings when any metric drifts outside acceptable ranges.
5. Print a footer summarising memory usage by calling `sizeof` on key structures so administrators appreciate the programmes footprint.

### Stretch goals

- Extract calculations into helper functions (for example, `double completion_percent(int earned, int total)`), then call them from `main`.
- Write compile-time assertions using `_Static_assert` to ensure critical assumptions, like `sizeof(long long) >= 8`.
- Add command-line parsing (preview for later lessons) that lets users pass the input values directly; include a help message displaying the expected arguments.

## Recap and next steps

You can now:

- Pick the correct primitive type for integers, floating-point numbers, and characters.
- Initialise variables, guard constants, and prevent accidental mutation.
- Combine arithmetic operators with explicit casts to control the result type.
- Measure type sizes, control output formatting, and defend against overflow before it corrupts your results.
- Relate variables to memory addresses so you can reason about layout and alignment.

Next, tackle the companion exercise to reinforce these ideas by building a reusable progress report function, then move on to control flow so your programmes can react to real-world scenarios.
