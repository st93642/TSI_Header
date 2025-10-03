# Mastering Loops and Iteration in C

Loops transform repetitive tasks into concise, maintainable code. This lesson takes you from the basics of `while`, `for`, and `do-while` through nested loops, sentinel values, multi-dimensional traversal, and performance tuning. You will build mental models to reason about termination, invariants, and data-driven workflows that scale beyond trivial examples.

## Learning goals

- Choose the right looping construct for counting, condition-driven, and post-condition workflows.
- Establish loop invariants that keep state consistent and prevent infinite loops.
- Combine iteration with arrays, strings, and input streams to process collections safely.
- Apply `break`, `continue`, and labels intentionally while avoiding spaghetti control flow.
- Optimise loops for readability and performance, including guard clauses and early exits.

## 1. Visualising repetition

Programmers describe loops as "repeat until" patterns. Before writing code, sketch the timeline of events, input data, and termination conditions. Ask:

1. What should happen before the loop starts (initialisation)?
2. Which condition governs another pass?
3. How is the loop variable updated to guarantee termination?
4. What state remains true at the start of each iteration (the invariant)?

```c
/* Pseudocode checklist */
initialise trackers or accumulators
while (condition still holds)
{
    work with current data
    update trackers
}
```

Use comments to explain the invariant in plain language: "At the start of each iteration, `count` equals the number of processed students." This habit catches off-by-one errors before you compile.

### Checkpoint: Describe a loop aloud

Pick a scenario, such as counting submissions. Without coding, write a three-sentence summary:

- What state do you initialise?
- Which condition keeps the loop going?
- When the loop finishes, what must be true?

Share the summary with a peer or future you in a comment.

## 2. `while` loops for condition-first logic

Use `while` when you cannot guarantee the number of iterations ahead of time. The condition is evaluated before each pass, so the body might not run at all.

```c
#include <stdio.h>

int main(void)
{
    int remaining = 5;

    while (remaining > 0)
    {
        printf("Remaining tasks: %d\n", remaining);
        remaining -= 1;
    }

    printf("All tasks complete.\n");
    return 0;
}
```

Best practices:

- Update loop variables in exactly one place to prevent missed updates.
- Combine complex conditions into helper functions or boolean flags for clarity.
- Guard against negative values or unexpected input before entering the loop.

### Checkpoint: Countdown with validation

1. Ask the user for `Enter countdown start:` and validate with `scanf`.
2. Reject negative starting numbers by printing `Invalid start.` and returning `1`.
3. Use a `while` loop to print every value down to zero, then `Blastoff!`.

## 3. Sentinel-driven input loops

Sentinel loops continue until a special value appears. They shine when processing user input or streams.

```c
#include <stdio.h>

int main(void)
{
    int score = 0;
    int total = 0;
    int count = 0;

    printf("Enter scores (-1 to finish):\n");
    while (scanf("%d", &score) == 1 && score != -1)
    {
        total += score;
        count += 1;
    }

    if (count > 0)
    {
        printf("Average: %.2f\n", total / (double)count);
    }
    else
    {
        printf("No scores provided.\n");
    }

    return 0;
}
```

Tips:

- Combine the sentinel check with the `scanf` result to prevent infinite loops on invalid input.
- Document the sentinel value near the prompt so users know how to exit.
- Reset accumulators when reusing the loop logic across multiple datasets.

### Checkpoint: End-of-input statistics

1. Collect daily study minutes until the user enters `0`.
2. Track the maximum, minimum, and average using a sentinel loop.
3. Print a summary showing total days, total minutes, and computed averages.

## 4. `for` loops for counted iteration

`for` loops encapsulate setup, condition, and update. They shine when you know the iteration count or traverse arrays by index.

```c
#include <stdio.h>

int main(void)
{
    for (int week = 1; week <= 12; ++week)
    {
        printf("Week %d: Submit progress report.\n", week);
    }

    return 0;
}
```

Guidelines:

- Use `++i` or `i++` consistently; both are fine in `for` loops when you ignore the returned value.
- Favour descriptive loop variable names (`index`, `row`, `col`) instead of single letters.
- Increment by more than one when iterating over strided data (`i += 2`).

### Checkpoint: Times table generator

1. Prompt for `Enter base:` and `Enter limit:`.
2. Use a `for` loop to print multiples from `1 * base` through `limit * base`.
3. Align the output using `printf("%2d x %2d = %3d\n", row, base, row * base);`.

## 5. Iterating arrays and strings

Loops often walk arrays, calculating totals, searching, or transforming values.

```c
#include <stdio.h>

int main(void)
{
    int credits[] = {24, 18, 30, 27, 21};
    const size_t length = sizeof credits / sizeof credits[0];

    int sum = 0;
    for (size_t i = 0; i < length; ++i)
    {
        sum += credits[i];
    }

    printf("Total credits: %d\n", sum);
    return 0;
}
```

For strings, iterate until the null terminator (`'\0'`).

```c
#include <stdio.h>

int main(void)
{
    const char *name = "TSI";

    for (size_t i = 0; name[i] != '\0'; ++i)
    {
        printf("Letter %zu: %c\n", i, name[i]);
    }

    return 0;
}
```

### Checkpoint: Highest scoring course

1. Store ten course names in a 2D array or array of pointers and ten scores in a parallel array.
2. Use a loop to locate the best score and store the corresponding index.
3. Print `Top course: {name} ({score})`.

## 6. `do-while` for post-condition loops

`do-while` guarantees the body executes at least once, then checks the condition.

```c
#include <stdio.h>

int main(void)
{
    int choice = 0;

    do
    {
        printf("Menu: 1=Start, 2=Help, 3=Quit\n");
        if (scanf("%d", &choice) != 1)
        {
            return 1;
        }
    } while (choice != 3);

    printf("Goodbye!\n");
    return 0;
}
```

Use cases:

- Menus that should display at least once.
- Validation loops that re-prompt for input until the user complies.
- Simulators where the first iteration must gather fresh data before evaluating termination.

### Checkpoint: Password confirmation

1. Prompt for a password using `do-while` until the string length is at least 8 and contains a digit (use `strcspn` or manual checks).
2. Inform the user why the password was rejected.
3. Count attempts and bail out after five failures with `Too many attempts.`.

## 7. Loop invariants and correctness

A loop invariant describes a truth that holds before and after every iteration. Document it to reason about correctness.

Example: In the credits summation loop, the invariant is "`sum` equals the total of elements processed so far." At the start of iteration `i`, the invariant states that `sum` equals the sum of elements `0` through `i-1`.

Practical steps:

- Write the invariant as a comment above the loop.
- Update variables in ways that preserve the invariant.
- Test boundary conditions (first iteration, last iteration, empty input) to confirm it holds.

### Checkpoint: Trace with invariants

1. Take the sentinel loop from Section 3.
2. Add comments explaining the invariant before the loop and inside the body.
3. Run with zero inputs and multiple inputs, verifying the invariant manually.

## 8. Guarding against infinite loops

Infinite loops happen when the termination condition never becomes false. Prevent them by:

- Updating loop variables predictably.
- Adding safety counters for untrusted data sources.
- Logging the state if iterations exceed an expected threshold.

```c
#include <stdio.h>

int main(void)
{
    int iterations = 0;
    const int MAX_ITERATIONS = 1000;

    while (1)
    {
        if (++iterations > MAX_ITERATIONS)
        {
            fprintf(stderr, "Loop guard triggered at %d\n", iterations);
            break;
        }

        /* Perform work here */
    }

    return 0;
}
```

### Checkpoint: Safety guard retrofit

1. Add a guard counter to a loop in your current project.
2. Print a diagnostic when the guard triggers.
3. Test by forcing the termination condition to fail and confirm the guard exits cleanly.

## 9. Nested loops and 2D traversal

Nested loops iterate over multi-dimensional data structures. Each level should have its own descriptive variable name.

```c
#include <stdio.h>

int main(void)
{
    int grid[3][4] = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12}
    };

    for (size_t row = 0; row < 3; ++row)
    {
        for (size_t col = 0; col < 4; ++col)
        {
            printf("%3d ", grid[row][col]);
        }
        printf("\n");
    }

    return 0;
}
```

Concepts:

- Inner loops should be as small as possible; extract helper functions when bodies grow beyond a few lines.
- Document the invariant for each level; e.g., "Before the inner loop, `row` is fixed and `col` indexes columns processed so far."
- For performance, choose the loop order that matches memory layout (row-major for standard C arrays).

### Checkpoint: Seating chart renderer

1. Represent lecture hall seats as a 10x6 integer matrix where 0 = empty, 1 = reserved, 2 = occupied.
2. Use nested loops to print rows like `R1: O O X X . .` where markers map to status.
3. Track totals per row and print a legend after the chart.

## 10. `break`, `continue`, and labels responsibly

`break` exits the nearest loop immediately. `continue` skips to the next iteration. Labels enable `goto`, which should be rare.

```c
#include <stdio.h>

int main(void)
{
    int numbers[] = {3, 5, 7, 11, 13, 17};
    size_t length = sizeof numbers / sizeof numbers[0];

    const int target = 11;
    int foundIndex = -1;

    for (size_t i = 0; i < length; ++i)
    {
        if (numbers[i] != target)
        {
            continue; /* skip non-matches */
        }

        foundIndex = (int)i;
        break; /* stop searching once found */
    }

    if (foundIndex >= 0)
    {
        printf("Found at index %d\n", foundIndex);
    }

    return 0;
}
```

Guidelines:

- Prefer early returns or helper functions to complex `goto` structures.
- When using `continue`, ensure loop variables still update correctly.
- Comment why `break` is safe; note what conditions remain unprocessed.

### Checkpoint: Short-circuit search

1. Search for the first failing grade (< 4.0) in an array of GPA values.
2. Use `continue` to ignore negative sentinel values representing dropped courses.
3. Report the index of the failing grade or print `All grades passing.`

## 11. Pattern libraries: accumulators, flags, and windows

Recognise common loop patterns:

- **Accumulator**: sum values, track totals.
- **Flag**: flip a boolean when a condition occurs, then exit or adjust later logic.
- **Window**: maintain the last N elements for rolling averages.

```c
#include <stdio.h>

int main(void)
{
    int values[] = {5, 8, 13, 21, 34, 55};
    size_t length = sizeof values / sizeof values[0];

    int hasEven = 0;
    int sumLastThree = 0;

    for (size_t i = 0; i < length; ++i)
    {
        if (values[i] % 2 == 0)
        {
            hasEven = 1;
        }

        if (i >= 3)
        {
            sumLastThree += values[i] - values[i - 3];
        }
        else
        {
            sumLastThree += values[i];
        }
    }

    printf("Has even: %s\n", hasEven ? "yes" : "no");
    printf("Rolling sum (last window): %d\n", sumLastThree);
    return 0;
}
```

### Checkpoint: Rolling study average

1. Read daily minutes for 14 days.
2. Maintain a rolling 7-day sum using a sliding window loop.
3. After each new value, print the current average.

## 12. Performance and readability trade-offs

Loops can be micro-optimised, but start with clarity. When performance matters:

- Hoist invariant calculations outside the loop.
- Minimise redundant memory accesses by caching pointer references.
- Use `restrict` qualifiers (C99+) when the compiler can assume non-overlapping arrays.

```c
#include <stdio.h>

void scale(double *restrict output, const double *restrict input, double factor, size_t length)
{
    for (size_t i = 0; i < length; ++i)
    {
        output[i] = input[i] * factor;
    }
}
```

Profile before and after adjustments to ensure readability sacrifices deliver measurable wins. Use tools like `gprof`, `perf`, or `time` for objective measurements.

### Checkpoint: Hoist invariants

1. Identify a loop that recalculates a constant each iteration.
2. Move the calculation outside the loop and measure the difference (even using a stopwatch).
3. Note the results in comments or documentation.

## 13. Debugging loop behaviour

When loops misbehave:

- Print loop variables at the start and end of each iteration.
- Use the debugger to set breakpoints inside the loop and inspect state.
- Test boundary inputs: empty arrays, single-element arrays, large datasets.

```c
#include <stdio.h>

int main(void)
{
    for (int i = -3; i <= 3; ++i)
    {
        printf("DEBUG: entering iteration i=%d\n", i);
        /* complex logic here */
        printf("DEBUG: exiting iteration i=%d\n", i);
    }

    return 0;
}
```

### Checkpoint: Instrument a stubborn loop

1. Add debug prints to the sentinel loop from earlier sections.
2. Run with intentionally invalid input to observe state transitions.
3. Remove or gate the logging once the issue is resolved.

## 14. Mini project: Progress tracker dashboard

Build a console dashboard that summarises student study performance over multiple weeks.

1. Prompt for the number of weeks (1 12) and validate input.
2. For each week, read seven daily study minute entries using nested loops.
3. Compute weekly totals, averages, and identify the most productive day.
4. Track cumulative totals across all weeks and calculate the global average.
5. Print a formatted table: `Week | Total Minutes | Avg/Day | Top Day`.
6. Flag weeks where average minutes fall below a target threshold by printing `*` next to the row.
7. After the table, display the best week overall and advise if the student should adjust their plan.

### Success criteria

- Compiles with `gcc -Wall -Wextra -Werror`.
- Handles invalid input gracefully with clear error messages before exiting.
- Magic numbers (like days per week) are stored in named constants.
- Nested loops include comments describing the invariant and data being processed.

## 15. Guided practice challenges

Work through these to solidify your understanding:

1. **Histogram builder**: Read integers 0 10 and tally counts in an array. Print a bar chart using repeated `#` characters. Ensure the total count matches inputs.
2. **Course planner simulation**: Simulate course loads for four semesters. Use nested loops to add courses until credits reach 30 per term, reporting leftover capacity.
3. **Prime scanner**: Iterate numbers up to `n` and use a nested loop to check divisibility. Break early when a factor is found.
4. **Attendance heatmap**: Store 5 weeks of attendance (7 days each). Use nested loops to compute per-day averages and highlight the day with the lowest average.
5. **Adaptive study planner**: Adjust daily targets based on progress. If rolling average drops below 100 minutes, increase next day's target by 10%. Use accumulators and conditional updates.

Document assumptions, add assertions (`#include <assert.h>`) where feasible, and track invariants in comments.

## 16. Self-check questions

1. When would you choose a `while` loop over a `for` loop, even if you know the maximum number of iterations?
2. How does a loop invariant help you reason about correctness and termination?
3. Why should sentinel checks often appear directly in the loop condition?
4. What strategies prevent infinite loops caused by bad input or external systems?
5. How do nested loops interact with memory layout, and why might loop order impact performance?
6. When is `break` clearer than a boolean flag, and when should you refactor to avoid `break` entirely?
7. How can sliding-window loops be used beyond averages? Provide two scenarios.

## Recap and next steps

You now wield the core loop constructs in C and can combine them to analyse datasets, build dashboards, and enforce business rules. You have practiced designing invariants, guarding against runaway iterations, and architecting nested loops that remain readable. Continue with the accompanying exercise to implement a data-driven weekly planner, then progress to module 1.5 to explore arrays and pointers in greater depth.
