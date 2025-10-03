# Making Decisions with Conditionals

As your C programmes grow, they must react to different inputs and scenarios. This lesson shows how to branch execution with `if`, `else if`, `else`, and `switch`, how to combine comparisons, and how to avoid common pitfalls such as uninitialised flags or missing `break` statements. Along the way you will practice writing readable conditionals that keep business rules obvious.

## 1. Start with simple `if` statements

`if` evaluates an expression and enters the block only when the expression is non-zero. Any non-zero value counts as "true"; zero is "false".

```c
#include <stdio.h>

int main(void)
{
    int creditsEarned = 45;
    int isEligible = creditsEarned >= 42; /* relational operator */

    if (isEligible)
    {
        printf("Eligible for internship applications.\n");
    }

    if (creditsEarned % 2)
    {
        printf("Odd number of credits.\n");
    }

    return 0;
}
```

- Always initialise the variables you plan to test before the conditional.
- Comparisons (`>=`, `==`, `!=`) produce `0` or `1`; store them in an `int` flag if you intend to reuse the result.
- Use braces even for single-line bodies; this prevents bugs when you add statements later.

### Checkpoint: First branch

1. Change `creditsEarned` to `38` and observe that the internship message disappears.
2. Add a second flag for `creditsEarned >= 120` and print `"Eligible for thesis registration."` when true.
3. Explain in a comment why `creditsEarned % 2` works as a condition.

## 2. Chain decisions with `if` / `else if` / `else`

Chaining keeps mutually exclusive branches readable. Order matters: the first condition that evaluates to non-zero wins.

```c
#include <stdio.h>

int main(void)
{
    int score = 78;

    if (score >= 90)
    {
        printf("Grade: A\n");
    }
    else if (score >= 80)
    {
        printf("Grade: B\n");
    }
    else if (score >= 70)
    {
        printf("Grade: C\n");
    }
    else if (score >= 60)
    {
        printf("Grade: D\n");
    }
    else
    {
        printf("Grade: F\n");
    }

    return 0;
}
```

- Test the highest thresholds first so each subsequent branch only handles the remaining cases.
- Keep comparisons consistent (`>=` in every branch) to avoid gaps.
- Provide an `else` fallback so unexpected inputs still produce a sensible message.

### Checkpoint: Validate the rubric

1. Run the programme with scores `95`, `82`, `74`, and `58`; verify the printed grades.
2. Add a comment showing how to turn the ladder into a reusable `grade_for(int score)` function.
3. Extend the ladder with an "A+" band (`score >= 97`) to confirm you can insert new branches without breaking the others.

## 3. Combine tests with logical operators

Use `&&` (logical AND) to require multiple conditions, and `||` (logical OR) when any condition should pass. Parentheses clarify intent.

```c
#include <stdio.h>

int main(void)
{
    int credits = 90;
    double average = 3.4;
    int isFullTime = 1;

    if ((credits >= 60 && average >= 3.2) || (credits >= 90 && average >= 3.0))
    {
        printf("Qualifies for honours track.\n");
    }

    if (!isFullTime)
    {
        printf("Student must enroll full time before applying.\n");
    }

    return 0;
}
```

- Wrap complex compound expressions in parentheses so the precedence is obvious to anyone reading the code.
- `!` negates a boolean expression; the result is `1` when the operand is zero.
- Store intermediate results in named variables when the logic becomes difficult to read inline.

### Checkpoint: Check eligibility rules

1. Modify the honours condition so part-time students (`isFullTime == 0`) are rejected even if the academic thresholds are met.
2. Add a second rule for scholarship renewal that requires `average >= 3.5` _and_ at least `30` credits in the current year.
3. Print separate messages that explain why an applicant was denied.

## 4. Select cases with `switch`

`switch` is ideal for exact matches against integers or characters. Each `case` must end with `break` to prevent fall-through.

```c
#include <stdio.h>

int main(void)
{
    char command = 'p';

    switch (command)
    {
        case 'n':
            printf("Create a new schedule.\n");
            break;
        case 'p':
            printf("Print current schedule.\n");
            break;
        case 'q':
            printf("Quit the application.\n");
            break;
        default:
            printf("Unknown command.\n");
            break;
    }

    return 0;
}
```

- `switch` compares the operand (`command`) to each `case` label in order; it executes the matching block until a `break` or the end of the statement.
- Include a `default` case for unsupported values.
- Group multiple labels to handle aliases by stacking `case` statements before a shared block.

### Checkpoint: Expand the menu

1. Add commands for `a` (add a course) and `d` (drop a course).
2. Group uppercase versions (`'P'`, `'Q'`, etc.) with the same logic; remember to include `break` only after the shared block.
3. Test the programme with each command to ensure no fall-through occurs.

## 5. Guard against invalid input early

Combine conditionals with input validation so your code fails fast when data is outside acceptable ranges.

```c
#include <stdio.h>

int main(void)
{
    int studyHours = 0;

    printf("Enter weekly study hours (0-80): ");
    if (scanf("%d", &studyHours) != 1)
    {
        printf("Invalid input.\n");
        return 1;
    }

    if (studyHours < 0 || studyHours > 80)
    {
        printf("Study hours must be between 0 and 80.\n");
        return 1;
    }

    printf("Recorded: %d hours.\n", studyHours);
    return 0;
}
```

- Check the return value of `scanf` to catch non-numeric input.
- Validate ranges immediately and exit with a non-zero status when constraints are violated.
- Keep the happy path at the bottom of the function so the main logic stays visible.

### Checkpoint: Harden user prompts

1. Extend the validation to reject study hours over `60` with a specific message.
2. Convert the guard into a helper function named `is_valid_hours(int)` that returns `1` for valid input.
3. Add a final confirmation message only when both the input and the range checks succeed.

## 6. Mini project: Academic standing advisor

Create a console advisor that classifies a students standing and recommends next steps.

1. Prompt for GPA (double), earned credits (int), and disciplinary incidents (int). Validate each input.
2. Use a chained conditional ladder to print one of four standings: `"Excellent"`, `"Good"`, `"Probation"`, or `"Suspended"` based on GPA and incidents. Ensure the ranges do not overlap.
3. Add targeted advice: for example, suggest honours seminars for "Excellent" standing or mandatory counselling for "Suspended" students.
4. Include a `switch` that maps a short command to follow-up actions (`'p'` for print transcript, `'a'` for appeal request, `'q'` to quit). Accept both uppercase and lowercase letters.

### Success criteria

- The programme compiles with `gcc -Wall -Wextra -Werror` and produces no warnings.
- All validation paths print clear, actionable messages before exiting.
- Each standing triggers exactly one advice message; commands that are not recognised fall back to `"Unknown action."`

## Recap and next steps

You now know how to:

- Express branching logic using `if`, `else if`, `else`, and combine comparisons with logical operators.
- Choose `switch` for exact matches and prevent accidental fall-through with consistent `break` statements.
- Harden your programmes with input validation that fails fast and guides the user.

Continue by implementing the companion exercise, which applies these skills to scheduling student consultations. The following module will introduce loops so your programmes can repeat work efficiently.
