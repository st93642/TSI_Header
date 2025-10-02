# Chapter 4: Making Decisions

## Learning goals

- Apply comparison and logical operators to model realistic decision trees.
- Build readable `if/else` chains and `switch` statements that respect fallthrough rules.
- Understand modern C++ initialiser statements for `if` and `switch`.

## Preparation checklist

- Work inside `~/tsi_cpp/ch4_decisions` so each chapter’s experiments stay isolated.
- Keep your precedence cheat-sheet from Chapter 3 nearby to reason about compound conditions.
- Draft two real-world scenarios (such as grading thresholds or character classification) that you will turn into code during this lesson.

## 4.1 Comparison operators and floating-point caveats

Explore how integers and floating-point types behave in comparisons. For floating-point values, compare within an epsilon using `std::fabs(a - b) < epsilon` instead of direct equality. Capture a few example cases in your notes and explain why direct equality often fails.

## 4.2 Building decision trees with `if/else`

Implement `grade_classifier.cpp` that categorises numeric scores into letter grades. Use braces even for single-line branches to avoid accidental fallthrough. Add diagnostic output identifying which branch executes so you can trace the flow offline.

## 4.3 Logical operators and short-circuiting

Create `login_sim.cpp` to demonstrate short-circuit evaluation. Mock stored credentials, simulate user input, and print messages indicating when the right-hand operand of `&&` or `||` was skipped. This experiment highlights how C++ avoids unnecessary work once the outcome is known.

## 4.4 `switch` statements and fallthrough guards

Write `status_router.cpp` that maps integer status codes to human-readable messages. Include a deliberate fallthrough marked with the `[[fallthrough]];` attribute and provide a `default` clause for unexpected codes. Log the path taken so you can verify behaviour during manual tests.

## 4.5 Lab: intake kiosk

1. Create `intake_kiosk.cpp`.
2. Prompt for a visitor type (`student`, `researcher`, `guest`) and an age.
3. Use an `if` initialiser (`if (auto age = read_age(); age < 0)`) to validate input before evaluating the main decision tree.
4. Route visitors with a `switch` on the role, using additional logical checks to tailor age-specific output.
5. Compile and test with a table of sample inputs recorded in your notes.
6. Add a `do/while` loop that repeats until the user enters `quit` to rehearse loop syntax introduced later.

## 4.6 Self-check prompts

- When is it appropriate to prefer `switch` over a chain of `if/else if` statements?
- How does `[[fallthrough]];` improve readability and compiler diagnostics?
- Why does declaring a variable inside an `if` initialiser keep your outer scope cleaner?

## 4.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| `switch` triggers multiple cases | Missing `break` | Add `break` statements or `[[fallthrough]];` annotations. |
| Logical condition always true | Mixed logical operators without parentheses | Add grouping to match your intent. |
| Variable unavailable after `if` | Declared in the initialiser scope | Move the declaration outside if you need it later. |

## Wrap-up

- [ ] You translated a real decision tree into C++ control flow.
- [ ] You demonstrated short-circuit behaviour with diagnostic prints.
- [ ] You implemented an input validation routine using `if` initialisers.

Chapter 5 combines arrays with looping constructs. Before progressing, make sure the `do/while` loop from the lab feels comfortable—you will reuse it when iterating over collections.
