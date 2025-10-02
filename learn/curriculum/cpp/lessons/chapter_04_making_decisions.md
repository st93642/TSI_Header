# Chapter 4: Making Decisions

## Learning goals

- Identify all six relational operators and explain in plain language how each produces a boolean verdict.
- Track how the values true and false move through storage, output streams, and compound expressions.
- Build layered if and else trees, early guards, and nested decisions without sacrificing readability.
- Control flow with switch statements that document intentional fallthrough and cover unexpected categories.
- Diagnose short-circuit behaviour so you can predict which operands evaluate and which ones the runtime skips.

## Preparation checklist

- Keep working inside **~/tsi_cpp/ch4_decisions** so every artefact stays offline and organised.
- Place your Chapter 3 precedence summary beside the keyboard; you will mix arithmetic and comparisons constantly.
- Draft two real-world decision tables (for example, laboratory check-in rules or grading policies) to convert into C++.

---

## 4.1 Relational operators and boolean storage

Table 4-1 from the reference text introduces the comparison toolbox: less-than, less-than-or-equal, greater-than, greater-than-or-equal, equal-to, and not-equal-to. Each comparison delivers a value of type bool that lives in memory like any other fundamental type.

- Example declaration: **bool** permit_granted { true };
- Brace initialisation with empty braces produces false, echoing how numeric variables default to zero.
- Avoid the classic mistake of typing a single equals sign when you intend a comparison; the compiler will happily treat it as an assignment and your logic silently drifts.

**Offline drill:** Copy all six operators into **notes/relational_matrix.md**. Next to each, describe a laboratory scenario that would evaluate to true so the meaning sticks without bandwidth.

## 4.2 Reporting boolean output cleanly

When you stream a bool, C++ prints 1 or 0 unless you request text. The book emphasises the **std::boolalpha** manipulator: insert it once before diagnostic output to show the words true and false, then revert with **std::noboolalpha** if you need digits again.

Sample flow without code fences:

- **std::cout** << "Card swipe valid? " << decision << '\\n';
- **std::cout** << **std::boolalpha** << decision;

Carry this habit into the kiosk project so guard messages remain readable in your logs.

## 4.3 Comparing floating-point values responsibly

Floating-point rounding appears the moment you compare sensor readings. Rather than expecting exact equality, judge whether the difference lives inside a safe epsilon. The reference text’s guidance boils down to two patterns:

- Acceptable: **std::fabs**(measured - reference) < epsilon
- Fragile: measured == reference

Log a handful of experiments in **experiments/epsilon_trials.md**. Record the values, chosen epsilon, and whether the comparison matched your intuition.

## 4.4 Building expressive boolean expressions

Arithmetic operators still outrank comparisons in the order of operations. In the expression grade < minimum + buffer, addition runs first. Parentheses remain optional when precedence is obvious, but strategic grouping improves legibility for the reader who returns later.

The book also reminds you that numeric results convert automatically to bool. Zero becomes false; anything else becomes true. Use the implicit conversion for quick guards, and annotate your intent so teammates understand why a bare variable appears inside a condition.

## 4.5 Branching with if and braces

A simple if evaluates a condition and runs a statement or block when the test succeeds. Adopt two defensive habits stressed in Chapter 4:

- Always include braces, even around a single statement, so future edits cannot accidentally detach the guard.
- Never place a stray semicolon immediately after the condition. Doing so creates an empty statement and silently severs the body from the test.

Illustrative lines with typographic emphasis:

- **if** (badge_code == required_code) {
- ⠀⠀**std::cout** << "Access granted" << '\\n';
- }

That output line runs only when the badge matches, and the absence of a trailing semicolon protects the logic.

## 4.6 Early guards and range validation

The range-checking example from the reference text ends the program as soon as the input falls outside the valid window. Borrow the same idea for your kiosk guard:

- **if** (age < 0) {
- ⠀⠀announce_invalid_age();
- ⠀⠀**return** failure_code;
- }

By handling bad data at the top of main, the remaining branches focus on legitimate scenarios. Sketch the guard in pseudocode before you start typing so your test-first workflow stays intact.

## 4.7 Chaining and nesting decisions

Nested if statements only execute their inner checks when the outer guard succeeds. When conditions are mutually exclusive, prefer an if / else if / else chain so exactly one block fires. Before committing a decision ladder, answer three questions in **notes/ch4_trace_table.md**:

- Does each branch cover a distinct, necessary case?
- Have you provided a final else to catch unexpected input?
- Did you trace at least one representative value through every branch on paper?

## 4.8 Logical operators and short-circuiting

The reference chapter’s login experiment shows how logical operators control evaluation. With logical AND, a false left operand halts the expression; with logical OR, a true left operand does the same. Instrument your kiosk code with diagnostic prints so you can see when the second operand runs.

Recommended walkthrough:

1. Write a miniature authenticator that sets flags such as password_matches and account_locked.
2. Insert traces: **std::cout** << "Evaluated password check" << '\\n'.
3. Run scenarios where the first operand already decides the outcome, verifying that the guard prevented unnecessary work.

## 4.9 Switching on discrete categories

When a variable naturally fans out across discrete labels, a switch statement clarifies intent. Chapter 4 stresses three habits:

- Provide a default branch to document unexpected values.
- Use break to avoid accidental fallthrough between cases.
- When fallthrough is deliberate, annotate it with [[fallthrough]]; so both teammates and compilers understand.

Plain-text template with typographic cues:

- **switch** (role.front()) {
- ⠀⠀**case** 's':
- ⠀⠀⠀⠀announce_student(age);
- ⠀⠀⠀⠀**break**;
- ⠀⠀**case** 'g':
- ⠀⠀⠀⠀announce_guest();
- ⠀⠀⠀⠀[[fallthrough]];
- ⠀⠀**default**:
- ⠀⠀⠀⠀announce_unknown();
- }

Test every branch manually, including the fallback path, and log your findings in **logs/ch4_switch_runs.txt**.

## 4.10 Lab: Intake kiosk

Complete this offline project to prepare for Exercise 4: Decision Desk.

1. Create **intake_kiosk.cpp** with a banner comment describing the scenario and guard strategy.
2. Read a role as a full line, consuming leading whitespace: **std::getline(std::cin >> std::ws, role);**
3. Read an integer age on the next line.
4. Immediately output the role and age lines so every path shares the same prefix.
5. Add the negative-age guard from Section 4.6. Print the error line and exit with a non-zero status.
6. For valid ages, route the workflow with a switch on the first character of the role. Within each case, add targeted if checks when age thresholds matter.
7. Append a do / while loop that repeats the intake until the role equals quit, preparing you for Chapter 5’s expanded loop coverage.
8. Document all sample runs—including an invalid age case—in **logs/ch4_kiosk_runs.txt**.

## 4.11 Offline experiments and reflection

- Stream formatting test: Toggle **std::boolalpha** in a throwaway program and log the difference in **logs/ch4_boolalpha.txt**.
- Short-circuit worksheet: Draft a table with columns *Condition*, *Evaluated?*, and *Result* to illustrate which operands executed.
- Flowchart sketch: Draw the kiosk logic in your notebook before coding so structural issues surface early.

## 4.12 Troubleshooting guide

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Guard never triggers | Condition converts non-zero values to true unexpectedly | Compare explicitly with zero or add clarifying comments. |
| Switch executes multiple cases | Missing break or undocumented fallthrough | Add break statements and insert [[fallthrough]]; when fallthrough is deliberate. |
| Default branch never runs | Input sanitisation consumes every option | Log unexpected inputs and ensure the default branch reports them. |
| Output shows 1 or 0 instead of words | **std::boolalpha** not enabled | Activate the manipulator before printing guard outcomes. |
| Loop ignores quit | Comparison runs before trimming input | Normalise the role to lowercase and trim whitespace before the final check. |

## 4.13 Self-check prompts

- Which relational operator would you use to confirm a badge ID does not match the expected value, and why?
- How does **std::boolalpha** aid manual testing of guard clauses?
- Map out a situation where short-circuiting prevents a runtime error, then describe each operand’s role.
- Why is a default clause part of defensive programming in your kiosk switch?
- At what point should you log and exit when user input violates your constraints?

## Wrap-up checklist

- [ ] You catalogued relational operators and wrote offline scenarios for each outcome.
- [ ] You experimented with boolean output formatting and captured the results.
- [ ] You built an intake kiosk with an early guard and traced every branch manually.
- [ ] You practised short-circuit reasoning through multiple trace scenarios.
- [ ] You justified every switch branch, including the default, in your lab notes.
