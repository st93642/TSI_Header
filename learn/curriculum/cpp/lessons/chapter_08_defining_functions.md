# Chapter 8: Defining Functions

## Learning goals

- Define functions with clear interfaces, default arguments, and overloading.
- Pass data by value, by reference, and by pointer intentionally.
- Manage forward declarations and headers to structure programmes offline.

## Preparation checklist

- Create `~/tsi_cpp/ch8_functions`.
- Draft a reusable header `course_utils.h` for experiments so you can separate declarations and definitions.
- Review the difference between declarations and definitions in your notes from earlier chapters.

## 8.1 Crafting function signatures

Write `signatures.cpp` with several functions returning different types. Emphasise naming conventions and parameter order that convey intent. Demonstrate prototypes placed before `main()` while defining the functions after `main()` or in separate files to mirror larger projects.

## 8.2 Passing data around

Implement `passing.cpp` with three variations of a score-adjustment function: pass-by-value, pass-by-reference, and pass-by-pointer. Log memory addresses inside each function to show when copies occur and when arguments share storage with the caller.

## 8.3 Default arguments and overloading

Create `formatting.cpp` that prints tables of course attendance. Provide overloaded `print_table` functions plus default arguments for alignment width. Experiment with different call sites and describe why the compiler chooses a particular overload.

## 8.4 Recursion essentials

Write `factorial.cpp` and `fibonacci.cpp` to rehearse recursive thinking. Add guard clauses to prevent runaway recursion and compare the runtime with iterative equivalents. Summarise your observations in your lab journal.

## 8.5 Lab: enrolment utilities

1. Split your code into `enrolment_utils.h` (declarations) and `enrolment_utils.cpp` (definitions).
2. Provide functions to calculate average marks, drop the lowest score, and format a summary string.
3. Implement overloaded `add_student` functions that accept either first/last name or a struct.
4. Include unit-style assertions in `main.cpp` to verify each overload produces the expected result.
5. Compile with multiple translation units:

   ```bash
   g++ -std=c++17 -Wall -Wextra -pedantic enrolment_utils.cpp main.cpp -o enrolment_app
   ```

6. Record the build output in your notes to reinforce the multi-file workflow.

## 8.6 Self-check prompts

- When should you prefer default arguments over function overloading?
- How does pass-by-const-reference balance safety with performance?
- Why must you declare functions before use in separate translation units?

## 8.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| “Undefined reference” linker errors | Missing function definition in the build | Add the corresponding `.cpp` file to the compile command. |
| Recursion never terminates | Missing base case | Add explicit guards at the top of the function. |
| Overload ambiguity | Call matches multiple overloads | Cast arguments or rename functions for clarity. |

## Wrap-up

- [ ] You created multiple translation units and compiled them together.
- [ ] You experimented with default arguments and overload resolution.
- [ ] You implemented recursive and iterative solutions to the same problem.

Chapter 9 introduces function templates—generalising the patterns you built here. Keep your helper functions modular so they can evolve into templates easily.
