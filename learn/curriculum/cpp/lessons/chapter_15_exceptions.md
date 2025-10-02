# Chapter 15: Runtime Errors and Exceptions

## Learning goals

- Identify when to throw, catch, and propagate exceptions.
- Understand exception safety guarantees (basic, strong, nothrow).
- Use standard exception types and create custom ones when needed.

## Preparation checklist

- Prepare `~/tsi_cpp/ch15_exceptions`.
- Review the hierarchy of standard exceptions and how RAII interacts with error handling.
- Plan a scenario (e.g., course enrolment validation) to retrofit with exceptions.

## 15.1 Throwing and catching basics

Write `exceptions_basic.cpp` that throws `std::runtime_error` for invalid input. Catch specific exceptions first, then a generic handler. Include logging to highlight the control flow.

## 15.2 Resource safety with exceptions

Demonstrate how RAII prevents leaks by pairing exceptions with smart pointers. Create a function that throws midway through allocation and verify that destructors run.

## 15.3 Custom exception types

Define `class EnrollmentError : public std::runtime_error` to hold extra context. Override `what()` if necessary and use `std::ostringstream` to assemble detailed messages.

## 15.4 Exception specifications and `noexcept`

Create functions marked `noexcept` and observe how the program terminates when they throw. Connect this behaviour to `std::terminate` in your notes.

## 15.5 Lab: enrolment validator

1. Implement `validate_enrolment(const Student&)` that throws typed exceptions for missing prerequisites, schedule conflicts, or capacity issues.
2. Wrap calls in `try/catch` blocks that categorise errors and produce offline logs saved to a file.
3. Add RAII wrappers (smart pointers or automatic resources) to prove clean-up still occurs.
4. Extend with a retry loop that catches exceptions, prompts for correction, and continues.

## 15.6 Self-check prompts

- What is the difference between `throw;` and `throw expression;`?
- When should a function be marked `noexcept`?
- How do you guarantee strong exception safety?

## 15.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Program terminates unexpectedly | Exception escapes a `noexcept` function | Remove `noexcept` or prevent the throw. |
| Catch block not triggered | Catching by value of the wrong type | Catch by reference and ensure the hierarchy matches. |
| Resource leak during throw | Resource not RAII-managed | Wrap allocations in smart pointers or use stack objects. |

## Wrap-up

- [ ] You threw and caught multiple exception types.
- [ ] You integrated RAII with exception control flow.
- [ ] You documented exception safety guarantees for your code.

Chapter 16 extends templates to classes—combine exception-safe practices with templated containers and utilities.
