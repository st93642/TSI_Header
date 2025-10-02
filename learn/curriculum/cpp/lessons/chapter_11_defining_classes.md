# Chapter 11: Defining Your Own Data Types

## Learning goals

- Design classes with constructors, encapsulation, and member functions.
- Manage resource ownership inside classes using RAII techniques.
- Apply access control (public/private/protected) and understand the `this` pointer.

## Preparation checklist

- Create `~/tsi_cpp/ch11_defining_classes`.
- Review earlier notes on encapsulation and composition.
- Sketch UML-style diagrams of the classes you plan to build and keep them with your offline notes.

## 11.1 Class structure and members

Define `student_card.h`/`.cpp` that encapsulate name, ID, and credits. Provide getters and setters with validation, and highlight how the `this` pointer differentiates member access inside setter implementations.

## 11.2 Constructors and member initialiser lists

Implement overloaded constructors, including delegating constructors. Provide defaulted special member functions where appropriate (`= default`). Log creation and destruction events with `std::cout` so you can trace object lifetimes step by step.

## 11.3 Const correctness and overloading

Create const and non-const versions of inspectors such as `display_summary() const`. Demonstrate that const member functions may call other const members but cannot invoke mutating ones.

## 11.4 Friends and static members

Add a `friend` function `print_card` for formatted output. Introduce a static counter tracking issued cards. Observe the difference between class-level data and per-instance data in your notes.

## 11.5 Lab: lab equipment registry

1. Model an `Equipment` class with serial number, department, and maintenance schedule.
2. Implement constructors using member initialiser lists for efficiency.
3. Store optional calibration data using `std::optional<double>` to reinforce modern patterns.
4. Provide a static factory function `Equipment::from_csv` that parses offline records.
5. Write `main.cpp` to create a registry vector, iterate, and print summaries.
6. Compile with sanitizers if available to confirm no leaks occur.

## 11.6 Self-check prompts

- Why should you prefer member initialiser lists over assignment in constructors?
- When do you declare friend functions or classes?
- How do static member variables differ from namespace-level globals?

## 11.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Members left uninitialised | Forgot member initialiser list | Initialise all members explicitly. |
| Static variable duplicated in multiple files | Defined in a header without `inline` | Provide a single definition in the `.cpp` file. |
| `const` method mutates state | Member not marked `mutable` yet modified | Revisit the design; reserve `mutable` for cached data. |

## Wrap-up

- [ ] You implemented a class with encapsulation and validation.
- [ ] You used member initialiser lists and documented constructor behaviour.
- [ ] You experimented with friends or static members.

Chapter 12 extends these types with operator overloading. Keep your `Equipment` class close—you will add overloaded operators next.
