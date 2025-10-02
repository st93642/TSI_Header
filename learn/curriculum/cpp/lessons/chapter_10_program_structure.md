# Chapter 10: Program Files and Preprocessing Directives

## Learning goals

- Organise code across multiple translation units with proper linkage.
- Use the C++ preprocessor for conditional compilation and header inclusion guards.
- Diagnose build issues stemming from duplicate definitions or missing declarations.

## Preparation checklist

- Create `~/tsi_cpp/ch10_program_structure`.
- Review the difference between internal and external linkage in your earlier notes.
- Prepare a project skeleton with two `.cpp` files and one shared header.

## 10.1 Translation units and linkage

Construct `module_a.cpp`, `module_b.cpp`, and `main.cpp`. Declare a global variable in one file with external linkage and access it from the others using `extern`. Intentionally violate the One Definition Rule to observe linker errors, then document the steps needed to resolve them.

## 10.2 Header design and include guards

Create `metrics.h` with include guards (`#ifndef METRICS_H`). Provide function declarations and small inline utilities. Intentionally include the header twice in a source file without guards to witness the compiler diagnostics, then restore the guard and note the difference.

## 10.3 Namespaces revisited

Group related functions into a namespace `tsi::metrics`. Provide namespace aliases and nested namespaces. Experiment with unnamed namespaces for internal helpers so you can control symbol visibility within a translation unit.

## 10.4 Preprocessor conditionals

Implement `config.h` featuring macros that toggle logging. Use `#if`, `#ifdef`, `#elif`, and, optionally, `#pragma once`. Compile with `-DDEBUG=1` to enable logging and verify how the output changes with and without the macro defined.

## 10.5 Lab: build profile switcher

1. Split your analytics project into `include/` and `src/` directories.
2. Implement a macro `TSI_ENABLE_TRACING` that wraps logging calls.
3. Optionally add a simple Makefile or CMake configuration to switch build flags quickly.
4. Document how enabling or disabling the macro affects the binary size (`ls -l`) and runtime output.
5. Capture a short transcript for your offline learning journal.

## 10.6 Self-check prompts

- How does the One Definition Rule influence header design?
- When is it appropriate to use `#pragma once` instead of classic include guards?
- Which scope applies to variables declared inside an unnamed namespace?

## 10.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Duplicate symbol errors at link time | Multiple definitions of the same entity | Keep definitions in one translation unit; use headers for declarations. |
| Macro persists unexpectedly | Forgot to `#undef` or limit scope | Wrap macros carefully or replace them with inline functions. |
| Namespace pollution | Used `using namespace` in headers | Prefer qualified names or selective `using` declarations inside source files. |

## Wrap-up

- [ ] You compiled a project with multiple translation units and resolved linkage errors.
- [ ] You used include guards and confirmed their effect.
- [ ] You toggled features with preprocessor macros and documented the outcomes.

Chapter 11 transitions into class design. Keep your header discipline—it becomes essential as you define complex types.
