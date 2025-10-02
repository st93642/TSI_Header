# Chapter 1: Basic Ideas

## Learning goals

- Understand how a C++ translation unit is organised and why headers matter.
- Identify the stages of translation: preprocessing, compilation, and linking.
- Build a minimal program with warnings enabled and record findings offline.

## Prerequisites

- A C++17-capable compiler such as `g++` (GCC 11+) or `clang++`.
- A terminal environment where you can run shell commands.
- A working directory, for example `~/tsi_cpp/ch1_basic_ideas`, reserved for this lesson.

## 1.1 From source file to executable

A C++ program starts life as plain text. The compiler translates that text through three main stages:

1. **Preprocessing** handles directives beginning with `#` by copying included headers into your source and expanding macros.
2. **Compilation** turns each translation unit (`.cpp` plus headers) into an object file containing machine code.
3. **Linking** combines all object files and required libraries into a runnable executable.

Keeping translation units small—one `.cpp` per major component with the necessary headers—makes the process faster and easier to reason about when you are offline.

> **Notebook task**: Jot down where in your file tree each stage reads and writes files. This habit pays off when diagnosing build issues without internet access.

## 1.2 Structure of a minimal program

Every C++ translation unit follows the same broad pattern:

- Include the headers that define the library facilities you use.
- Declare or define functions, classes, and global objects.
- Provide exactly one `main()` function, which acts as the entry point.

Whitespace does not change semantics, but a consistent style improves readability. Adopt a layout such as four-space indentation and braces on their own line, then stick with it for the remainder of the curriculum.

```cpp
// main.cpp
#include <iostream>

int main() {
   std::cout << "Hello from Riga!" << std::endl;
   return 0; // explicit success status
}
```

Compile the program with explicit standard and warnings to catch mistakes early:

```bash
g++ -std=c++17 -Wall -Wextra -pedantic main.cpp -o hello_basic
./hello_basic
```

If you see the greeting in your terminal, the toolchain is configured correctly.

## 1.3 Namespaces and qualified identifiers

Large code bases often define functions or classes with the same name. Namespaces prevent clashes by grouping identifiers. The Standard Library lives inside the `std` namespace, so you access `cout` as `std::cout`. Avoid the blanket directive `using namespace std;` in header files; it can pollute the global namespace for every file that includes yours. Instead, qualify names explicitly or introduce small, targeted `using` declarations inside functions.

> **Offline reminder**: Record unfamiliar identifiers in a glossary (for example “namespace” or “translation unit”) so you can review them later without a search engine.

## 1.4 Hands-on checklist

1. Create `main.cpp` using the example above.
2. Build the program with the command shown in §1.2. Inspect the warnings list—there should be none.
3. Modify the output message, recompile, and re-run to prove the feedback loop is working.
4. Run `ls -l hello_basic` (Linux/macOS) or `dir hello_basic.exe` (Windows) to confirm the executable bit or file extension.
5. Note the full compile command in your lab journal for reuse in later chapters.

## 1.5 Self-check

Answer these prompts in `chapter_01_reflection.md`:

- What does the preprocessor contribute that raw compilation cannot?
- Why must `#include <iostream>` appear before you call `std::cout`?
- How would you explain the distinction between a source file and a translation unit to a teammate?

## 1.6 Troubleshooting table

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| `g++: command not found` | Compiler not installed or PATH not updated | Install your distribution’s build tools (`sudo apt install build-essential`, `xcode-select --install`, etc.). |
| `undefined reference to main` | `main()` missing from the file being linked | Ensure one translation unit defines `int main()`. |
| Output shows garbled characters | Terminal locale mismatch | Export an appropriate UTF-8 locale before running the program. |

## Wrap-up

Tick off these milestones before moving forward:

- [ ] Built and executed `hello_basic` locally.
- [ ] Captured notes on the translation pipeline and namespace usage.
- [ ] Logged the compile command for future reuse.

When you are comfortable with these basics, continue to Chapter 2 to explore C++ fundamental types and literal forms.
