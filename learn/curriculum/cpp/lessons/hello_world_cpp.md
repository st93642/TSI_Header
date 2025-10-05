# Building Hello World

Welcome to C++! This expanded lesson walks you step by step through writing, compiling, and running your first program. Along the way you will explore how the compilation pipeline works, why the standard library matters, and what to check when things go wrong. By the end you will have a repeatable process for turning source code into a running executable on any operating system.

## Learning goals

By completing this lesson you will be able to:

- Explain the three stages of going from source file to executable (preprocessing, compilation, linking).
- Author a minimal but standards-compliant `main` function using `<iostream>`.
- Build and run the program from the command line on Linux, macOS, or Windows/MinGW.
- Diagnose common “first compile” failures such as missing headers, typos in `main`, or PATH misconfiguration.
- Extend the basic program with additional output and simple escape sequences.

## Why start with Hello World?

The canonical “Hello World” program is more than a tradition. It verifies your tooling, demonstrates how C++ manages program startup, and gives instant feedback that your development loop works. Until you can compile and run a single source file reliably, later lessons that depend on multiple files, libraries, or build systems will be frustrating.

### The build pipeline at a glance

1. **Preprocessing** – Handles directives such as `#include <iostream>`. The preprocessor literally pastes the referenced header into your translation unit so the compiler knows about `std::cout`.
2. **Compilation** – Translates your C++ code into machine-specific object code. Each `.cpp` file compiles independently and produces an `.o`/`.obj` artifact.
3. **Linking** – Merges compiled objects and required libraries into a single executable. On failure you will see “undefined reference” errors pointing to missing symbols.

Understanding these stages helps you interpret error messages and make confident changes when projects scale up.

## The minimal program

```cpp
#include <iostream>

int main() {
        std::cout << "Hello, world!" << '\n';
        return 0;
}
```

### Key takeaways

- `#include <iostream>` gives access to the standard stream objects (`std::cout`, `std::cin`, `std::cerr`). Angle brackets indicate headers that ship with the standard library.
- Every hosted C++ program must define exactly one `int main()` (with optional parameters). The integer return value communicates success (`0`) or failure (non-zero) to the operating system.
- `std::cout` lives in the `std` namespace, so you must qualify the name or bring it into scope with `using`. Here we prefer explicit qualification to reinforce where the symbol comes from.
- The character literal `'\n'` inserts a newline without flushing the output buffer. Compare this to `std::endl`, which emits a newline **and** flushes, often making programs slower in tight loops.

### Variations worth exploring

```cpp
#include <iostream>
int main() {
        std::cout << "Hello from TSI!" << std::endl;
        std::cout << "Remember: return 0 signals success." << '\n';
}
```

- Omitting the explicit `return 0;` is legal in C++ since the compiler inserts it automatically in `main`, but adding it keeps intentions clear.
- Using `std::endl` shows the difference between line termination and flushing. Tools that capture program output (such as online judges) sometimes rely on flush behavior.

## Compile and run

> Adapt the commands to match your compiler path or build system. If you use an IDE, locate the equivalent build/run actions.

- **Linux / macOS:**

    ```sh
    g++ -std=c++17 hello.cpp -o hello
    ./hello
    ```

- **Windows (MinGW or WSL):**

    ```cmd
    g++ -std=c++17 hello.cpp -o hello.exe
    hello.exe
    ```

- **MSVC (Developer Command Prompt):**

    ```cmd
    cl /EHsc /std:c++17 hello.cpp
    hello.exe
    ```

Watch the terminal output each time you compile. For beginners, the difference between compiler diagnostics and program output can be subtle. Diagnostics use `stderr`; your program prints on `stdout` unless you flush to `stderr` explicitly.

### Understanding compiler flags

- `-std=c++17` selects the language standard. Later lessons rely on C++17 features like structured bindings and `std::optional`.
- `-Wall -Wextra -Werror` (optional but recommended) promote common mistakes to warnings or errors.
- `-g` embeds debug symbols, enabling IDEs and command-line debuggers to step through code.

## Troubleshooting the first build

| Symptom | Likely cause | How to fix |
| --- | --- | --- |
| `g++: command not found` | Compiler not installed or PATH misconfigured | Install a compiler (`sudo apt install build-essential`, Xcode Command Line Tools, or MinGW) and reopen your shell. |
| `hello.cpp: No such file or directory` | Terminal path differs from file location | Run `pwd`/`cd` into the directory containing `hello.cpp`. |
| `undefined reference to WinMain` | Using MSVC without `main` defined properly | Ensure the file uses `int main()` rather than `void main()` and that the function is not nested in a namespace. |
| Program prints nothing | Forgot to run the executable or output buffered | Confirm you executed `./hello` and do not exit immediately without printing. |

Keep notes on the fixes you apply—later modules build on the same toolchain.

## Deeper look: how the runtime starts

When your program launches, the runtime initializes global objects, sets up I/O, then calls `main`. Anything you do before `main` (global constructors, static initializers) must be deliberate because failures there are harder to debug. Returning from `main` (or calling `std::exit`) tears down the runtime and reports status to the OS.

## Practice time

1. **Institute greeting:** Print a two-line welcome where the first line uses `std::cout <<` and the second uses `std::printf`. Observe the different headers required.
2. **Unicode check:** Output a string containing UTF-8 characters (e.g., `"Привет, мир!"`). Verify your terminal encoding displays it correctly.
3. **Command-line flags:** Rebuild with `-Wall -Wextra` and intentionally remove the `return 0;`. Notice the compiler no longer warns because the standard guarantees an implicit return in `main`.
4. **Stream flush experiment:** Print without a newline, run the program, then terminate it with `Ctrl+C`. Observe that buffered text may not appear—another reason to flush when needed.

Document your results; you will reference them when you add richer console interactions in the next lesson.

## Self-check quiz

1. Which header provides `std::cout` and why must you include it?
2. What does the linker do, and which error type indicates a linking failure?
3. How does `std::endl` differ from `'\n'` in terms of performance?
4. Where does the operating system read the exit code from, and what value signals success?
5. What happens if you declare `int main(int argc, char** argv)` but compile without using the arguments?

Spend a few minutes answering these in your own words before moving forward.

## Next steps

With your toolchain confirmed, proceed to the next lesson (`iostream_basics`) to gather user input and print formatted data. Keep the `hello.cpp` file handy—it becomes the template for future console programs.

<!-- markdownlint-disable MD033 MD010 -->

## Hello World — Practical Appendix: CI, Tooling, and Debugging

This appendix extends the Hello World lesson with practical, copy-pasteable snippets you can use to build robust, portable examples: a simple CMake project, CI build matrix, cross-compilation Docker tips, sanitizers and runtime checks, static analysis, and debugging recipes.

### Simple CMake project (copyable)

Create a minimal project layout:

```text
hello-cmake/
  ├─ CMakeLists.txt
  ├─ src/hello.cpp
  └─ include/hello.h
```

`CMakeLists.txt` (minimal):

```cmake
cmake_minimum_required(VERSION 3.15)
project(hello CXX)
set(CMAKE_CXX_STANDARD 17)
add_executable(hello src/hello.cpp)
install(TARGETS hello RUNTIME DESTINATION bin)
```

This lets you build locally with:

```bash
mkdir build && cd build
cmake ..
cmake --build . --config Release
./hello
```

### CI Build Matrix (example for GitHub Actions)

Use a small build matrix to validate on Linux, macOS, and Windows. This example also demonstrates sanitizer and release builds.

```yaml
name: ci
on: [push, pull_request]

jobs:
  build:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        compiler: [gcc, clang, msvc]
        build_type: [Release, Debug]
    steps:
      - uses: actions/checkout@v4
      - name: Setup
        run: |
          if [ "${{ matrix.os }}" == "ubuntu-latest" ]; then sudo apt-get update && sudo apt-get install -y build-essential cmake; fi
      - name: Configure
        run: |
          mkdir build && cd build
          cmake -DCMAKE_BUILD_TYPE=${{ matrix.build_type }} ..
      - name: Build
        run: cmake --build build --config ${{ matrix.build_type }}
      - name: Run
        run: ./build/hello
```

### Build matrix (HTML) — target coverage

<table>
  <thead>
    <tr><th>Platform</th><th>Compiler</th><th>Checks</th></tr>
  </thead>
  <tbody>
    <tr><td>ubuntu-latest</td><td>gcc/clang</td><td>Release, Debug, ASan/UBSan</td></tr>
    <tr><td>macos-latest</td><td>clang</td><td>Release, Debug</td></tr>
    <tr><td>windows-latest</td><td>MSVC</td><td>Release, Debug</td></tr>
  </tbody>
</table>

<!-- markdownlint-enable MD033 MD010 -->

### Sanitizers and runtime checks

Sanitizers find common runtime bugs early. For Hello World these are simple to enable and instructive to run.

- AddressSanitizer (ASan) — finds heap/stack OOB and use-after-free.
- UndefinedBehaviorSanitizer (UBSan) — finds undefined behavior (e.g., signed integer overflow).
- LeakSanitizer (LSan) — detects leaks (often part of ASan on Linux).

Compile with sanitizers (gcc/clang):

```bash
g++ -std=c++17 -fsanitize=address,undefined -fno-omit-frame-pointer -g hello.cpp -o hello.san
./hello.san
```

In CI, run sanitizer jobs only on Debug builds or dedicated sanitizer matrix entries to contain runtime costs.

### Static analysis & linters

- Use `clang-tidy` for code-quality suggestions and modernizations.
- Use `cppcheck` for additional static checks.

Example `clang-tidy` run (CMake integration):

```bash
mkdir build && cd build
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
clang-tidy ../src/hello.cpp -- -I../include
```

Fail the CI job if `clang-tidy` reports errors above an agreed threshold.

### Cross-compilation & Docker quickstart

Use lightweight Docker images to provide consistent toolchains for students or CI:

```dockerfile
FROM ubuntu:24.04
RUN apt-get update && apt-get install -y build-essential cmake git
WORKDIR /work
COPY . /work
RUN mkdir build && cd build && cmake .. && cmake --build .
```

For cross-compiling to Windows, consider using `osxcross` (macOS cross) or `mingw-w64` toolchains inside containers.

### Minimal unit test harness (Catch2)

Add a small test to exercise your `add` function. Use a single-header Catch2 distribution for simplicity.

`tests/test_add.cpp`:

```cpp
#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "math_utils.h"

TEST_CASE("add works") {
  REQUIRE(add(1,2) == 3);
  REQUIRE(add(-1,1) == 0);
}
```

CMake snippet to add tests:

```cmake
add_subdirectory(third_party/catch2)
add_executable(tests tests/test_add.cpp)
target_link_libraries(tests PRIVATE Catch2::Catch2WithMain)
add_test(NAME unit-tests COMMAND tests)
```

Run tests locally:

```bash
ctest --output-on-failure
```

### Debugging quick recipes

- gdb (Linux):

```bash
g++ -g hello.cpp -o hello.debug
gdb ./hello.debug
# inside gdb: run, backtrace, print <var>
```

- lldb (macOS):

```bash
lldb ./hello.debug
(lldb) run
```

- Visual Studio: open `Developer Command Prompt`, compile with `cl`, or import the CMake project into Visual Studio and set breakpoints.

### Common first-linker errors and fixes

- `undefined reference to` — missing object or library during linking. Ensure you list the library after the object files in the linker command (e.g., `g++ main.o -lmylib -o main`).
- `duplicate symbol` or `multiple definition` — ensure functions are declared `inline` if defined in headers, or move definitions to `.cpp` files.
- `cannot find -l<name>` — missing library on system; install dev packages or point the linker to the custom location via `-L`.

### Packaging and distribution notes

- For small exercises, share source and a `CMakeLists.txt` to let students reproduce builds.
- For artifact distribution, build stable release artifacts in CI and publish them to a temporary storage bucket for exercises.

### Quick checklist for instructors (copyable)

- Ensure the exercise repository contains a `README.md` with build steps for Linux/macOS/Windows.
- Provide Docker images or CI jobs for reproducible builds.
- Add `clang-tidy`/`iwyu` checks as optional CI gates for advanced students.

### Exercises: Hello World Extended (unique)

1. Create a small CMake project that builds `hello` and a unit test using Catch2. Add a GitHub Actions workflow that runs the test on push.
2. Rebuild `hello` with ASan and instrument a small intentional bug (e.g., use-after-free) to observe the sanitizer report.
3. Create a Dockerfile that builds the project and runs the unit tests; publish the image to a container registry (optional).

---

End of Hello World practical appendix.


<!-- Practical Appendix: Reference and further reading -->

### Practical Appendix
This appendix contains brief practical notes and quick references to complement the lesson content. It is intentionally short and safe: no code execution or large data dumps.

- Reference: Official documentation and language core references are excellent further reading sources. Follow the standard docs for authoritative examples.
- Quick tips:
  - Re-run the examples in a REPL to experiment with small changes.
  - Use small, focused test cases when validating behavior.
  - Prefer idiomatic standard-library helpers for clarity and maintainability.

Further reading and sources:
- Official language documentation (search for "official <LANG> docs" where <LANG> is the lesson's language).
- Standard library reference and API pages.
- For curriculum authors: keep examples minimal and include runnable snippets in fenced code blocks.

*End of Practical Appendix.*
