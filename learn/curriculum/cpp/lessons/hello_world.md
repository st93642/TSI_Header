# Lesson 1.1: Hello World

Welcome to your first C and C++ program! "Hello World" teaches the complete lifecycle of writing source code, compiling it, running it, and seeing output on screen. By the end of this lesson you will understand every line that appears in the classic example and know how to diagnose the most common first-week mistakes.

## Learning Goals

- Describe the minimal structure of a valid C++ program.
- Explain the purpose of headers, namespaces, and the `main()` entry point.
- Produce console output with `std::cout` and with C's `printf`.
- Compile and run programs from the command line using `g++` (C++) and `gcc` (C).
- Recognize and fix typical beginner errors (missing semicolons, typos in headers, incorrect capitalization).

## 1. Anatomy of the Smallest C++ Program

Every C++ translation unit follows a predictable structure:

```cpp
#include <iostream>          // 1. bring input/output utilities into scope

int main() {                 // 2. define the program entry point
    std::cout << "Hello, World!" << std::endl; // 3. perform work
    return 0;                // 4. signal success to the operating system
}
```

### Breakdown

1. **Preprocessor directive** `#include <iostream>` copies the declarations for the standard stream library into your file so that the compiler knows about `std::cout` and `std::endl`.
2. **Function signature** `int main()` defines the starting point for native applications. Returning zero indicates the program completed successfully. Any non-zero value signals an error code.
3. **Body** between `{}` contains statements to execute. Statements end with `;`.
4. **Output statement** uses the insertion operator `<<` to send data to the console. `std::endl` prints a newline and flushes the buffer so text appears immediately.

You may see some code samples use `using namespace std;`. For small snippets this reduces typing, but in production code it is safer to write `std::cout` explicitly so symbols from other libraries do not collide. Throughout the curriculum we favor fully qualified names.

## 2. Building and Running the Program

Compilers translate your `.cpp` file into an executable. The workflow looks like this:

```mermaid
flowchart LR
    A[hello.cpp] -- compile --> B{Compiler (g++)}
    B -- success --> C[a.out / hello]
    C -- run --> D[(Console Output)]
```

### Command-Line Steps (Linux/macOS)

```bash
g++ -std=c++17 -Wall -Wextra -pedantic hello.cpp -o hello
./hello
```

- `-std=c++17` selects the language standard used throughout this course.
- `-Wall -Wextra -pedantic` enable helpful warnings that catch mistakes early.
- `-o hello` names the executable; otherwise `a.out` is produced by default.

### Visual Studio Code Run Shortcut

If you use VS Code with the C/C++ extension, you can click the ▶️ icon in the editor to compile and run using the configured build task. Regardless of tooling, the compiler still performs the same steps shown above.

## 3. Understanding Each Component

| Component | Purpose | Common Mistakes |
|-----------|---------|-----------------|
| `#include <iostream>` | Provides stream I/O facilities (`std::cout`, `std::cin`). | Typing `<iostream.h>` (obsolete header) or forgetting the angle brackets. |
| `int main()` | Entry point; operating system calls this function first. | Misspelling `main`, omitting the return type, or adding parameters before you know what they do. |
| `std::cout << ...` | Sends text to standard output using the insertion operator. | Forgetting `std::`, missing `<<`, or omitting string quotes. |
| `return 0;` | Ends the program and reports status. | Forgetting the semicolon or returning a string/other type instead of an integer. |

### Alternatives to `std::endl`

`std::endl` forces a flush, which can slow down tight loops. The idiomatic way to end a line without flushing is to append `"\n"` to your output:

```cpp
std::cout << "Hello, World!\n";
```

You will see both styles in the wild. When performance matters, prefer `"\n"` and call `std::cout.flush()` only when necessary.

## 4. The Same Program in C

Studying the C version reinforces which features are shared and which are C++-specific.

```c
#include <stdio.h>

int main(void) {
    printf("Hello, World!\n");
    return 0;
}
```

- `<stdio.h>` contains declarations for `printf`, the classic C output function.
- `printf` uses format strings—in this case `%s`, `%d`, etc.—but plain text works too.
- The `\n` sequence inserts a newline *without* flushing automatically.
- The parameter list is often written as `void` in C to indicate no parameters. In C++ you simply use empty parentheses.

When you compile the C version, switch to the C compiler:

```bash
gcc -std=c17 -Wall hello.c -o hello_c
./hello_c
```

## 5. Troubleshooting Checklist

- **Compiler cannot find `<iostream>`** → Make sure you saved the file with a `.cpp` extension and are using a C++ compiler (`g++`, `clang++`).
- **`undefined reference to WinMain`** (on Windows) → Ensure the file is compiled as a console application (`g++ hello.cpp -o hello.exe`).
- **No output appears** → Confirm the executable actually ran and did not exit early, and that you are writing to `std::cout` instead of `std::cerr` when redirecting output.
- **Strange symbols in terminal** → Set your file encoding to UTF-8; avoid smart quotes when copying code from formatted documents.

## 6. Try It Yourself

1. Modify the string to print your own greeting.
2. Add a second line that prints the current year. Remember to include `"\n"` or another `std::endl`.
3. Challenge: prompt the user for their name with `std::cin` and then print `Hello, <name>!`.

## 7. Practice Exercise

Launch the exercise for Lesson 1.1 to reinforce the concepts in both languages. The automated tests expect:

- The correct headers and entry point signature.
- Output that matches the blueprint exactly, including punctuation and spacing.
- A return value of `0` from `main`.

Once you pass the tests, review the output and warnings to build confidence running programs from the command line.
