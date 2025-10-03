# Building Hello World

Welcome to C++! This lesson walks you through writing, compiling, and running your first program without assuming any previous exposure.

## Why start with Hello World?

- Confirms your compiler, editor, and PATH are set up correctly.
- Demonstrates how source code moves through the build pipeline.
- Gives you a quick win before digging into language mechanics.

## The minimal program

```cpp
#include <iostream>

int main() {
    std::cout << "Hello, world!" << '\n';
    return 0;
}
```

### Key takeaways

- `#include <iostream>` makes the standard input/output stream library available.
- `int main()` is the entry point; returning `0` signals success.
- `std::cout` writes text to the console; `\n` adds a newline.

## Compile and run

> The exact commands vary between platforms. Feel free to adapt them to your toolchain.

- **Linux / macOS**: `g++ -std=c++17 hello.cpp -o hello`
- **Windows (MinGW or WSL)**: `g++ -std=c++17 hello.cpp -o hello.exe`
- Run the produced executable. If you see `Hello, world!`, the toolchain works.

### Troubleshooting

- *Compiler not found*: ensure `g++ --version` prints a version number.
- *Permission denied*: re-run the command with the file closed in other tools.
- *Wrong output*: confirm you saved the file and recompiled before running.

## Practice Time

Try the following before moving on:

1. Print the Transport and Telecommunication Institute greeting on two lines.
2. Return `0` explicitly and then implicitly (omit the `return`); observe that both compile.
3. Experiment with `std::endl` vs `\n`. Observe how `std::endl` flushes the stream immediately while `\n` does not.

When you are ready, open the exercise to reinforce the compilation flow.
