# Lesson 1.1: Hello World

Welcome to your first C++ lesson! In this lesson, you'll learn how to write your first C++ program.

## What You'll Learn

- How to include header files
- Understanding the main() function
- Using cout for output
- Basic C++ program structure

## C++ Program Structure

Every C++ program follows a basic structure:

```cpp
#include <iostream>
using namespace std;

int main() {
    // Your code here
    return 0;
}
```

## Key Components

### 1. Header Files

```cpp
#include <iostream>
```

- `#include` tells the preprocessor to include a file
- `<iostream>` provides input/output stream functionality

### 2. Namespace

```cpp
using namespace std;
```

- Allows us to use `cout` instead of `std::cout`
- Standard library components are in the `std` namespace

### 3. Main Function

```cpp
int main() {
    return 0;
}
```

- Entry point of every C++ program
- Must return an integer (0 means success)

### 4. Output

```cpp
cout << "Hello, World!" << endl;
```

- `cout` is used to display output
- `<<` is the insertion operator
- `endl` adds a newline and flushes the buffer

## Your First Program in C++

Let's write a simple "Hello, World!" program in C++:

```cpp
#include <iostream>
using namespace std;

int main() {
    cout << "Hello, World!" << endl;
    return 0;
}
```

## C Version of Hello World

Because many fundamentals overlap between C and C++, it's helpful to see the same program in both languages. Here's the C equivalent using `printf`:

```c
#include <stdio.h>

int main(void) {
    printf("Hello, World!\n");
    return 0;
}
```

### Key Differences

- C uses `<stdio.h>` and the `printf` function for output.
- The function signature is typically `int main(void)` in C.
- Newlines are added manually with `\n`.
- Both versions must return `0` from `main` to signal success.

## Practice Exercise

Now it's time to practice! Complete the exercise to reinforce what you've learned in both languages.

**Ready to start coding?** Click the "Start Exercise" button below and choose either the C or C++ variant to begin.
