# Lesson 1.2: Variables and Data Types

In this lesson, you'll learn about variables and the fundamental data types in C++.

## What You'll Learn

- How to declare variables
- Basic data types (int, double, char, bool, string)
- Variable initialization
- Naming conventions

## Variables in C++

A variable is a named storage location that holds a value. In C++, you must declare a variable before using it.

### Basic Syntax

```cpp
dataType variableName = value;
```

## Fundamental Data Types

### 1. Integer Types

```cpp
int age = 25;           // Whole numbers
short smallNum = 100;   // Smaller range
long bigNum = 1000000;  // Larger range
```

### 2. Floating Point Types

```cpp
float price = 19.99f;   // Single precision
double distance = 123.456;  // Double precision (more accurate)
```

### 3. Character Type

```cpp
char grade = 'A';       // Single character
char letter = 'Z';
```

### 4. Boolean Type

```cpp
bool isStudent = true;  // true or false
bool hasLicense = false;
```

### 5. String Type

```cpp
#include <string>       // Required for string
string name = "Alice";
string course = "Computer Science";
```

## Variable Declaration and Initialization

### Declaration Only

```cpp
int count;              // Declared but not initialized
double temperature;     // Contains garbage value
```

### Declaration with Initialization

```cpp
int count = 0;          // Declared and initialized
double temperature = 98.6;
```

### Multiple Declarations

```cpp
int x, y, z;            // Declare multiple variables
int a = 1, b = 2, c = 3; // Declare and initialize multiple
```

## Naming Conventions

### Valid Variable Names

- Must start with letter or underscore
- Can contain letters, numbers, underscores
- Case sensitive

```cpp
int studentAge;         // camelCase (recommended)
int student_age;        // snake_case
int _private;           // Starting with underscore
```

### Invalid Variable Names

```cpp
int 2age;              // Cannot start with number
int student-age;       // Cannot contain hyphens
int class;             // Cannot use C++ keywords
```

## Example Program in C++

```cpp
#include <iostream>
#include <string>
using namespace std;

int main() {
    // Declare and initialize variables
    int age = 20;
    double height = 5.8;
    char grade = 'A';
    bool isStudent = true;
    string name = "John";
    
    // Display values
    cout << "Name: " << name << endl;
    cout << "Age: " << age << endl;
    cout << "Height: " << height << endl;
    cout << "Grade: " << grade << endl;
    cout << "Is student: " << (isStudent ? "true" : "false") << endl;
    
    return 0;
}
```

## Example Program in C

```c
#include <stdio.h>
#include <stdbool.h>

int main(void) {
    int age = 20;
    double height = 5.8;
    char grade = 'A';
    bool isStudent = true;

    printf("Age: %d\n", age);
    printf("Height: %.1f\n", height);
    printf("Grade: %c\n", grade);
    printf("Is student: %s\n", isStudent ? "true" : "false");

    return 0;
}
```

### What Changes Between C and C++?

- C requires `#include <stdbool.h>` to use the `bool` type.
- C++ offers `std::string` for text, while C uses character arrays or pointers.
- Printing booleans directly yields `1`/`0` in C; convert them to text for clarity.
- Both languages support the core numeric and character types shown above.

## Practice Exercise

Ready to practice with variables and data types? Complete the exercise to create programs that use different variable types in both C and C++!
