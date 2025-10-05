# Introduction to Classes and Objects

Classes are the foundation of object-oriented programming in C++. They encapsulate data and behavior into reusable blueprints called objects. This lesson introduces class syntax, constructors, destructors, member functions, and access control, building on structs to show how classes enable encapsulation and abstraction.

## Learning goals

- Define classes with member variables and functions.
- Understand constructors and destructors for object lifecycle.
- Use access specifiers (public, private, protected) for encapsulation.
- Implement member functions and the `this` pointer.
- Create and manage objects with automatic storage.
- Differentiate classes from structs in C++.

## From structs to classes

Structs bundle data, but classes add behavior and control access.

```cpp
struct Point {
    int x, y; // public by default
};

class Rectangle {
private:
    int width, height;
public:
    Rectangle(int w, int h) : width(w), height(h) {}
    int area() { return width * height; }
};
```

### Checkpoint: Struct vs class

1. Define a struct `Circle` with public `radius`.
2. Define a class `Square` with private `side`, public constructor and `area()`.

## Class definition syntax

Classes use `class` keyword, members can be data or functions.

```cpp
class Car {
private:
    std::string model;
    int year;
public:
    Car(std::string m, int y) : model(m), year(y) {}
    void display() {
        std::cout << model << " (" << year << ")" << std::endl;
    }
};
```

- Private members hidden from outside.
- Public members form the interface.

### Checkpoint: Basic class

1. Create class `Book` with private `title`, `author`, public constructor and `print()`.

## Constructors

Initialize objects when created.

```cpp
class Person {
private:
    std::string name;
    int age;
public:
    Person() : name("Unknown"), age(0) {} // default
    Person(std::string n, int a) : name(n), age(a) {} // parameterized
};
```

- Called automatically on object creation.
- Use initializer list for efficiency.

### Checkpoint: Constructors

1. Add default and parameterized constructors to `Book`.

## Destructors

Clean up resources when objects die.

```cpp
class FileHandler {
private:
    std::ofstream file;
public:
    FileHandler(std::string path) {
        file.open(path);
    }
    ~FileHandler() {
        file.close();
    }
};
```

- Tilde prefix, no parameters.
- Automatic for stack objects.

### Checkpoint: Destructor

1. Add destructor to `FileHandler` that prints "File closed".

## Member functions

Methods that operate on object data.

```cpp
class Counter {
private:
    int count;
public:
    Counter() : count(0) {}
    void increment() { count++; }
    int getCount() { return count; }
};
```

- Access members with `this` implicitly.

### Checkpoint: Member functions

1. Add `setTitle()` and `getTitle()` to `Book`.

## The `this` pointer

Points to current object.

```cpp
class Node {
private:
    int value;
    Node* next;
public:
    Node(int v) : value(v), next(nullptr) {}
    void setNext(Node* n) { next = n; }
    Node* getNext() { return next; }
};
```

- Useful for returning `*this` or disambiguation.

### Checkpoint: this pointer

1. Use `this` in `setNext()` to return `*this` for chaining.

## Access specifiers

Control visibility: public, private, protected.

```cpp
class BankAccount {
private:
    double balance;
public:
    void deposit(double amt) { balance += amt; }
    double getBalance() { return balance; }
};
```

- Private: internal implementation.
- Public: external interface.

### Checkpoint: Access control

1. Make `balance` private in `BankAccount`, add public methods.

## Objects and instantiation

Create instances of classes.

```cpp
Car myCar("Toyota", 2020);
myCar.display();

Rectangle rect(10, 5);
std::cout << rect.area() << std::endl;
```

- Stack allocation automatic.
- Heap with `new`/`delete`.

### Checkpoint: Objects

1. Create objects of `Book` and call methods.

## Encapsulation benefits

Hide implementation, protect data.

```cpp
class Temperature {
private:
    double celsius;
public:
    Temperature(double c) : celsius(c) {}
    double getFahrenheit() { return celsius * 9/5 + 32; }
    void setCelsius(double c) { celsius = c; }
};
```

- Change internals without affecting users.

### Checkpoint: Encapsulation

1. Encapsulate data in `Temperature` class.

## Static members

Shared across all objects.

```cpp
class Student {
private:
    std::string name;
    static int count;
public:
    Student(std::string n) : name(n) { count++; }
    static int getCount() { return count; }
};
int Student::count = 0;
```

- Static data initialized outside class.

### Checkpoint: Static

1. Add static counter to `Car` class.

## Const member functions

Don't modify object.

```cpp
class Vector2D {
private:
    double x, y;
public:
    Vector2D(double a, double b) : x(a), y(b) {}
    double length() const { return sqrt(x*x + y*y); }
};
```

- `const` after parameter list.

### Checkpoint: Const

1. Make `getBalance()` const in `BankAccount`.

## Friend functions

Access private members.

```cpp
class Secret {
private:
    int data;
public:
    Secret(int d) : data(d) {}
    friend void reveal(Secret s);
};
void reveal(Secret s) { std::cout << s.data << std::endl; }
```

- Use sparingly.

### Checkpoint: Friend

1. Make a friend function for `BankAccount`.

## Operator overloading basics

Custom operators for classes.

```cpp
class Complex {
private:
    double real, imag;
public:
    Complex(double r, double i) : real(r), imag(i) {}
    Complex operator+(const Complex& other) {
        return Complex(real + other.real, imag + other.imag);
    }
};
```

- Overload `+` for addition.

### Checkpoint: Operator

1. Overload `==` for `Point` class.

## Mini project: Simple bank system

Create classes for accounts and transactions.

1. `Account` class with balance, deposit/withdraw.
2. `Transaction` class linking accounts.
3. Demonstrate encapsulation and objects.

### Success criteria

- Private data, public interface.
- Constructors initialize properly.
- Methods perform operations correctly.

## Guided practice challenges

1. Implement a `Date` class with validation.
2. Create a `Stack` class with push/pop.
3. Build a `Library` class managing books.
4. Add copy constructors to existing classes.
5. Use composition: `Car` has `Engine`.

## Self-check questions

1. Difference between struct and class?
2. When is destructor called?
3. What does `this` point to?
4. Why use private members?
5. How to declare static member?

## Recap and next steps

Classes enable OOP by bundling data and behavior. Next, explore inheritance.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Classes & Objects — Rule of Five & RAII (Appendix — classes_objects_cpp-appendix-20251005)

Practical patterns for resource management in C++ using RAII and implementing the rule of five correctly.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concept</th><th>Advice</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>RAII</td><td>Wrap resources in classes</td><td>Destructors release resources</td></tr>
    <tr><td>Rule of Five</td><td>Define move/copy as needed</td><td>Prefer `= default` where possible</td></tr>
    <tr><td>Smart pointers</td><td>Use unique_ptr/shared_ptr</td><td>Prefer unique_ptr for ownership</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: RAII file wrapper

```cpp
struct File {
  FILE* f;
  File(const char* path) { f = fopen(path, "r"); }
  ~File() { if (f) fclose(f); }
};
```

### Exercises (Appendix — classes_objects_cpp-appendix-20251005)

1. Convert a raw pointer-based API to RAII classes and add tests verifying no leaks.
2. Implement move constructors and test move vs copy behaviour.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->


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
