# Lesson 5.1: Classes and Objects

Object-oriented programming (OOP) in C++ starts with classes—user-defined types that bundle data and the operations that act on that data. This lesson serves as a complete offline tutorial: it explains every concept from first principles, demonstrates idiomatic C++17 code, contrasts related patterns in C, and provides build/run instructions you can follow without an internet connection.

## Learning Goals

- Describe the relationship between classes, objects, and member functions.
- Declare classes with appropriate access specifiers and understand default visibility.
- Distinguish between `struct` and `class`, and know when to choose each.
- Separate interface (declarations) from implementation to keep projects maintainable.
- Build, run, and troubleshoot simple class-based programs from the command line.

## 1. Why Classes?

A class defines a **blueprint** for objects. Instead of scattering variables and functions across a program, you collect related state and behavior in one place, enforcing invariants and improving readability.

```cpp
class StudentCard {
public:
    void setName(const std::string& value) { name_ = value; }
    void setId(int value) { id_ = value; }
    std::string summary() const {
        return name_ + " (" + std::to_string(id_) + ")";
    }

private:
    std::string name_;
    int id_{};
};
```

- **Public interface** (`setName`, `setId`, `summary`) controls how other code interacts with the object.
- **Private data** (`name_`, `id_`) hides implementation details and prevents accidental misuse from outside the class.

Creating an object uses the blueprint:

```cpp
StudentCard card;
card.setName("Antra Ozola");
card.setId(176245);
std::cout << card.summary() << '\n';
```

## 2. Class vs. Struct

In C++, `struct` and `class` differ only in default visibility:

| Keyword | Default member access | Default inheritance | Typical use |
|---------|-----------------------|---------------------|-------------|
| `struct` | `public` | `public` | Passive data aggregates (POD types, simple records) |
| `class` | `private` | `private` | Encapsulated types with invariants |

Prefer `class` when you intend to enforce invariants, and `struct` when the type merely groups data (similar to a C `struct`). You can still declare private members in a `struct` explicitly if needed.

## 3. Anatomy of a Class Definition

```cpp
class Course {
public:
    // 1. Constructors and member functions
    void setTitle(const std::string& value);
    std::string title() const { return title_; }

private:
    // 2. Data members
    std::string title_;
    int credits_{}; // zero-initialized
};
```

1. **Declarations** in the class body describe what operations exist. Definitions can appear inside or outside the class.
2. **Member variables** (often suffixed with `_`) keep implementation details private.

### Access Specifiers Refresher

- `public`: visible to any translation unit that includes the class definition.
- `protected`: visible to the class and derived classes (covered in Lesson 5.3).
- `private`: visible only inside the class definition (and friends, if declared).

## 4. Implementing Members Outside the Class

To keep class definitions concise, define member functions outside using the `ClassName::` qualifier.

```cpp
class Course {
public:
    void setCredits(int value);
    int credits() const;

private:
    int credits_{};
};

void Course::setCredits(int value) {
    credits_ = value;
}

int Course::credits() const {
    return credits_;
}
```

Separating declaration and definition makes header (`.hpp`) files smaller and avoids unnecessary recompilation for downstream code.

## 5. Header/Implementation Split

Larger projects keep class declarations in header files and definitions in source files:

```text
include/
  course.hpp     // declarations + inline helpers
src/
  course.cpp     // definitions
main.cpp
```

- `course.hpp` contains the `class` definition and short inline functions.
- `course.cpp` includes the header and implements longer functions.
- `main.cpp` includes `course.hpp` and uses the type.

This separation prevents duplicate symbol definitions and reduces build times—especially important as your class count grows.

## 6. Objects, Methods, and the `this` Pointer

Every non-static member function receives a hidden pointer named `this`. Use it when you need to refer to the current object explicitly (e.g., returning `*this` for method chaining or disambiguating parameters):

```cpp
class Timer {
public:
    Timer& setMinutes(int value) {
        minutes_ = value;
        return *this; // enables chaining
    }

private:
    int minutes_{};
};
```

`const` member functions, declared with `... foo() const`, promise not to modify the observable state of the object and treat `this` as `const Timer*`. Mark accessors and pure queries as `const` by default.

## 7. Memory Layout and Object Size

All non-static data members contribute to an object’s size. Static members belong to the class itself. You can inspect sizes offline using `sizeof`:

```cpp
class Packet {
    char header[4];
    std::uint32_t payloadSize{};
};

std::cout << sizeof(Packet) << '\n';
```

Remember that compilers insert padding to respect alignment requirements; grouping members by size can reduce wasted bytes.

## 8. Working with Collections of Objects

Standard containers work seamlessly with user-defined types when you provide the necessary operations:

```cpp
std::vector<Course> catalog;
catalog.push_back(Course{});
catalog.emplace_back(); // Calls default constructor in-place
```

If your class is movable (default in C++17 when all members are movable), the container handles resizing efficiently.

## 9. Interoperability with C

C has no classes, but you can approximate encapsulation by storing data in a `struct` and passing pointers to functions that operate on it. C++ classes provide stronger guarantees:

| Concept | C-style approach | C++ class approach |
|---------|-----------------|--------------------|
| Encapsulation | Exposed `struct` fields | Private data members |
| Methods | Separate functions with `struct*` parameter | Member functions |
| Namespacing | Function name prefixes | Nested in class scope |

Understanding both perspectives helps when migrating legacy C code to modern C++.

## 10. Building and Running Offline

Create `classes_demo.cpp` with your class definition and a `main()` function, then compile and run directly:

```bash
# Compile with warnings enabled
 g++ -std=c++17 -Wall -Wextra -pedantic classes_demo.cpp -o classes_demo
# Run the executable
 ./classes_demo
```

- Use `-std=c++17` to align with this curriculum.
- `-Wall -Wextra -pedantic` catch mistaken access specifiers or missing return statements early.
- If compilation fails, read the first error—often one fix resolves multiple follow-on messages.

## 11. Troubleshooting Checklist

- **“undefined reference” errors**: Ensure each member function declared in a header is defined exactly once in a source file and linked.
- **Access violation**: Did you accidentally access a private member from outside the class? Add a public accessor or adjust visibility.
- **Linker complaining about duplicate symbols**: Move definitions into a single translation unit or mark small functions as `inline`.
- **Unexpected copies**: Pass large user-defined types by `const&` to avoid heavy copying.

## 12. Practice Ideas

1. Model a `LibraryBook` class with methods to check out and return books, tracking the borrower and due date.
2. Create a `Stopwatch` class that records elapsed milliseconds between `start()` and `stop()`, using `std::chrono` APIs.
3. Build a `ProgressTracker` class with `increment()` and `percentage()` methods, then use it to monitor a loop.

When you complete the practice exercise, expect to:

- Define a well-encapsulated class in pure C++17.
- Provide public methods that enforce invariants and derive summaries.
- Follow the prescribed input order and match output formatting exactly.
- Compile and run the solution offline using the command sequence provided earlier.
