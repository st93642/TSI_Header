# Function and Class Templates

Templates enable generic programming in C++, allowing code to work with any type while maintaining type safety. This lesson covers function templates, class templates, template specialization, and variadic templates.

## Why Templates Matter

Templates are one of C++'s most powerful features, enabling generic programming - writing code that works with multiple types without sacrificing type safety or performance. Before templates, you'd need to write separate functions for each type or use unsafe void pointers. Templates solve this by letting the compiler generate type-specific code automatically.

Benefits of templates:

- **Type safety**: Compile-time type checking prevents runtime errors
- **Performance**: No runtime overhead from type erasure
- **Code reuse**: Write once, use with any compatible type
- **Abstraction**: Hide implementation details behind generic interfaces
- **Zero-cost abstraction**: Template code is as efficient as hand-written type-specific code

## Learning goals

- Write function templates for generic algorithms.
- Create class templates for generic containers.
- Use template specialization for specific types.
- Understand template instantiation and compilation.
- Apply variadic templates for flexible functions.

## Function templates

Function templates allow you to write generic functions that work with any data type. The compiler generates the actual function code when you use the template with specific types.

### Basic Syntax

```cpp
template <typename T>
T max(T a, T b) {
    return a > b ? a : b;
}

int main() {
    std::cout << max(5, 3) << std::endl;       // int
    std::cout << max(5.5, 3.2) << std::endl;   // double
    std::cout << max('a', 'z') << std::endl;   // char
}
```

### Template Parameters

- `typename T` or `class T`: Both mean the same thing - a type parameter
- You can have multiple parameters: `template <typename T, typename U>`
- Parameters can have default values: `template <typename T = int>`

### How Template Instantiation Works

When you call `max(5, 3)`:

1. Compiler sees `max<int>(5, 3)` (deduces T = int)
2. Generates: `int max(int a, int b) { return a > b ? a : b; }`
3. Calls the generated function

This happens at compile time - no runtime overhead!

### Template Argument Deduction

The compiler can often figure out template parameters from function arguments:

```cpp
template <typename T>
void print(T value) {
    std::cout << value << std::endl;
}

print(42);        // T = int
print(3.14);      // T = double
print("hello");   // T = const char*
```

### Explicit Template Arguments

Sometimes you need to specify types explicitly:

```cpp
template <typename T>
T convert(const std::string& s) {
    if constexpr (std::is_same_v<T, int>) {
        return std::stoi(s);
    } else if constexpr (std::is_same_v<T, double>) {
        return std::stod(s);
    }
}

int i = convert<int>("42");
double d = convert<double>("3.14");
```

### Common Issues with Function Templates

1. **Operator requirements**: Templates assume operators exist
2. **Type compatibility**: All uses of T must be compatible
3. **Performance**: Code bloat from multiple instantiations

### Best Practices

- Keep templates simple and focused
- Document requirements for template parameters
- Use concepts (C++20) to constrain parameters
- Consider non-template alternatives for simple cases

### Checkpoint: Function template

1. Write template for `swap` function.
2. Test with different types (int, double, std::string).
3. Handle potential issues with const or reference types.

## Class templates

Class templates allow you to create generic classes that work with any type. This is the foundation of STL containers like `std::vector`, `std::map`, etc.

### Basic Class Template

```cpp
template <typename T>
class Stack {
private:
    std::vector<T> data;
public:
    void push(T value) { data.push_back(value); }
    T pop() {
        if (data.empty()) throw std::out_of_range("Stack is empty");
        T val = data.back();
        data.pop_back();
        return val;
    }
    bool empty() const { return data.empty(); }
    size_t size() const { return data.size(); }
};

int main() {
    Stack<int> intStack;
    intStack.push(1);
    intStack.push(2);
    std::cout << intStack.pop() << std::endl; // 2
    
    Stack<std::string> stringStack;
    stringStack.push("hello");
    stringStack.push("world");
    std::cout << stringStack.pop() << std::endl; // "world"
}
```

### Template Member Functions

Class templates can have template member functions with different parameters:

```cpp
template <typename T>
class Container {
private:
    std::vector<T> data;
public:
    // Regular member function
    void add(T value) { data.push_back(value); }
    
    // Template member function
    template <typename U>
    void addConverted(U value) { 
        data.push_back(static_cast<T>(value)); 
    }
    
    // Template constructor
    template <typename InputIt>
    Container(InputIt first, InputIt last) : data(first, last) {}
};
```

### Static Members in Templates

Each template instantiation gets its own static members:

```cpp
template <typename T>
class Counter {
private:
    static int count;  // One count per T
public:
    Counter() { ++count; }
    static int getCount() { return count; }
};

template <typename T>
int Counter<T>::count = 0;  // Definition outside class

int main() {
    Counter<int> c1, c2;      // count = 2
    Counter<double> c3;       // count = 1 (different static)
    
    std::cout << Counter<int>::getCount() << std::endl;    // 2
    std::cout << Counter<double>::getCount() << std::endl; // 1
}
```

### Template Inheritance

Templates can inherit from other templates:

```cpp
template <typename T>
class Base {
protected:
    T value;
public:
    Base(T v) : value(v) {}
};

template <typename T>
class Derived : public Base<T> {
public:
    Derived(T v) : Base<T>(v) {}
    void print() { std::cout << this->value << std::endl; }
};
```

### Friend Functions and Templates

```cpp
template <typename T>
class MyClass {
private:
    T data;
public:
    MyClass(T d) : data(d) {}
    
    // Friend function template
    template <typename U>
    friend std::ostream& operator<<(std::ostream& os, const MyClass<U>& obj) {
        return os << obj.data;
    }
};
```

### Checkpoint: Class template

1. Implement `Pair<T1, T2>` class with constructors, accessors, and comparison.
2. Add template member functions for type conversion.
3. Test with different type combinations.

## Template specialization

Template specialization allows you to provide custom implementations for specific types when the generic template isn't appropriate.

### Full Specialization

Provide a completely different implementation for a specific type:

```cpp
template <typename T>
class Printer {
public:
    void print(T value) { std::cout << value << std::endl; }
};

// Full specialization for bool
template <>
class Printer<bool> {
public:
    void print(bool value) {
        std::cout << (value ? "true" : "false") << std::endl;
    }
};

// Full specialization for const char*
template <>
class Printer<const char*> {
public:
    void print(const char* value) {
        if (value) {
            std::cout << "\"" << value << "\"" << std::endl;
        } else {
            std::cout << "null" << std::endl;
        }
    }
};

int main() {
    Printer<int> p1;
    p1.print(42);        // 42
    
    Printer<bool> p2;
    p2.print(true);      // true
    
    Printer<const char*> p3;
    p3.print("hello");   // "hello"
}
```

### Partial Specialization

Specialize for a pattern of types:

```cpp
// Primary template
template <typename T, typename U>
class Pair {
public:
    T first;
    U second;
    Pair(T f, U s) : first(f), second(s) {}
};

// Partial specialization: both types same
template <typename T>
class Pair<T, T> {
public:
    T first;
    T second;
    Pair(T f, T s) : first(f), second(s) {}
    bool same() const { return first == second; }
};

// Partial specialization: pointer types
template <typename T, typename U>
class Pair<T*, U*> {
public:
    T* first;
    U* second;
    Pair(T* f, U* s) : first(f), second(s) {}
    ~Pair() { delete first; delete second; }  // Manage pointers
};
```

### When to Use Specialization

- **Type-specific behavior**: When certain types need different logic
- **Optimization**: When generic version is inefficient for some types
- **Interface differences**: When some types don't support certain operations

### Function Template Specialization

Function templates can also be specialized:

```cpp
template <typename T>
void process(T value) {
    std::cout << "Generic: " << value << std::endl;
}

// Full specialization
template <>
void process<int>(int value) {
    std::cout << "Int: " << value << " (specialized)" << std::endl;
}

// Overload (preferred over specialization for functions)
void process(double value) {
    std::cout << "Double: " << value << " (overloaded)" << std::endl;
}
```

**Note**: Function template overloading is usually preferred over specialization.

### Checkpoint: Specialization

1. Specialize `Printer` for `std::string` to add quotes.
2. Create partial specialization for `Pair<T*, T*>` that manages memory.
3. Test with different type combinations.

## Non-type template parameters

Template parameters can be values (not just types), allowing compile-time constants to be part of the type system.

### Basic Non-Type Parameters

```cpp
template <typename T, size_t N>
class Array {
private:
    T data[N];
public:
    T& operator[](size_t index) { 
        if (index >= N) throw std::out_of_range("Index out of bounds");
        return data[index]; 
    }
    const T& operator[](size_t index) const { 
        if (index >= N) throw std::out_of_range("Index out of bounds");
        return data[index]; 
    }
    size_t size() const { return N; }
};

int main() {
    Array<int, 5> arr;
    arr[0] = 10;
    arr[4] = 50;
    std::cout << arr.size() << std::endl; // 5
    
    // Different sizes are different types
    Array<int, 10> bigArr;  // Different type from arr
}
```

### Allowed Non-Type Parameter Types

- **Integral types**: `int`, `size_t`, `char`, `bool`
- **Enumeration types**
- **Pointers to objects/functions**
- **References to objects/functions**
- **Pointers to member objects/functions**

### Examples of Non-Type Parameters

```cpp
// Fixed-size matrix
template <typename T, size_t Rows, size_t Cols>
class Matrix {
    T data[Rows][Cols];
public:
    T& at(size_t r, size_t c) { return data[r][c]; }
    static constexpr size_t rows = Rows;
    static constexpr size_t cols = Cols;
};

// String literal as parameter (pointer)
template <size_t N>
class FixedString {
    char data[N];
public:
    FixedString(const char (&str)[N]) {
        std::copy(str, str + N, data);
    }
    const char* c_str() const { return data; }
};

// Function pointer parameter
template <int (*Func)(int)>
class FunctionWrapper {
public:
    int call(int x) { return Func(x); }
};
```

### Template Metaprogramming with Non-Type Parameters

```cpp
// Compile-time power calculation
template <int Base, int Exp>
struct Power {
    static constexpr int value = Base * Power<Base, Exp - 1>::value;
};

template <int Base>
struct Power<Base, 0> {
    static constexpr int value = 1;
};

int main() {
    std::cout << Power<2, 10>::value << std::endl; // 1024
}
```

### Checkpoint: Non-type

1. Create `Buffer<T, Size>` class with bounds checking.
2. Implement `Matrix<T, Rows, Cols>` with basic operations.
3. Use non-type parameters for compile-time computations.

## Template metaprogramming basics

Template metaprogramming (TMP) uses templates to perform computations at compile time. This enables complex type manipulation and optimization.

### Basic TMP: Factorial

```cpp
template <int N>
struct Factorial {
    static const int value = N * Factorial<N-1>::value;
};

template <>
struct Factorial<0> {
    static const int value = 1;
};

int main() {
    std::cout << Factorial<5>::value << std::endl; // 120
    // This is computed at compile time!
}
```

### TMP for Type Traits

```cpp
// Check if type is pointer
template <typename T>
struct IsPointer {
    static const bool value = false;
};

template <typename T>
struct IsPointer<T*> {
    static const bool value = true;
};

// Check if types are same
template <typename T, typename U>
struct IsSame {
    static const bool value = false;
};

template <typename T>
struct IsSame<T, T> {
    static const bool value = true;
};
```

### Conditional Types with TMP

```cpp
template <bool Condition, typename TrueType, typename FalseType>
struct Conditional {
    using type = TrueType;
};

template <typename TrueType, typename FalseType>
struct Conditional<false, TrueType, FalseType> {
    using type = FalseType;
};

// Usage
using IntOrDouble = Conditional<sizeof(int) > 4, int, double>::type;
```

### TMP for Loop Unrolling

```cpp
template <int N>
struct UnrollLoop {
    template <typename Func>
    static void execute(Func f) {
        f(N-1);
        UnrollLoop<N-1>::execute(f);
    }
};

template <>
struct UnrollLoop<0> {
    template <typename Func>
    static void execute(Func f) {}
};

int main() {
    auto printNum = [](int n) { std::cout << n << " "; };
    UnrollLoop<5>::execute(printNum); // Prints: 4 3 2 1 0
}
```

### Advantages of TMP

- **Zero runtime cost**: All computation at compile time
- **Type safety**: Compile-time type checking
- **Optimization**: Compiler can optimize based on known values
- **Generic libraries**: Enable complex generic code

### Limitations

- **Complexity**: Hard to read and debug
- **Compile time**: Can slow compilation
- **Error messages**: Cryptic template errors
- **C++11/14/17/20 alternatives**: `constexpr`, `consteval`, concepts often better

### Checkpoint: Metaprogramming

1. Implement `Power<Base, Exp>` template for compile-time exponentiation.
2. Create `IsPointer<T>` type trait.
3. Use TMP for conditional compilation.

## Variadic templates

Variadic templates allow functions and classes to accept a variable number of template arguments. This is the foundation of features like `std::tuple`, `std::variant`, and parameter packs.

### Basic Variadic Functions

```cpp
template <typename... Args>
void print(Args... args) {
    (std::cout << ... << args) << std::endl; // C++17 fold expression
}

int main() {
    print(1, 2.5, "hello");     // 12.5hello
    print("Single arg");        // Single arg
    print();                    // (empty line)
}
```

### Parameter Packs

- `Args...` is a parameter pack
- Can contain zero or more types
- Accessed with `...` operator

### Manual Pack Expansion

```cpp
template <typename... Args>
void printWithSpaces(Args... args) {
    // Manual expansion
    int dummy[] = {0, ((std::cout << args << " "), 0)... };
    std::cout << std::endl;
}

template <typename... Args>
size_t countArgs(Args... args) {
    return sizeof...(Args);  // Number of arguments
}
```

### Variadic Class Templates

```cpp
template <typename... Types>
class Tuple;  // Forward declaration

// Base case: empty tuple
template <>
class Tuple<> {};

// Recursive case
template <typename T, typename... Rest>
class Tuple<T, Rest...> {
private:
    T value;
    Tuple<Rest...> rest;
public:
    Tuple(T v, Rest... r) : value(v), rest(r...) {}
    
    T get() const { return value; }
    const Tuple<Rest...>& tail() const { return rest; }
};

int main() {
    Tuple<int, double, std::string> t(42, 3.14, "hello");
    std::cout << t.get() << std::endl; // 42
}
```

### Advanced: Perfect Forwarding

```cpp
template <typename... Args>
void forwardToFunction(Args&&... args) {
    someFunction(std::forward<Args>(args)...);
}
```

### Fold Expressions (C++17)

```cpp
template <typename... Args>
auto sum(Args... args) {
    return (args + ...);  // Binary left fold
}

template <typename... Args>
bool allTrue(Args... args) {
    return (args && ...);  // Binary left fold
}

template <typename... Args>
void printAll(Args... args) {
    (std::cout << ... << args) << std::endl;  // Unary left fold
}
```

### Variadic Templates in Practice

Used in:

- `std::tuple`: Store multiple types
- `std::variant`: Type-safe union
- `std::function`: Type-erased callable
- Perfect forwarding in factory functions

### Checkpoint: Variadic

1. Write `sum` function with variadic template and fold expression.
2. Implement simple `Tuple` class with `get<N>()` method.
3. Create `printAll` function that prints any number of arguments.

## Template constraints (C++20)

Require certain properties.

```cpp
#include <concepts>

template <typename T>
concept Addable = requires(T a, T b) { a + b; };

template <Addable T>
T add(T a, T b) {
    return a + b;
}
```

- Concepts for better error messages.

### Checkpoint: Concepts

1. Use concept for `Printable` types.

## SFINAE and enable_if

Advanced template techniques.

```cpp
template <typename T>
typename std::enable_if<std::is_integral<T>::value, T>::type
increment(T value) {
    return value + 1;
}
```

- Substitution failure is not an error.

### Checkpoint: SFINAE

1. Use `enable_if` for integral types.

## Mini project: Generic sort

Implement bubble sort as template function.

1. Function template for any comparable type.
2. Test with arrays of int, double, string.

### Success criteria

- Works with different types.
- Correct sorting.

## Guided practice challenges

1. Implement a generic `find` function.
2. Create a `Tuple` class template.
3. Write a type-safe `printf` with variadic templates.
4. Add iterator support to custom container.
5. Use templates for policy-based design.

## Self-check questions

1. What is a function template?
2. How to specialize a class template?
3. What are non-type parameters?
4. How do variadic templates work?
5. What is SFINAE?

## Recap and next steps

Templates enable generic code. Next, explore STL internals.
