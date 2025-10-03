# Classes and Encapsulation

Encapsulation is the core of object-oriented programming, bundling data and behavior while hiding implementation details. This lesson dives deep into class design, access control, constructors, destructors, and best practices for creating robust, maintainable classes.

## Learning goals

- Design classes with strong encapsulation.
- Implement multiple constructors and copy semantics.
- Use const-correctness in member functions.
- Understand object lifecycle and resource management.
- Apply class design principles for real-world code.

## Advanced class design

Classes should hide complexity and expose clean interfaces.

```cpp
class BankAccount {
private:
    std::string accountNumber;
    double balance;
    static int nextAccountNumber;
public:
    BankAccount(double initialBalance);
    BankAccount(const BankAccount& other);
    ~BankAccount();
    
    void deposit(double amount);
    bool withdraw(double amount);
    double getBalance() const;
    std::string getAccountNumber() const;
    
    static int getNextAccountNumber();
};
```

- Private data, public interface.
- Static members for shared state.

### Checkpoint: Class design

1. Design a `LibraryBook` class with private title, author, isbn, public methods.

## Constructors and initialization

Proper initialization prevents bugs.

```cpp
class Rectangle {
private:
    double width, height;
public:
    Rectangle() : width(0), height(0) {}
    Rectangle(double w, double h) : width(w), height(h) {}
    Rectangle(const Rectangle& other) : width(other.width), height(other.height) {}
    
    Rectangle& operator=(const Rectangle& other) {
        if (this != &other) {
            width = other.width;
            height = other.height;
        }
        return *this;
    }
};
```

- Default, parameterized, copy constructors.
- Assignment operator for deep copies if needed.

### Checkpoint: Constructors

1. Add copy constructor and assignment to `LibraryBook`.

## Destructors and RAII

Manage resources automatically.

```cpp
class FileManager {
private:
    std::FILE* file;
public:
    FileManager(const char* filename) : file(std::fopen(filename, "w")) {}
    ~FileManager() { if (file) std::fclose(file); }
    
    void write(const char* data) {
        if (file) std::fputs(data, file);
    }
};
```

- Destructor cleans up resources.
- RAII ensures exception safety.

### Checkpoint: Destructor

1. Implement RAII for `FileManager`.

## Const member functions

Promise not to modify object.

```cpp
class Circle {
private:
    double radius;
public:
    Circle(double r) : radius(r) {}
    double getArea() const { return 3.14159 * radius * radius; }
    double getCircumference() const { return 2 * 3.14159 * radius; }
    void setRadius(double r) { radius = r; }
};
```

- `const` after parameter list.
- Can call other const functions.

### Checkpoint: Const

1. Make getters const in `LibraryBook`.

## Access levels and encapsulation

Control what outsiders can do.

```cpp
class Employee {
private:
    std::string name;
    double salary;
protected:
    void setSalary(double s) { salary = s; }
public:
    Employee(std::string n, double s) : name(n), salary(s) {}
    std::string getName() const { return name; }
    virtual double getSalary() const { return salary; }
};
```

- Private: internal only.
- Protected: for derived classes.
- Public: interface.

### Checkpoint: Access

1. Use protected in `Employee` for salary setting.

## Static members

Shared across instances.

```cpp
class Counter {
private:
    static int count;
public:
    Counter() { count++; }
    ~Counter() { count--; }
    static int getCount() { return count; }
};
int Counter::count = 0;
```

- Static data initialized outside class.
- Static functions don't need instance.

### Checkpoint: Static

1. Add static member to track `LibraryBook` instances.

## Friend classes and functions

Grant access to private members.

```cpp
class Printer {
public:
    void printAccount(const BankAccount& acc);
};

class BankAccount {
    friend class Printer;
    friend void auditAccount(const BankAccount& acc);
private:
    double balance;
};
```

- Use sparingly for tight coupling.

### Checkpoint: Friend

1. Make `Printer` friend of `LibraryBook`.

## Operator overloading

Make classes behave like built-ins.

```cpp
class Complex {
private:
    double real, imag;
public:
    Complex(double r = 0, double i = 0) : real(r), imag(i) {}
    
    Complex operator+(const Complex& other) const {
        return Complex(real + other.real, imag + other.imag);
    }
    
    Complex& operator+=(const Complex& other) {
        real += other.real;
        imag += other.imag;
        return *this;
    }
    
    friend std::ostream& operator<<(std::ostream& os, const Complex& c) {
        os << c.real << " + " << c.imag << "i";
        return os;
    }
};
```

- Overload arithmetic operators.
- Friend for stream operators.

### Checkpoint: Operators

1. Overload `+` and `<<` for `Complex`.

## Class invariants and design

Maintain object consistency.

```cpp
class Date {
private:
    int day, month, year;
    bool isValidDate(int d, int m, int y) const;
public:
    Date(int d, int m, int y) {
        if (isValidDate(d, m, y)) {
            day = d; month = m; year = y;
        } else {
            throw std::invalid_argument("Invalid date");
        }
    }
};
```

- Validate in constructors.
- Keep invariants true.

### Checkpoint: Invariants

1. Add validation to `Date` class.

## Mini project: Shape hierarchy base

Create base `Shape` class with derived `Circle`, `Rectangle`.

1. Base class with virtual area().
2. Derived classes override area().
3. Demonstrate polymorphism.

### Success criteria

- Proper encapsulation.
- Virtual functions.
- Correct area calculations.

## Guided practice challenges

1. Implement a `String` class with copy-on-write.
2. Create a `Matrix` class with operator overloading.
3. Build a `SmartPointer` template class.
4. Add move semantics to existing classes.
5. Design a `Logger` singleton class.

## Self-check questions

1. Why use encapsulation?
2. Difference between copy constructor and assignment?
3. When to use const member functions?
4. What are static members for?
5. How to overload operators?

## Recap and next steps

Encapsulation enables modular design. Next, explore inheritance.
