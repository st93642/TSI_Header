# Inheritance

Inheritance allows classes to inherit properties and behavior from base classes, enabling code reuse and hierarchical relationships. This lesson covers base/derived classes, access specifiers in inheritance, overriding, and the `is-a` relationship.

## Why Inheritance Matters

Inheritance is a fundamental concept in object-oriented programming that allows you to create new classes based on existing ones. It promotes code reuse, establishes hierarchical relationships, and enables polymorphism.

Imagine you're building a game with different types of characters: warriors, mages, archers. Instead of duplicating common character functionality (health, name, level), you can create a base Character class and have specialized classes inherit from it.

Benefits of inheritance:

- **Code reuse**: Avoid duplicating common functionality
- **Hierarchical organization**: Model real-world relationships
- **Polymorphism foundation**: Enable dynamic behavior
- **Maintainability**: Changes to base class affect all derived classes
- **Extensibility**: Add new types without modifying existing code

## Learning goals

- Define base and derived classes.
- Use public, protected, private inheritance.
- Override base class methods.
- Understand constructor/destructor order.
- Apply inheritance for polymorphism foundation.

## Basic inheritance

Inheritance creates a relationship between classes where derived classes (child classes) inherit properties and behaviors from base classes (parent classes). The derived class can add new members or override inherited ones.

### Syntax and Structure

```cpp
class Base {
protected:
    std::string name;
public:
    Base(std::string n) : name(n) {}
    virtual void speak() const { std::cout << "Animal sound" << std::endl; }
};

class Dog : public Animal {
public:
    Dog(std::string n) : Animal(n) {}  // Call base constructor
    void speak() const override { 
        std::cout << name << " barks" << std::endl; 
    }
};
```

### Key Concepts

1. **Base class**: The class being inherited from
2. **Derived class**: The class that inherits
3. **Inheritance specifier**: `public`, `protected`, or `private`
4. **Member inheritance**: Data members and methods are inherited
5. **Constructor chaining**: Base constructors called before derived

### The "Is-A" Relationship

Public inheritance models an "is-a" relationship. A Dog "is an" Animal. This means anywhere you can use an Animal, you should be able to use a Dog.

### Constructor Order

When creating a derived object:

1. Base class constructor executes first
2. Derived class constructor executes second

When destroying:

1. Derived class destructor executes first
2. Base class destructor executes second

### Accessing Base Members

- `public` members: Accessible everywhere
- `protected` members: Accessible in derived classes and base class
- `private` members: Only accessible in the defining class

### Checkpoint: Basic inheritance

1. Create `Cat` inheriting from `Animal`, override `speak`.
2. Add a `breed` member to `Dog` and `Cat`.
3. Create objects and demonstrate inheritance.

## Access specifiers in inheritance

Control inheritance visibility.

```cpp
class Base {
private:
    int privateData;
protected:
    int protectedData;
public:
    int publicData;
};

class Derived : public Base {
    // privateData: inaccessible
    // protectedData: protected
    // publicData: public
};

class DerivedPrivate : private Base {
    // All inherited as private
};
```

- Public: maintains access levels.
- Private: makes all inherited private.
- Protected: public becomes protected.

### Checkpoint: Access inheritance

1. Use private inheritance for `Engine` in `Car`.

## Constructor and destructor order

Base constructed first, derived last.

```cpp
class Base {
public:
    Base() { std::cout << "Base constructor" << std::endl; }
    ~Base() { std::cout << "Base destructor" << std::endl; }
};

class Derived : public Base {
public:
    Derived() { std::cout << "Derived constructor" << std::endl; }
    ~Derived() { std::cout << "Derived destructor" << std::endl; }
};
```

- Destructors in reverse order.

### Checkpoint: Order

1. Observe construction/destruction order.

## Overriding methods

Derived classes can redefine behavior.

```cpp
class Shape {
public:
    virtual double area() const = 0; // pure virtual
    virtual void draw() const { std::cout << "Drawing shape" << std::endl; }
};

class Circle : public Shape {
private:
    double radius;
public:
    Circle(double r) : radius(r) {}
    double area() const override { return 3.14159 * radius * radius; }
    void draw() const override { std::cout << "Drawing circle" << std::endl; }
};
```

- Virtual for dynamic binding.
- Override keyword for clarity.
- Pure virtual makes abstract.

### Checkpoint: Override

1. Override `area` in `Rectangle`.

## Multiple inheritance

Inherit from multiple bases.

```cpp
class Vehicle {
public:
    void move() { std::cout << "Moving" << std::endl; }
};

class Electric {
public:
    void charge() { std::cout << "Charging" << std::endl; }
};

class ElectricCar : public Vehicle, public Electric {
    // Inherits from both
};
```

- Diamond problem with common base.

### Checkpoint: Multiple

1. Create `HybridCar` inheriting from `Vehicle` and `Electric`.

## Composition vs inheritance

Favor composition over inheritance.

```cpp
class Engine {
public:
    void start() { std::cout << "Engine started" << std::endl; }
};

class Car {
private:
    Engine engine; // composition
public:
    void start() { engine.start(); }
};
```

- `has-a` vs `is-a`.

### Checkpoint: Composition

1. Use composition for `Car` with `Engine`.

## Virtual destructors

Ensure proper cleanup in inheritance.

```cpp
class Base {
public:
    virtual ~Base() { std::cout << "Base destroyed" << std::endl; }
};

class Derived : public Base {
public:
    ~Derived() { std::cout << "Derived destroyed" << std::endl; }
};
```

- Virtual destructor for polymorphic deletion.

### Checkpoint: Virtual dtor

1. Make destructors virtual in hierarchy.

## Inheritance best practices

Use inheritance wisely.

- Prefer composition.
- Keep hierarchies shallow.
- Use abstract bases for interfaces.
- Avoid slicing.

### Checkpoint: Best practices

1. Refactor code to use composition.

## Mini project: Employee hierarchy

Create `Employee` base, `Manager`, `Developer` derived.

1. Base with salary, name.
2. Derived with specific methods.
3. Demonstrate polymorphism.

### Success criteria

- Proper inheritance.
- Overridden methods.
- Correct behavior.

## Guided practice challenges

1. Implement a `File` hierarchy with `TextFile`, `BinaryFile`.
2. Create a `Shape` hierarchy with area calculations.
3. Build a `Device` class with `Printer`, `Scanner`.
4. Add virtual functions to existing classes.
5. Resolve diamond inheritance issue.

## Self-check questions

1. What is inheritance?
2. Difference between public and private inheritance?
3. When to use virtual functions?
4. What is the diamond problem?
5. Why virtual destructors?

## Recap and next steps

Inheritance enables hierarchies. Next, explore polymorphism.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Inheritance in C++ — Virtuals, Slicing & Design (Appendix — inheritance_cpp-appendix2)

Guidance for safe inheritance usage in C++: prefer composition, use virtual destructors, avoid object slicing, and test polymorphic behavior.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Issue</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Virtual destructor</td><td>virtual ~Base()</td><td>Always provide if deleting via base pointer</td></tr>
    <tr><td>Slicing</td><td>Use pointers/references</td><td>Avoid storing derived objects by value in base-type containers</td></tr>
    <tr><td>Testing</td><td>Polymorphic tests</td><td>Use base pointers to assert overridden behaviour</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example

```cpp
struct Base { virtual ~Base() = default; virtual void act() = 0; };
struct Derived : Base { void act() override { /*...*/ } };

Base* b = new Derived();
delete b; // safe because Base has virtual destructor
```

### Exercises (Appendix — inheritance_cpp-appendix2)

1. Write a polymorphic hierarchy with a virtual method and test that a base pointer calls the derived implementation.
2. Demonstrate object slicing and fix it by using pointers or smart pointers; add tests showing the difference.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Inheritance — Patterns, CRTP & Testing (Appendix — inheritance_cpp-appendix3)

Notes on modern inheritance patterns, when to use CRTP (Curiously Recurring Template Pattern), and testing strategies for polymorphic hierarchies.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>CRTP</td><td>Static polymorphism</td><td>Avoids virtual calls when appropriate</td></tr>
    <tr><td>Strategy</td><td>Composition over inheritance</td><td>Prefer interfaces for behaviour</td></tr>
    <tr><td>Testing</td><td>Polymorphic assertions</td><td>Test through base interfaces</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: CRTP

```cpp
template <typename Derived>
struct Base {
  void interface() { static_cast<Derived*>(this)->implementation(); }
};

struct Impl : Base<Impl> { void implementation() {/*...*/} };
```

### Exercises (Appendix — inheritance_cpp-appendix3)

1. Implement a CRTP helper and use it for a small compile-time polymorphism example; add tests asserting behaviour.
2. Replace an inheritance-based design with composition and add tests verifying behaviour remains correct.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
