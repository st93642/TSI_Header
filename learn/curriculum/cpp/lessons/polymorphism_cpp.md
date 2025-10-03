# Polymorphism and Virtual Functions

Polymorphism allows objects of different classes to be treated as objects of a common base class, enabling dynamic behavior. This lesson covers virtual functions, abstract classes, runtime polymorphism, and the virtual table mechanism.

## Why Polymorphism Matters

Polymorphism is one of the four fundamental principles of object-oriented programming (alongside encapsulation, inheritance, and abstraction). It allows you to write code that can work with objects of different types in a uniform way, promoting code reusability and flexibility.

Imagine you're building a graphics application that needs to draw various shapes: circles, rectangles, triangles. Without polymorphism, you'd need separate functions for each shape type. With polymorphism, you can have a single `draw()` function that works for any shape, as long as they inherit from a common base class.

Benefits of polymorphism:

- **Code reusability**: Write generic code that works with multiple types
- **Extensibility**: Add new types without changing existing code
- **Maintainability**: Centralized logic for related operations
- **Type safety**: Compile-time checks prevent errors

## Learning goals

- Implement virtual functions and overriding.
- Use abstract base classes and pure virtual functions.
- Understand dynamic binding and vtables.
- Apply polymorphism in collections and function parameters.
- Handle object slicing and covariant return types.

## Virtual functions

Virtual functions are the cornerstone of polymorphism in C++. When a function is declared as virtual in a base class, it allows derived classes to provide their own implementation of that function. This is achieved through a mechanism called dynamic dispatch, where the correct function to call is determined at runtime based on the actual type of the object, not the type of the pointer or reference.

In the example below, the Shape class declares area() as a pure virtual function (= 0), meaning any class that inherits from Shape must provide an implementation for area(). The draw() function is virtual but has a default implementation, so derived classes can choose to override it or not.

The override keyword is not required but highly recommended as it tells the compiler that you intend to override a virtual function, and it will catch errors if the function signature doesn't match.

```cpp
class Shape {
public:
    virtual double area() const = 0; // pure virtual
    virtual void draw() const {
        std::cout << "Drawing shape" << std::endl;
    }
};

class Circle : public Shape {
private:
    double radius;
public:
    Circle(double r) : radius(r) {}
    double area() const override {
        return 3.14159 * radius * radius;
    }
    void draw() const override {
        std::cout << "Drawing circle" << std::endl;
    }
};
```

- Virtual: can be overridden.
- Pure virtual: must be overridden.
- Override: ensures correct overriding.

### Understanding Virtual Function Mechanics

When you declare a function as virtual, the compiler creates a virtual function table (vtable) for each class that contains virtual functions. Each object of such a class contains a hidden pointer (vptr) to its class's vtable. When you call a virtual function through a pointer or reference, the compiler looks up the function in the vtable at runtime.

This runtime lookup is what enables polymorphism - the same function call can execute different code depending on the actual object type.

### Pure Virtual Functions and Abstract Classes

A pure virtual function is declared with = 0 at the end. Classes containing pure virtual functions cannot be instantiated directly - they are called abstract classes. Abstract classes serve as interfaces or base classes that define a common interface for derived classes.

For example, you can't create a Shape object directly because area() is pure virtual. You must create concrete shapes like Circle or Rectangle that implement all pure virtual functions.

### The override Keyword

Introduced in C++11, the override keyword is optional but crucial for safety. It tells the compiler that this function is intended to override a virtual function from a base class. If no matching virtual function exists in the base class, the compiler will generate an error.

This prevents subtle bugs where you think you're overriding a function but actually created a new one due to a signature mismatch.

### Checkpoint: Virtual

1. Make `Shape` abstract with pure virtual `area`.
2. Add a virtual `perimeter()` function with a default implementation.
3. Override both in `Circle` and `Rectangle` classes.

## Runtime polymorphism

Runtime polymorphism, also known as dynamic polymorphism, occurs when the decision about which function to call is made at runtime rather than compile time. This is in contrast to compile-time polymorphism (like function overloading or templates) where the binding happens during compilation.

The key to runtime polymorphism is using pointers or references to base classes that point to derived class objects. When you call a virtual function through such a pointer or reference, the system determines which implementation to use based on the actual object type, not the pointer type.

```cpp
void printArea(const Shape* shape) {
    std::cout << "Area: " << shape->area() << std::endl;
}

int main() {
    Circle c(5);
    Shape* s = &c;
    printArea(s); // calls Circle::area, not Shape::area
}
```

In this example, even though `s` is a `Shape*`, calling `s->area()` invokes `Circle::area()` because the actual object is a Circle. This is dynamic binding in action.

### Why Use Pointers and References?

You must use pointers or references for polymorphism to work. If you pass objects by value, you get object slicing (which we'll discuss later), and polymorphism is lost.

```cpp
void printArea(Shape shape) { // BAD: pass by value
    std::cout << "Area: " << shape.area() << std::endl;
}

int main() {
    Circle c(5);
    printArea(c); // slices Circle to Shape, loses polymorphism
}
```

Always use `const Shape&` or `Shape*` for polymorphic parameters.

### Dynamic Binding vs Static Binding

- **Static binding**: Function call resolved at compile time based on pointer/reference type
- **Dynamic binding**: Function call resolved at runtime based on actual object type

Only virtual functions use dynamic binding. Non-virtual functions use static binding.

### Checkpoint: Polymorphism

1. Use base pointer to call derived methods.
2. Create a function that takes a `Shape&` and calls both `area()` and `draw()`.
3. Test with different shape types.

## Abstract classes

An abstract class is a class that cannot be instantiated directly. It serves as a blueprint for derived classes and typically contains one or more pure virtual functions that must be implemented by concrete subclasses.

Abstract classes are essential for defining interfaces in C++. They allow you to define a common interface that multiple classes can implement, ensuring that all implementations provide certain functionality.

```cpp
class Animal {
public:
    virtual void speak() const = 0; // pure virtual - must implement
    virtual void eat() const {      // virtual with default - optional override
        std::cout << "Eating..." << std::endl;
    }
    virtual ~Animal() {} // virtual destructor
};

class Dog : public Animal {
public:
    void speak() const override {
        std::cout << "Woof!" << std::endl;
    }
    // eat() inherited, can override if needed
};

class Cat : public Animal {
public:
    void speak() const override {
        std::cout << "Meow!" << std::endl;
    }
    void eat() const override {
        std::cout << "Eating tuna..." << std::endl;
    }
};
```

### Key Characteristics of Abstract Classes

1. **Cannot be instantiated**: `Animal a;` would cause a compile error
2. **Can have data members and implemented methods**
3. **Can have constructors** (called by derived classes)
4. **Pure virtual functions define the interface**
5. **Virtual functions provide optional customization**

### When to Use Abstract Classes

- Define interfaces for plugins or extensions
- Create base classes for related functionality
- Enforce implementation of certain methods in derived classes
- Provide default behavior that can be customized

### Abstract vs Concrete Classes

- **Abstract**: Cannot create objects, defines interface
- **Concrete**: Can create objects, provides complete implementation

### Checkpoint: Abstract

1. Create abstract `Vehicle` with pure virtual `drive`.
2. Add virtual `stop()` with default implementation.
3. Implement `Car` and `Bike` classes.

## Virtual destructors

Virtual destructors are crucial when dealing with polymorphic objects. If a base class destructor is not virtual, deleting a derived object through a base pointer leads to undefined behavior - typically, only the base destructor runs, and derived resources aren't cleaned up properly.

This is one of the most common and dangerous mistakes in C++ polymorphism.

```cpp
class Base {
public:
    Base() { std::cout << "Base ctor" << std::endl; }
    virtual ~Base() { std::cout << "Base dtor" << std::endl; } // virtual!
};

class Derived : public Base {
public:
    Derived() { std::cout << "Derived ctor" << std::endl; }
    ~Derived() { std::cout << "Derived dtor" << std::endl; }
};

int main() {
    Base* b = new Derived(); // Derived ctor, then Base ctor
    delete b; // Derived dtor, then Base dtor - correct!
}
```

Without `virtual ~Base()`, only `Base dtor` would print, and `Derived`'s destructor wouldn't run.

### The Rule

**Always declare destructors virtual in polymorphic base classes.**

Even if the base class doesn't need cleanup, make the destructor virtual to ensure proper destruction of derived objects.

### Virtual Destructors and Pure Virtual Classes

If a class has pure virtual functions, it doesn't automatically make the destructor pure virtual. You can have:

```cpp
class Abstract {
public:
    virtual ~Abstract() = 0; // pure virtual destructor
    virtual void doSomething() = 0;
};

Abstract::~Abstract() {} // must provide implementation
```

Pure virtual destructors require an implementation (usually empty) in the source file.

### Why This Matters

Improper destruction can lead to:

- Resource leaks (files not closed, memory not freed)
- Undefined behavior
- Hard-to-debug crashes

### Checkpoint: Virtual dtor

1. Add virtual destructors to hierarchy.
2. Create a polymorphic container and delete objects properly.

## Function overriding rules

When overriding virtual functions, you must follow strict rules to ensure correct polymorphic behavior. The overriding function must have the same signature as the base class function, with some exceptions.

### Signature Matching

The overriding function must match:

- Return type (with covariant return types allowed)
- Function name
- Parameter types (exactly)
- Const-ness
- Reference qualifiers (C++11)

```cpp
class Base {
public:
    virtual void func(int x) {}
    virtual Base* clone() const { return new Base(*this); }
    virtual void constFunc() const {}
};

class Derived : public Base {
public:
    void func(int x) override {} // ok - exact match
    Derived* clone() const override { return new Derived(*this); } // covariant return
    void constFunc() const override {} // ok - const match
};
```

### Covariant Return Types

Since C++11, you can override a function with a more specific return type. If the base function returns `Base*`, the derived can return `Derived*`.

This is safe because a `Derived*` can be implicitly converted to `Base*`.

### What Cannot Be Changed

- Parameter types (overloading instead)
- Adding/removing `const`
- Exception specifications (in older C++)
- Access level (but can be more public)

### Using the override Keyword

Always use `override` when overriding virtual functions. It provides:

- Compile-time checking
- Clear intent documentation
- Prevention of accidental overloading

### Common Mistakes

```cpp
class Base {
public:
    virtual void func(int x) {}
};

class Derived : public Base {
public:
    void func(double x) {} // NOT overriding - different signature
    virtual void func(int x) {} // NOT overriding - missing override
};
```

### Checkpoint: Override

1. Override with covariant return type.
2. Demonstrate what happens without `override`.

## Polymorphism in containers

One of the most powerful uses of polymorphism is storing objects of different derived types in the same container, then operating on them uniformly. This enables writing generic algorithms that work with any type in the hierarchy.

However, storing polymorphic objects requires careful consideration of ownership and lifetime management.

### Using Smart Pointers

Always use smart pointers (unique_ptr, shared_ptr) for polymorphic objects in containers. Raw pointers lead to memory management issues.

```cpp
#include <vector>
#include <memory>

int main() {
    std::vector<std::unique_ptr<Shape>> shapes;
    
    shapes.push_back(std::make_unique<Circle>(5));
    shapes.push_back(std::make_unique<Rectangle>(4, 3));
    shapes.push_back(std::make_unique<Triangle>(3, 4, 5));
    
    double totalArea = 0.0;
    for (const auto& shape : shapes) {
        shape->draw();
        std::cout << "Area: " << shape->area() << std::endl;
        totalArea += shape->area();
    }
    
    std::cout << "Total area: " << totalArea << std::endl;
}
```

### Why Smart Pointers?

- **Automatic memory management**: No manual delete needed
- **Exception safety**: Memory freed even if exceptions occur
- **Ownership clarity**: unique_ptr = exclusive ownership, shared_ptr = shared
- **Prevents memory leaks**: RAII principle

### Alternative: References in Containers

For non-owning containers, you can use reference_wrapper:

```cpp
#include <vector>
#include <functional>

std::vector<std::reference_wrapper<Shape>> shapes;
Circle c(5);
Rectangle r(4, 3);
shapes.push_back(c);
shapes.push_back(r);

for (Shape& shape : shapes) {
    shape.draw(); // works polymorphically
}
```

But be careful with object lifetimes!

### Performance Considerations

- Virtual function calls have small overhead (vtable lookup)
- Smart pointers have minimal overhead
- For performance-critical code, consider alternatives like std::variant

### Checkpoint: Containers

1. Store shapes in vector, iterate and call methods.
2. Calculate total area of all shapes.
3. Add a new shape type and verify it works without changing the loop.

## Object slicing

Object slicing is a common pitfall when working with polymorphism. It occurs when a derived object is copied to a base object, losing all derived-specific data and behavior.

This happens because C++ copies only the base part of the object, "slicing off" the derived portions.

```cpp
class Shape {
public:
    virtual double area() const = 0;
};

class Circle : public Shape {
private:
    double radius;
public:
    Circle(double r) : radius(r) {}
    double area() const override {
        return 3.14159 * radius * radius;
    }
};

int main() {
    Circle c(5);
    Shape s = c; // SLICING! Only Shape part copied
    
    std::cout << "Circle area: " << c.area() << std::endl; // 78.54
    std::cout << "Sliced shape area: " << s.area() << std::endl; // CRASH or wrong!
}
```

### Why Slicing Happens

When you assign a derived object to a base object by value, the compiler only knows about the base class members. It performs a member-wise copy of the base part, ignoring derived members.

### How to Avoid Slicing

1. **Use pointers/references**: `Shape* s = &c;` or `Shape& s = c;`
2. **Use smart pointers**: `std::unique_ptr<Shape> s = std::make_unique<Circle>(5);`
3. **Avoid copying polymorphic objects by value**

### Slicing in Function Calls

Slicing commonly occurs in function parameters:

```cpp
void processShape(Shape shape) { // BAD: pass by value
    std::cout << shape.area() << std::endl;
}

int main() {
    Circle c(5);
    processShape(c); // slices!
}
```

Fix: `void processShape(const Shape& shape)`

### Detecting Slicing

Slicing can be hard to detect because it compiles without warnings. Always use pointers/references for polymorphic parameters.

### Checkpoint: Slicing

1. Demonstrate slicing vs proper polymorphism.
2. Show how slicing breaks virtual function calls.
3. Fix by using references.

## Virtual function table

The virtual function table (vtable) is the mechanism that enables runtime polymorphism in C++. Understanding how it works helps you write more efficient and correct polymorphic code.

### How Vtables Work

1. **Vtable Creation**: For each class with virtual functions, the compiler creates a static array called the vtable. Each entry in the vtable is a pointer to a virtual function implementation.

2. **Vptr in Objects**: Every object of a polymorphic class contains a hidden pointer (vptr) that points to its class's vtable.

3. **Function Calls**: When you call a virtual function through a pointer/reference, the compiler:
   - Gets the vptr from the object
   - Looks up the function pointer in the vtable
   - Calls the function

### Vtable Layout Example

```cpp
class Base {
public:
    virtual void func1() {}
    virtual void func2() {}
};

class Derived : public Base {
public:
    void func1() override {} // overrides Base::func1
    void func2() override {} // overrides Base::func2
};
```

The vtables would look like:

- Base vtable: [Base::func1, Base::func2]
- Derived vtable: [Derived::func1, Derived::func2]

### Performance Implications

- **Space overhead**: One vptr per object, one vtable per class
- **Time overhead**: Extra indirection for virtual calls
- **Inlining**: Virtual functions cannot be inlined

### When Vtables Are Created

- Classes with virtual functions get vtables
- Pure virtual functions still create vtable entries (null or special)
- Multiple inheritance creates multiple vptrs

### Vtables and Optimization

Modern compilers can sometimes devirtualize calls when they can prove the exact type at compile time.

### Checkpoint: Vtable

1. Understand vtable mechanism.
2. Explain why virtual calls have overhead.
3. Discuss when polymorphism might not be worth the cost.

## Multiple inheritance and virtual

Multiple inheritance with virtual functions introduces complexity, especially with the diamond problem. When a class inherits from multiple base classes that have virtual functions, you need to handle potential ambiguities.

### The Diamond Problem

```cpp
class A {
public:
    virtual void f() { std::cout << "A::f" << std::endl; }
};

class B : public A {
public:
    void f() override { std::cout << "B::f" << std::endl; }
};

class C : public A {
public:
    void f() override { std::cout << "C::f" << std::endl; }
};

class D : public B, public C {
public:
    // Which f()? Ambiguous!
};
```

D inherits two versions of A, creating ambiguity.

### Virtual Inheritance Solution

Use virtual inheritance to create a diamond hierarchy:

```cpp
class A {
public:
    virtual void f() { std::cout << "A::f" << std::endl; }
};

class B : virtual public A {
public:
    void f() override { std::cout << "B::f" << std::endl; }
};

class C : virtual public A {
public:
    void f() override { std::cout << "C::f" << std::endl; }
};

class D : public B, public C {
public:
    void f() override { std::cout << "D::f" << std::endl; }
};
```

Now there's only one copy of A, and D can override f() unambiguously.

### Multiple Vtables

In multiple inheritance, objects may have multiple vptrs - one for each base class with virtual functions.

### Best Practices

- Avoid multiple inheritance when possible
- Use virtual inheritance sparingly
- Prefer composition over multiple inheritance
- Use interfaces (abstract classes) instead of concrete multiple inheritance

### Checkpoint: Multiple

1. Resolve ambiguity in multiple inheritance.
2. Implement virtual inheritance to solve diamond problem.
3. Show how multiple vptrs work.

## Mini project: Shape calculator

Create shapes, calculate total area polymorphically.

1. Abstract `Shape` with `area()`.
2. `Circle`, `Rectangle` implementations.
3. Vector of shapes, sum areas.

### Success criteria

- Polymorphic calls.
- Correct area calculations.
- No slicing.

## Guided practice challenges

1. Implement a `Sorter` with virtual compare.
2. Create a `Plugin` interface for extensions.
3. Build a `Command` hierarchy for undo/redo.
4. Add RTTI with `typeid`.
5. Use `dynamic_cast` safely.

## Self-check questions

1. What is polymorphism?
2. Difference between virtual and pure virtual?
3. Why virtual destructors?
4. What is object slicing?
5. How does vtable work?

## Recap and next steps

Polymorphism enables flexible code. Next, explore templates.
