# Lesson 5.4: Polymorphism

Polymorphism enables C++ code to treat related objects uniformly while letting each derived class supply specialized behavior. This offline guide covers virtual functions, overriding, abstract interfaces, and safe resource management for polymorphic hierarchies.

## Learning Goals

- Explain static vs. dynamic polymorphism and when each applies.
- Declare virtual functions and override them safely with the `override` keyword.
- Create abstract base classes with pure virtual functions.
- Manage polymorphic objects with smart pointers to ensure proper destruction.
- Build, run, and debug polymorphic code locally without external references.

## 1. Static vs. Dynamic Polymorphism

| Technique | Compile-time? | Example | Use when |
|-----------|---------------|---------|----------|
| Function overloading | ✔ | `print(int)`, `print(std::string)` | Same operation for different parameter types |
| Templates | ✔ | `std::vector<T>` | Generic algorithms over many types |
| Virtual functions | ✖ (runtime dispatch) | `virtual std::string summary() const` | Behavior varies by derived type at runtime |

This lesson focuses on **dynamic polymorphism** using virtual functions.

## 2. Virtual Functions and Overrides

```cpp
class Report {
public:
    virtual ~Report() = default;
    virtual std::string summary() const {
        return "Generic report";
    }
};

class AttendanceReport : public Report {
public:
    std::string summary() const override {
        return "Attendance breakdown";
    }
};
```

- The base function marked `virtual` enables runtime dispatch.
- `override` tells the compiler you intend to override a base virtual function—mismatches trigger errors.
- The virtual destructor ensures derived destructors run when deleting through a base pointer.

## 3. Pure Virtual Functions and Abstract Classes

```cpp
class LearningResource {
public:
    virtual ~LearningResource() = default;
    virtual std::string summary() const = 0; // pure virtual
};
```

A class with at least one pure virtual function is **abstract** and cannot be instantiated directly. Derived classes must implement the pure virtual functions.

## 4. Polymorphic Collections with Smart Pointers

```cpp
std::vector<std::unique_ptr<LearningResource>> resources;
resources.push_back(std::make_unique<Article>("Intro", 10));
resources.push_back(std::make_unique<Video>("Walkthrough", 18, "Maya"));

for (const auto& resource : resources) {
    std::cout << resource->summary() << '\n';
}
```

Use `std::unique_ptr` (or `std::shared_ptr` if shared ownership is necessary) to manage polymorphic objects automatically. Avoid raw `new`/`delete` to reduce memory leaks and ensure destructors run.

## 5. Virtual Dispatch Cost

Virtual calls add a small runtime cost (an indirect jump through the vtable). For most applications the clarity outweighs the cost. If profiling shows hot spots, consider static polymorphism (templates) or alternative designs.

## 6. Slicing Pitfalls

Assigning a derived object to a base-class variable **slices** the derived portion:

```cpp
Video video{"Walkthrough", 18, "Maya"};
LearningResource base = video; // slicing: base keeps only the LearningResource part
```

Avoid slicing by storing polymorphic objects through pointers or references.

## 7. Downcasting Safely

If you must convert from a base pointer to a derived pointer, use `dynamic_cast` and test the result:

```cpp
if (auto* videoPtr = dynamic_cast<Video*>(resource.get())) {
    videoPtr->enableCaptions();
}
```

Prefer virtual member functions over downcasts—extending the interface with a virtual function keeps the design cleaner.

## 8. Building and Running Offline

```bash
# Compile a polymorphism example with smart pointers
 g++ -std=c++17 -Wall -Wextra polymorphism_demo.cpp -o polymorphism_demo
# Run the executable
 ./polymorphism_demo
```

If you see missing destructor output, ensure the base class has a virtual destructor and that you are using smart pointers or deleting through the base pointer.

## 9. Troubleshooting Checklist

- **“undefined reference to vtable”**: Provide definitions for all pure virtual functions and ensure the translation unit containing the class definition is compiled.
- **Incorrect override**: Add `override` to derived functions and match the exact signature of the base function.
- **Memory leak**: Always delete through a base pointer with a virtual destructor or use `std::unique_ptr`.
- **Unexpected slicing**: Store objects via pointers or references; avoid copying polymorphic objects by value.

## 10. Practice Ideas

1. Model a base `Assignment` class with derived `CodingAssignment`, `EssayAssignment`, and `PresentationAssignment` classes, each overriding `summary()`.
2. Create an `AudioProcessor` hierarchy with virtual `process()` methods that vary per effect.
3. Build a plugin system where the base class defines a `run()` method and derived classes implement specific tasks.

The module exercise asks you to implement polymorphic learning resources, store them in a smart-pointer collection, and compute total study time while printing specialized summaries for each resource.
