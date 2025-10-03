# Modeling Data with Structs

Structs let you bundle related fields under one name so values travel together. In modern C++, structs and classes are nearly identical—the only difference is the default member access (`public` for structs, `private` for classes). This lesson explores defining structs, supplying behaviour, and integrating them with the Standard Library.

## Learning goals

- Define structs with member variables, default initialisers, and helper functions.
- Understand aggregate initialisation and when constructors are necessary.
- Pass structs by value or reference without unnecessary copies.
- Combine structs with containers and algorithms for richer data models.
- Organise struct declarations in headers and keep implementation details in source files when needed.

## Defining a struct

Declare a struct with members inside braces and terminate with a semicolon:

```cpp
struct City {
    std::string name;
    double highTempCelsius{}; // default-initialised to 0.0
};
```

- Default member initialisers provide fallback values when fields are omitted.
- Because access is public, callers can reach `name` and `highTempCelsius` directly.

## Aggregate initialisation

Brace initialisation assigns members in declaration order:

```cpp
City riga{"Riga", 12.5};
City tallinn{"Tallinn", 10.1};
City defaultCity{}; // name empty, temperature 0.0
```

With C++20 designated initialisers, you can name the members:

```cpp
City vilnius{.name = "Vilnius", .highTempCelsius = 11.3};
```

## Adding behaviour with member functions

Structs can define functions, just like classes.

```cpp
struct City {
    std::string name;
    double highTempCelsius{};

    void print() const {
        std::cout << name << " -> " << highTempCelsius << "°C\n";
    }
};
```

- Mark member functions `const` when they do not modify fields.
- Use helper functions to keep presentation or calculations near the data they operate on.

## Passing structs around

- Pass small structs by value when copying is cheap and intentional.
- Pass by `const` reference to avoid copies for larger structs or when you only need read access.
- Mutate via non-const reference when updates should affect the caller’s copy.

```cpp
double averageHigh(const City& first, const City& second) {
    return (first.highTempCelsius + second.highTempCelsius) / 2.0;
}
```

## Structs with containers and algorithms

Combine structs with `std::vector`, `std::map`, and algorithms to model complex data sets.

```cpp
std::vector<City> tour{{"Riga", 12.5}, {"Tallinn", 10.1}, {"Vilnius", 11.3}};

std::sort(tour.begin(), tour.end(), [](const City& a, const City& b) {
    return a.highTempCelsius > b.highTempCelsius; // warmest first
});

for (const City& stop : tour) {
    std::cout << stop.name << " -> " << stop.highTempCelsius << "°C\n";
}
```

Use structured bindings to unpack fields during iteration:

```cpp
for (const auto& [name, temperature] : tour) {
    std::cout << name << " -> " << temperature << "°C\n";
}
```

## Constructors and invariants

When default initialisation is insufficient, add constructors to enforce invariants:

```cpp
#include <stdexcept>

struct Measurement {
    std::string unit;
    double value{};

    Measurement(std::string unit, double value)
        : unit(std::move(unit)), value(value) {
        if (value < 0.0) {
            throw std::invalid_argument("Measurement cannot be negative");
        }
    }
};
```

Provide `explicit` constructors when a single argument should not be implicitly converted.

## Organising struct declarations

- Place struct declarations in headers when they are shared across translation units.
- Implement member functions in the corresponding `.cpp` when they become non-trivial.
- Document each field with a brief comment to explain its purpose, especially for public APIs.

## Practice time

1. **City climate report:** Define a struct with name, high, and low temperatures. Populate a vector, sort by high temperature, and print a formatted report.
2. **Student records:** Create a struct storing `name`, `creditsEarned`, and `gpa`. Write functions to compute academic standing and print alerts for students below a threshold.
3. **Inventory item:** Model SKU, description, price, and quantity. Add a member function that returns total value (`price * quantity`). Store items in a map keyed by SKU.
4. **Input builder:** Write a function that reads struct fields from `std::cin`, handling invalid input by re-prompting until valid data is provided.

## Self-check questions

1. How do structs differ from classes in C++? When might you prefer a class?
2. What is aggregate initialisation and how does it interact with constructors?
3. When should you pass a struct by value versus by reference?
4. Why mark member functions `const` when they do not modify fields?
5. How could you organise struct declarations and implementations across headers and source files in a multi-file project?

With structs under your belt, you can model richer data as you continue through the curriculum.
