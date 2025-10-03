# Modeling Data with Structs

Structs let you bundle related fields under one name so values travel together. In modern C++, structs and classes are nearly identical—the default member access (`public` versus `private`) is the main difference.

## Defining a struct

Declare a struct with its member variables inside braces and terminate with a semicolon:

```cpp
struct City {
    std::string name;
    double highTemp;
};
```

You can create instances with brace initialisation:

```cpp
City riga{"Riga", 12.5};
City tallinn{"Tallinn", 10.1};
```

## Working with structs

Members are accessed with the dot operator. Combine structs with standard containers to model lists of richer records.

```cpp
std::vector<City> tour{
    {"Riga", 12.5},
    {"Tallinn", 10.1}
};

for (const City& stop : tour) {
    std::cout << stop.name << " -> " << stop.highTemp << '\n';
}
```

## Formatting and helper functions

Encapsulate presentation logic in helper functions so the rest of your code remains declarative:

```cpp
void print_city(const City& city) {
    std::cout << city.name << " -> " << city.highTemp << "°C\n";
}
```

Returning structs from functions and accepting them by reference keeps your code easy to test.

## Practice Time

1. Define a `struct` that stores a city's name and daily high temperature.
2. Read data for two cities from standard input and store each in a struct instance.
3. Print each city on its own line in the format `Name -> temperature°C` with one decimal place.
4. Compute and print the average temperature for the two cities.

Once you're happy with the approach, open the exercise to implement the full workflow.
