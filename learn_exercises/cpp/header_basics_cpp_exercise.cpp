#include <iostream>
#include <iomanip>

// math_conversions.h (conceptual header)
double fahrenheit_to_celsius(double fahrenheit);
double kilometers_to_meters(double kilometers);

int main() {
    std::cout << std::fixed << std::setprecision(1);

    std::cout << "68°F in Celsius: " << fahrenheit_to_celsius(68.0) << '\n';
    std::cout << "3.5 km in meters: " << kilometers_to_meters(3.5);

    return 0;
}

// math_conversions.cpp (definitions)
double fahrenheit_to_celsius(double fahrenheit) {
    // TODO: Convert Fahrenheit to Celsius using (fahrenheit - 32.0) * 5.0 / 9.0

    return 0.0;
}

double kilometers_to_meters(double kilometers) {
    // TODO: Convert kilometres to metres by multiplying by 1000.0
    return 0.0;
}
