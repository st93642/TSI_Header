#include <iostream>
#include <iomanip>
#include <limits>

// Offline tutorial standard:
//   * Produce the fact sheet exactly as listed in Chapter 2 using std::numeric_limits for ranges.
//   * Declare the constants seats=24, tablets=32u, passRate=92.3, and initial='R' before printing.
//   * Stream labels left-aligned with std::setw(18) to mirror the layout from the exercise description.
// Output blueprint:
//   Type Report: C++ Fundamentals
//   Seats (int): 24 (bytes: 4, min: -2147483648, max: 2147483647)
//   Tablets (unsigned): 32 (bytes: 4, min: 0, max: 4294967295)
//   Pass Rate (double): 92.3 (bytes: 8, min: -1.79769e+308, max: 1.79769e+308)
//   Initial (char): R (bytes: 1, min: 0, max: 127)

int main() {
    // TODO: set std::cout to std::left for aligned labels.
    // TODO: declare the four constants with the exact values shown above.
    // TODO: use sizeof and std::numeric_limits<T>::min()/max()/lowest() to populate each line.
    // TODO: print the four lines so they match the blueprint exactly.
    return 0;
}
