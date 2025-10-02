// Chapter 5 Arrays and Loops - Exercise Stub
// Follow the offline blueprint: implement the required functions, then
// print the verification lines so the automated tests can compare output.

#include <iostream>
#include <vector>
#include <array>

// Exercise 5.1: Array Basics
// Create an array of 5 integers and return their sum
int sum_array() {
    // TODO: Declare an array with values {1, 2, 3, 4, 5}
    // TODO: Use std::size (or a constant) to iterate safely over the array
    return 0;
}

// Exercise 5.2: For Loop Sum
// Use a for loop to sum numbers from 1 to n
int sum_to_n(int n) {
    // TODO: Use a classic for loop that accumulates values from 1 through n
    return 0;
}

// Exercise 5.3: While Loop Input
// Read integers until 0 is entered, return count of positive numbers
int count_positives() {
    // TODO: Prompt for numbers until 0, counting strictly positive entries
    return 0;
}

// Exercise 5.4: Range-Based Loop
// Double all values in the vector using range-based loop
void double_values(std::vector<int>& numbers) {
    // TODO: Use a range-based for loop by reference to multiply each value by 2
    (void)numbers; // remove after implementation
}

// Exercise 5.5: Nested Loops - Multiplication Table
// Fill the 2D array with multiplication table (1-10)
void fill_multiplication_table(int table[10][10]) {
    // TODO: Use nested loops so table[i][j] == (i + 1) * (j + 1)
    (void)table; // remove after implementation
}

// Exercise 5.6: Vector Operations
// Add numbers to vector until sum exceeds 100, return final sum
int sum_until_100() {
    // TODO: Store numbers in a std::vector and stop when the running sum > 100
    return 0;
}

// Exercise 5.7: Loop Control - Skip Negatives
// Sum only positive numbers, skip negatives using continue
int sum_positives_only(const std::vector<int>& numbers) {
    // TODO: Iterate with a loop and continue to skip negatives
    (void)numbers; // remove after implementation
    return 0;
}

int main() {
    // Output blueprint (follow exactly once the functions are implemented):
    // Array sum: <value>
    // Sum to 5: <value>
    // Sum to 10: <value>
    // Doubled values: <a> <b> <c>
    // Multiplication table: <table[0][0]> <table[2][3]> <table[9][9]>
    // Positive sum: <value>

    std::cout << "Array sum: " << sum_array() << std::endl;
    std::cout << "Sum to 5: " << sum_to_n(5) << std::endl;
    std::cout << "Sum to 10: " << sum_to_n(10) << std::endl;

    std::vector<int> test_vec = {1, 2, 3};
    double_values(test_vec);
    std::cout << "Doubled values: "
              << test_vec[0] << " " << test_vec[1] << " " << test_vec[2] << std::endl;

    int table[10][10] = {};
    fill_multiplication_table(table);
    std::cout << "Multiplication table: "
              << table[0][0] << " " << table[2][3] << " " << table[9][9] << std::endl;

    std::vector<int> test_data = {1, -2, 3, -4, 5};
    std::cout << "Positive sum: " << sum_positives_only(test_data) << std::endl;

    return 0;
}