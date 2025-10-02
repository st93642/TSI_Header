#include <iostream>

// Output blueprint (follow exactly):
//   Hello World
//   Name: TSI Student
//   Age: 21
// Each line must end with a single newline and there should be no extra text.
// Chapter checkpoints:
//   1. Print "Hello World" with std::cout to verify your compile-link-run loop (Exercise 1-1).
//   2. Print "Name: TSI Student" and "Age: 21" on their own lines (Exercise 1-2).
//   3. Keep the code standards-compliant—fixing typos reflects Exercise 1-3's debugging practice.
// Broken snippet from the chapter for reference (do not copy as-is):
//   include <iostream>
//   Int main()
//   {
//       std:cout << "Hello World" << std:endl
//   )

int main() {
    // TODO: Use std::cout with explicit std:: qualification to print the three lines above.

    std::cout << "Hello World" << std::endl;
    std::cout << "Name: TSI Student" << std::endl;
    std::cout << "Age: 21" << std::endl;
    // TODO: Terminate each line with "\n" or std::endl; do not add extra spaces or text.
    return 0;
}
