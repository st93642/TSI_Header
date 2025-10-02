#include <iostream>
#include <string>

int main() {
    // TODO: declare the course summary with these exact values:
    std::string course_name = "TSI C++ Essentials";
    int seats = 24;
    double pass_rate = 92.3;
    bool has_lab = true;
    //   Course: TSI C++ Essentials (std::string)
    //   Seats: 24 (int)
    //   Pass Rate: 92.3 (double)
    //   Has Lab: true (bool)
    // You will convert the bool to the words "yes" or "no" when printing.
    std::cout << "Course: " << course_name << std::endl;
    std::cout << "Seats: " << seats << std::endl;
    std::cout << "Pass Rate: " << pass_rate << std::endl;
    std::cout << "Has Lab: " << (has_lab ? "yes" : "no") << std::endl;
    // TODO: print each line exactly as follows using std::cout:
    // Course: TSI C++ Essentials
    // Seats: 24
    // Pass Rate: 92.3
    // Has Lab: yes
    // Remember to keep the same capitalization and punctuation.
    return 0;
}
