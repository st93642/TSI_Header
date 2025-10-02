#include <iostream>
#include <string>
#include <string_view>
#include <iomanip>

int main() {
    // TODO: declare constexpr/const values with these exact contents:
    constexpr std::string_view CAMPUS = "TSI Riga";
    const int MAX_SEATS = 32;
    const double PI = 3.1416;
    //   campus (std::string_view): "TSI Riga"
    //   maxSeats (int): 32
    //   pi (double): 3.1416
    // TODO: print the summary exactly as below using std::cout and std::endl:
    //   Campus: TSI Riga
    //   Max Seats: 32
    //   Pi Value: 3.1416
    // Apply std::fixed and std::setprecision(4) before printing pi.
    std::cout << "Campus: " << CAMPUS << std::endl;
    std::cout << "Max Seats: " << MAX_SEATS << std::endl;
    std::cout << std::fixed << std::setprecision(4);
    std::cout << "Pi Value: " << PI << std::endl;
    return 0;
}
