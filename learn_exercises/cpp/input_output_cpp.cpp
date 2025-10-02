#include <iostream>

int main() {
    // TODO: read two integers from std::cin (first, then second)
    int first, second;
    std::cin >> first >> second;
    // TODO: calculate sum = first + second, difference = first - second, product = first * second
    // TODO: output exactly these lines using std::cout and std::endl:
    std::cout << "Sum: " << (first + second) << std::endl;
    std::cout << "Difference: " << (first - second) << std::endl;
    std::cout << "Product: " << (first * second) << std::endl;
    // Example input:
    //   Sum: <value>
    //   Difference: <value>
    //   Product: <value>
    return 0;
}
