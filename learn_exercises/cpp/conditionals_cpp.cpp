#include <iostream>

int main() {
    // TODO: read exactly one integer score from std::cin
    int score;
    std::cin >> score;
    // TODO: output exactly these two lines using std::cout and std::endl:
    //   Score: <original value>
    //   Result: <classification text>
    std::cout << "Score: " << score << std::endl;
    // TODO: choose the classification with an if/else-if/else chain:
    //   Excellent  -> score between 90 and 100 inclusive
    //   Good       -> score between 75 and 89 inclusive
    //   Pass       -> score between 60 and 74 inclusive
    //   Retake     -> score between 0 and 59 inclusive
    //   Invalid score -> any value outside 0-100 (print this text exactly)
    if (score >= 90 && score <= 100) {
        std::cout << "Result: Excellent" << std::endl;
    } else if (score >= 75 && score <= 89) {
        std::cout << "Result: Good" << std::endl;
    } else if (score >= 60 && score <= 74) {
        std::cout << "Result: Pass" << std::endl;
    } else if (score >= 0 && score <= 59) {
        std::cout << "Result: Retake" << std::endl;
    } else {
        std::cout << "Result: Invalid score" << std::endl;
    }
    return 0;
}
