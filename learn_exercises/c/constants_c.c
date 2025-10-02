#include <stdio.h>

#define CAMPUS "TSI Riga"
#define PI 3.1416

int main(void) {
    // TODO: declare const int MAX_SEATS with a value of 32
    const int MAX_SEATS = 32;
    // TODO: print the constants exactly in this order and format using printf:
    //   Campus: TSI Riga
    //   Max Seats: 32
    //   Pi Value: 3.1416
    // Hint: use %s for CAMPUS, %d for MAX_SEATS, and %.4f for PI to match the expected output.
    printf("Campus: %s\n", CAMPUS);
    printf("Max Seats: %d\n", MAX_SEATS);
    printf("Pi Value: %.4f\n", PI);
    return 0;
}
