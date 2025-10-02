#include <stdio.h>
#include <stdbool.h>

int main(void) {
    // TODO: declare variables for the number of students, average score, top grade, and accreditation status

    int students = 28;
    double averageScore = 91.5;
    char topGrade = 'A';
    bool accredited = true;

    printf("Students: %d\n", students);
    printf("Average Score: %.1f\n", averageScore);
    printf("Top Grade: %c\n", topGrade);
    printf("Accredited: %s\n", accredited ? "true" : "false");
    return 0;
}
