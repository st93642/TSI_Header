#include <stdio.h>

int main(void)
{
    const int TOTAL_CREDITS = 180;
    const int CREDITS_PER_SEMESTER = 30;
    const double TUITION_PER_CREDIT = 42.50;

    int completedCredits = 0;
    int weeklyMinutes = 0;

    printf("Enter completed credits:\n");
    // TODO: Read completedCredits with scanf.

    printf("Enter weekly study minutes:\n");
    // TODO: Read weeklyMinutes with scanf.

    int remainingCredits = 0;
    int semestersRounded = 0;
    double semestersExact = 0.0;
    double weeklyHours = 0.0;
    double tuitionRemaining = 0.0;

    // TODO: Compute remainingCredits safely (no negative results).
    // TODO: Calculate semestersRounded (ceiling) and semestersExact (double precision).
    // TODO: Convert weeklyMinutes to weeklyHours with one decimal of precision.
    // TODO: Calculate tuitionRemaining using TUITION_PER_CREDIT.

    printf("Completed credits: %d\n", completedCredits);
    printf("Remaining credits: %d\n", remainingCredits);
    printf("Semesters remaining (rounded): %d\n", semestersRounded);
    printf("Semesters remaining (exact): %.1f\n", semestersExact);
    printf("Weekly study hours: %.1f\n", weeklyHours);
    printf("Tuition remaining: %.2f EUR\n", tuitionRemaining);

    // TODO: Print "Study load warning: Reduce load" when weeklyHours > 40.0, otherwise "Study load warning: OK".

    return 0;
}
