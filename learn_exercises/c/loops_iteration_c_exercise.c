#include <stdio.h>

static const int DAYS_PER_WEEK = 7;

int main(void)
{
    int weeks = 0;

    printf("Enter number of weeks (1-4):\n");
    if (scanf("%d", &weeks) != 1 || weeks < 1 || weeks > 4)
    {
        printf("Invalid number of weeks.\n");
        return 1;
    }

    int topWeekIndex = 0;        /* store index of the best week (0-based) */
    int topWeekTotal = -1;       /* best total minutes seen so far */
    int overallMinutes = 0;

    for (int week = 0; week < weeks; ++week)
    {
        int weekTotal = 0;
        int weekPeakDay = 0;
        int weekPeakMinutes = -1;

        for (int day = 0; day < DAYS_PER_WEEK; ++day)
        {
            int minutes = 0;
            printf("Enter minutes for week %d day %d:\n", week + 1, day + 1);

            // TODO: Read minutes, guard against scanf failure or negative values.

            // TODO: Add minutes to weekTotal and overallMinutes.

            // TODO: Update weekPeakDay and weekPeakMinutes when a new maximum appears.
            //       Remember to keep the earlier day when values tie.
        }

        // TODO: Compute the weekly average as a double (weekTotal / 7.0).
        // TODO: Print the week summary line in the required format.

        // TODO: Update topWeekIndex and topWeekTotal if this week beats the previous best.
    }

    // TODO: After all weeks, compute overall average per day (overallMinutes / (weeks * 7.0)).
    // TODO: Print the summary lines for the top week and overall average.

    return 0;
}
