#include <stdio.h>

// Output blueprint (follow exactly):
//   Hello World
//   Name: TSI Student
//   Age: 21
// Each line must end with a single newline and there should be no extra text.
// Steps inspired by the textbook exercises:
//   1. Re-create the classic Hello World to verify the toolchain (Exercise 1-1).
//   2. Add your name and age on separate lines using the blueprint values (Exercise 1-2).
//   3. Ensure the final program compiles cleanly—fixing any typos mirrors Exercise 1-3.
/* Broken snippet from the chapter for reference (do not copy as-is):
   include <stdio.h>
   Int main()
   {
       printf("Hello World"
   );
   )
*/

int main(void) {
    // TODO: Use three printf calls that match the blueprint above exactly.

    printf("Hello World\n");
    printf("Name: TSI Student\n");
    printf("Age: 21\n");
    // TODO: Maintain the capitalization and punctuation shown in the blueprint.
    return 0;
}
