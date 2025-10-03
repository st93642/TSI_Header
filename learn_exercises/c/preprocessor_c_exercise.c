#include <stdio.h>

/*
Macro-Based Calculator Exercise

Task: Define macros for arithmetic, use in calculator.

Steps:
1. #define ADD(a,b) ((a) + (b))
2. Similarly for SUB, MUL, DIV
3. In main, char op; int a,b; scanf("%c %d %d", &op, &a, &b)
4. switch(op) { case '+': printf("%d\n", ADD(a,b)); break; ... }
5. default: printf("Invalid\n");

Input: op a b
Expected Output: result
*/

// TODO: Define macros

int main(void)
{
    char op;
    int a, b;
    if (scanf("%c %d %d", &op, &a, &b) != 3)
    {
        printf("Invalid input\n");
        return 1;
    }

    // TODO: Use switch to compute and print result

    return 0;
}
