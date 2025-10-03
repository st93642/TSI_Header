# Standard Library in C

C's standard library provides essential functions for memory management, string manipulation, math operations, and more. This lesson explores key headers like stdlib.h, string.h, math.h, ctype.h, and time.h. You'll learn to use library functions effectively and safely.

## Learning goals

- Use stdlib.h for memory allocation, random numbers, conversions.
- Manipulate strings with string.h functions.
- Perform math operations with math.h.
- Classify and convert characters with ctype.h.
- Handle dates and times with time.h.
- Understand library safety and best practices.

## 1. stdlib.h essentials

Core utilities for C programs.

```c
#include <stdlib.h>

int main(void) {
    // Memory allocation
    int *arr = malloc(10 * sizeof(int));
    if (arr == NULL) return 1;
    
    // Random numbers
    srand(time(NULL));
    int random = rand() % 100;
    
    // String to number
    char *str = "123";
    int num = atoi(str);
    
    free(arr);
    return 0;
}
```

### Checkpoint: malloc and free

1. Allocate array of 5 ints, initialize, free.

## 2. String manipulation with string.h

Powerful string functions.

```c
#include <string.h>

int main(void) {
    char str1[20] = "Hello";
    char str2[] = " World";
    
    // Concatenate
    strcat(str1, str2); // "Hello World"
    
    // Copy
    char dest[20];
    strcpy(dest, str1);
    
    // Length
    size_t len = strlen(str1);
    
    // Compare
    if (strcmp(str1, dest) == 0) {
        // equal
    }
}
```

### Checkpoint: strcpy and strcmp

1. Copy string, compare originals.

## 3. Math operations with math.h

Mathematical functions.

```c
#include <math.h>

int main(void) {
    double x = 2.0;
    
    // Power
    double sq = pow(x, 2);
    
    // Square root
    double root = sqrt(x);
    
    // Trigonometry
    double sin_val = sin(M_PI / 2);
    
    // Rounding
    double rounded = round(3.7);
}
```

### Checkpoint: pow and sqrt

1. Calculate hypotenuse using sqrt and pow.

## 4. Character classification with ctype.h

Test and convert characters.

```c
#include <ctype.h>

int main(void) {
    char c = 'a';
    
    if (isalpha(c)) {
        // letter
    }
    
    if (isdigit(c)) {
        // digit
    }
    
    // Convert case
    char upper = toupper(c);
    char lower = tolower('A');
}
```

### Checkpoint: isalpha and toupper

1. Count letters in string, convert to uppercase.

## 5. Time handling with time.h

Work with dates and times.

```c
#include <time.h>

int main(void) {
    time_t now = time(NULL);
    
    // Format time
    char buffer[26];
    ctime_r(&now, buffer);
    
    // Structured time
    struct tm *tm_info = localtime(&now);
    printf("%d-%02d-%02d\n", 
           tm_info->tm_year + 1900,
           tm_info->tm_mon + 1,
           tm_info->tm_mday);
}
```

### Checkpoint: time and ctime

1. Print current date and time.

## 6. Safe string functions

Avoid buffer overflows.

```c
// Instead of strcpy
strncpy(dest, src, sizeof(dest) - 1);
dest[sizeof(dest) - 1] = '\0';

// Instead of sprintf
snprintf(buffer, sizeof(buffer), "%s %d", str, num);
```

### Checkpoint: strncpy

1. Safely copy string with strncpy.

## 7. Memory management best practices

Prevent leaks and corruption.

```c
// Always check malloc
int *ptr = malloc(size);
if (ptr == NULL) {
    // handle error
}

// Use calloc for zero-initialized
int *arr = calloc(n, sizeof(int));

// Realloc for resizing
ptr = realloc(ptr, new_size);
if (ptr == NULL) {
    // handle error
}

// Free when done
free(ptr);
ptr = NULL;
```

### Checkpoint: calloc and realloc

1. Allocate, resize, free array.

## 8. Random number generation

Proper seeding and usage.

```c
#include <stdlib.h>
#include <time.h>

int random_int(int min, int max) {
    return min + rand() % (max - min + 1);
}

int main(void) {
    srand((unsigned)time(NULL));
    int r = random_int(1, 10);
}
```

### Checkpoint: srand and rand

1. Generate 10 random numbers between 1-100.

## 9. String tokenization

Split strings with strtok.

```c
#include <string.h>

int main(void) {
    char str[] = "hello,world,test";
    char *token = strtok(str, ",");
    
    while (token != NULL) {
        printf("%s\n", token);
        token = strtok(NULL, ",");
    }
}
```

### Checkpoint: strtok

1. Tokenize comma-separated string.

## 10. Math constants and functions

Common math utilities.

```c
#include <math.h>

#define PI 3.141592653589793

int main(void) {
    // Absolute value
    int abs_val = abs(-5);
    
    // Floating point abs
    double fabs_val = fabs(-3.14);
    
    // Ceiling and floor
    double ceil_val = ceil(2.3);
    double floor_val = floor(2.7);
}
```

### Checkpoint: ceil and floor

1. Round numbers up and down.

## 11. Mini project: Simple calculator with history

Calculator that logs operations.

1. Read expressions from input.
2. Support +, -, *, /, sin, cos, sqrt.
3. Log each operation with timestamp.
4. Handle errors gracefully.

### Success criteria

- Supports multiple operations.
- Logs with timestamps.
- Handles invalid input.

## 12. Guided practice challenges

1. Implement string reversal using library functions.
2. Create a simple text-based calendar.
3. Build a random password generator.
4. Develop a basic file statistics tool.
5. Write a program to sort strings alphabetically.

## 13. Self-check questions

1. Difference between malloc and calloc?
2. How to safely copy strings?
3. What does srand do?
4. How to get current time?
5. When to use realloc?

## Recap and next steps

The standard library is powerful. Next, explore data structures.
