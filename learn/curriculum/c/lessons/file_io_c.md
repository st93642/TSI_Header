# File Input and Output in C

File I/O enables programs to persist data beyond execution, reading from and writing to files. This lesson covers opening, reading, writing, and closing files using standard library functions, handling errors, and working with text and binary modes. You'll learn to process files line-by-line, manage buffers, and implement robust file operations that handle edge cases gracefully.

## Learning goals

- Open and close files with fopen and fclose.
- Read from files using fscanf, fgets, and fread.
- Write to files using fprintf, fputs, and fwrite.
- Handle file errors and check for end-of-file.
- Work with text and binary file modes.
- Implement file copying, processing, and parsing utilities.
- Manage file pointers and seek operations.

## 1. Opening and closing files

Use fopen to open files, fclose to close.

```c
#include <stdio.h>

int main(void)
{
    FILE *fp = fopen("example.txt", "r");
    if (fp == NULL)
    {
        perror("Error opening file");
        return 1;
    }

    // Use file

    fclose(fp);
    return 0;
}
```

Modes: "r" read, "w" write, "a" append, "rb" binary read, etc.

### Checkpoint: Open and close

1. Open a file for writing, write "Hello", close.

## 2. Writing to files

Use fprintf for formatted output.

```c
#include <stdio.h>

int main(void)
{
    FILE *fp = fopen("output.txt", "w");
    if (fp == NULL) return 1;

    fprintf(fp, "Name: %s, Age: %d\n", "Alice", 30);
    fputs("End of file\n", fp);

    fclose(fp);
    return 0;
}
```

### Checkpoint: Write data

1. Write numbers 1 to 10 to a file, one per line.

## 3. Reading from files

Use fscanf for formatted input.

```c
#include <stdio.h>

int main(void)
{
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) return 1;

    int num;
    while (fscanf(fp, "%d", &num) == 1)
    {
        printf("Read: %d\n", num);
    }

    fclose(fp);
    return 0;
}
```

### Checkpoint: Read numbers

1. Read integers from file, sum them, print sum.

## 4. Line-based I/O

fgets reads lines, fputs writes lines.

```c
#include <stdio.h>

int main(void)
{
    FILE *fp = fopen("lines.txt", "r");
    if (fp == NULL) return 1;

    char buffer[256];
    while (fgets(buffer, sizeof(buffer), fp))
    {
        printf("Line: %s", buffer);
    }

    fclose(fp);
    return 0;
}
```

### Checkpoint: Process lines

1. Read lines from file, count lines, print count.

## 5. Binary I/O

fread and fwrite for binary data.

```c
#include <stdio.h>

int main(void)
{
    int data[] = {1, 2, 3, 4, 5};
    FILE *fp = fopen("binary.dat", "wb");
    if (fp == NULL) return 1;

    fwrite(data, sizeof(int), 5, fp);
    fclose(fp);

    // Read back
    fp = fopen("binary.dat", "rb");
    int read_data[5];
    fread(read_data, sizeof(int), 5, fp);
    fclose(fp);

    return 0;
}
```

### Checkpoint: Binary write/read

1. Write array of floats to binary file, read back, verify.

## 6. Error handling

Check return values, use feof and ferror.

```c
#include <stdio.h>

int main(void)
{
    FILE *fp = fopen("nonexistent.txt", "r");
    if (fp == NULL)
    {
        perror("fopen");
        return 1;
    }

    int ch;
    while ((ch = fgetc(fp)) != EOF)
    {
        putchar(ch);
    }

    if (ferror(fp))
    {
        perror("Read error");
    }

    fclose(fp);
    return 0;
}
```

### Checkpoint: Handle errors

1. Try to open non-existent file, handle error.

## 7. File positioning

fseek, ftell, rewind.

```c
#include <stdio.h>

int main(void)
{
    FILE *fp = fopen("seek.txt", "r");
    if (fp == NULL) return 1;

    fseek(fp, 10, SEEK_SET); // Skip 10 bytes
    char ch = fgetc(fp);
    printf("Char at pos 10: %c\n", ch);

    fclose(fp);
    return 0;
}
```

### Checkpoint: Seek position

1. Seek to middle of file, read a char.

## 8. File copying

Implement a simple cp utility.

```c
#include <stdio.h>

int main(int argc, char *argv[])
{
    if (argc != 3) return 1;

    FILE *src = fopen(argv[1], "rb");
    FILE *dst = fopen(argv[2], "wb");
    if (!src || !dst) return 1;

    char buffer[1024];
    size_t bytes;
    while ((bytes = fread(buffer, 1, sizeof(buffer), src)) > 0)
    {
        fwrite(buffer, 1, bytes, dst);
    }

    fclose(src);
    fclose(dst);
    return 0;
}
```

### Checkpoint: Copy file

1. Copy text file using buffer.

## 9. Parsing files

Read structured data.

```c
#include <stdio.h>

int main(void)
{
    FILE *fp = fopen("data.txt", "r");
    if (fp == NULL) return 1;

    char name[50];
    int age;
    while (fscanf(fp, "%s %d", name, &age) == 2)
    {
        printf("Name: %s, Age: %d\n", name, &age);
    }

    fclose(fp);
    return 0;
}
```

### Checkpoint: Parse records

1. Read name and score pairs, find highest score.

## 10. Temporary files

tmpfile for temporary storage.

```c
#include <stdio.h>

int main(void)
{
    FILE *tmp = tmpfile();
    if (tmp == NULL) return 1;

    fprintf(tmp, "Temporary data\n");
    rewind(tmp);
    char buf[100];
    fgets(buf, sizeof(buf), tmp);
    printf("%s", buf);

    fclose(tmp); // Auto-deleted
    return 0;
}
```

### Checkpoint: Temp file

1. Write to temp file, read back.

## 11. Mini project: Word count

Implement wc-like utility.

1. Read file, count lines, words, chars.
2. Handle command line args.
3. Print results.

### Success criteria

- Handles text files.
- Accurate counts.
- Error handling.

## 12. Guided practice challenges

1. File reversal: reverse lines in file.
2. CSV parser: read comma-separated values.
3. Log merger: merge two sorted log files.
4. Binary search in file.
5. File compression simulation.

## 13. Self-check questions

1. Difference between text and binary modes?
2. How to detect EOF?
3. When to use fseek?
4. How to handle file errors?

## Recap and next steps

File I/O connects programs to persistent storage. Next, explore advanced topics like multi-file programs.
