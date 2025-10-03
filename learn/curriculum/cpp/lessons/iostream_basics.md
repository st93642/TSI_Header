# Streams and the iostream Library

C++ leans heavily on the iostream library for console input and output. This lesson distills the essentials you need to confidently send text to the terminal and collect user responses.

## Writing with `std::cout`

`std::cout` is the standard character output stream. You send values to it with the insertion operator `<<`, and you can chain multiple insertions in a single expression.

```cpp
std::cout << "The answer is " << 42 << '\n';
```

Chaining avoids building temporary strings and mirrors the order you want the text to appear.

## Ending lines

Two common ways to end a line are the newline character `\n` and `std::endl`.

- `\n` is fast and leaves the buffer unflushed.
- `std::endl` inserts a newline **and** flushes the buffer, which is helpful when you need immediate output.

Use `std::endl` sparingly—flushing too often can slow programs down.

## Reading with `std::cin`

`std::cin` reads values from standard input using the extraction operator `>>`. It skips leading whitespace and stops reading when it encounters whitespace that does not belong to the requested type.

```cpp
std::string name;
int age{};

std::cout << "Enter your name:\n";
std::cin >> name;

std::cout << "Enter your age:\n";
std::cin >> age;
```

If you need to capture whole lines (including spaces), use `std::getline` after clearing any leftover newline characters from previous extractions.

## Handling errors

When a read fails (for example, the user types a letter when you expect a number) the stream enters a failed state. Clear it with `std::cin.clear()` and discard the bad input using `std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');`.

## Practice Time

Try the following before moving on:

1. Prompt the user for their first name using `std::cout`, then read it with `std::cin`.
2. Ask for their age and store it in an `int`.
3. Print `Hello, {name}! You are {age} years old.` on a new line, matching the punctuation exactly.
4. Finish by printing `Ready to explore iostream!` with `std::endl` so the line flushes immediately.

When you are ready, open the exercise to reinforce stream insertion and extraction.
