# Variables and Type Basics

Before you can store answers in a program you need variables. This lesson focuses on the fundamental types, declarations, and initialisation styles you will use most often.

## Declaring variables

A declaration introduces a name and associates it with a type. In C++, declarations usually look like this:

```cpp
int score;      // declaration without initial value
int lives{3};   // declaration with brace initialisation
```

Prefer giving every variable an initial value so it never holds garbage data.

## Fundamental types

- `int` stores whole numbers (positive, negative, or zero)
- `double` stores floating-point values with fractional parts
- `char` stores individual characters
- `bool` stores `true` or `false`

Use `std::string` from the &lt;string&gt; header when you need text.

## Initialisation styles

C++ offers several syntaxes:

- Copy initialisation: `int width = 640;`
- Direct initialisation: `int height(480);`
- Brace initialisation: `int depth{24};`

Brace initialisation prevents narrowing conversions, making it the safest default.

## Type inference with `auto`

`auto` lets the compiler deduce a type from the initialiser:

```cpp
auto ratio = 0.618;     // deduced as double
auto name = std::string{"Ada"};
```

Use `auto` when the deduced type is obvious from the right-hand side; otherwise be explicit.

## Practice Time

Try the following before moving on:

1. Declare and initialise an `int` for the number of students in your cohort.
2. Declare a `double` storing the average grade and a `char` holding the highest grade symbol.
3. Use `auto` to capture whether the lab is open as a `bool` literal.
4. Print a summary message that includes all the values with clear labels.

When you are ready, open the exercise to reinforce declarations, initialisation, and formatted output.
