# Writing and Reusing Functions

Functions package logic into reusable blocks. This lesson explains how to define, call, and return values from functions so you can reuse behaviour confidently.

## Function anatomy

A function definition specifies a return type, a name, a parameter list, and a body:

```cpp
int doubleNumber(int value) {
    return value * 2;
}
```

Call it with parentheses and supply arguments in the same order as the parameters.

## Return values

A function can return a single value via the `return` statement. Execution stops once `return` runs.

- Use `void` when you only need side effects, such as printing.
- Use descriptive types (`int`, `double`, `std::string`, etc.) when you need to send data back to the caller.

## Pass-by-value

By default, C++ copies arguments into parameters. Modifying the parameter does not change the original argument unless you pass by reference (`int&`) or pointer. For small types, pass-by-value is cheap and clear.

## Declaring before defining

If you define a function **after** `main`, provide a declaration (prototype) beforehand so the compiler knows the signature:

```cpp
int doubleNumber(int value); // declaration

int main() {
    std::cout << doubleNumber(5);
}

int doubleNumber(int value) { // definition
    return value * 2;
}
```

## Practice Time

Try the following before moving on:

1. Write a function `int triple(int value)` that returns three times its argument.
2. Write a function `double average(double first, double second)` that returns their mean.
3. Call both functions from `main`, storing the results in variables.
4. Print the outputs with clear labels so you can verify the results quickly.

When you are ready, open the exercise to reinforce function declarations and calls.
