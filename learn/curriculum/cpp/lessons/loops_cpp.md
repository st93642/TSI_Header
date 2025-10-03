# Looping with `for`, `while`, and `do-while`

Loops repeat work efficiently. This lesson explores the three primary loop constructs so you can pick the right one for each scenario.

## `while` loops

`while` checks the condition first and runs the body as long as it remains true.

```cpp
int countdown{5};
while (countdown > 0) {
    std::cout << countdown << '\n';
    --countdown;
}
```

Use `while` when you do not know in advance how many iterations you will need.

## `for` loops

`for` consolidates initialisation, condition, and increment in one line. They shine when you know the iteration count.

```cpp
for (int i{0}; i <= 5; ++i) {
    std::cout << i << '\n';
}
```

## `do-while`

`do-while` executes the body once before checking the condition, making it handy for menu loops that should run at least once.

```cpp
int choice{};

do {
    std::cout << "1. Start\n2. Quit\n";
    std::cin >> choice;
} while (choice != 2);
```

## Accumulators and running totals

Many loops build up a result by accumulating values. Start with an initial value and update the total inside the loop.

```cpp
int limit{};
std::cin >> limit;

int sum{0};
for (int i{1}; i <= limit; ++i) {
    sum += i;
}
```

## Practice Time

Try the following before moving on:

1. Ask the user for a positive integer `n`.
2. Print the numbers from 1 to `n` on one line separated by spaces using a `for` loop.
3. In the same loop, accumulate the running total.
4. After the loop, print `Sum: {total}` on a new line.

When you are ready, open the exercise to reinforce loop control and accumulator patterns.
