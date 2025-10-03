# Making Decisions with Conditionals

Programs adapt by taking different branches. This lesson teaches the essential `if`, `else if`, and `switch` patterns you need to react to user input.

## The `if` statement

Use `if` to run code only when a condition is true. Pair it with `else` to handle the alternative path.

```cpp
int score{};
std::cin >> score;

if (score >= 90) {
    std::cout << "Excellent!";
} else {
    std::cout << "Keep practising.";
}
```

## Cascading branches

For more than two outcomes, chain `else if` blocks in descending order of priority so the first true condition wins.

```cpp
if (score >= 90) {
    // A
} else if (score >= 75) {
    // B
} else if (score >= 60) {
    // C
} else {
    // below 60
}
```

## Comparing values

Relational operators (`==`, `!=`, `<`, `<=`, `>`, `>=`) return boolean results. Combining them with logical operators (`&&`, `||`, `!`) lets you build richer decisions.

## `switch` for discrete options

Use `switch` when you have many discrete integral cases.

```cpp
switch (menuChoice) {
case 1:
    start();
    break;
case 2:
    settings();
    break;
default:
    std::cout << "Unknown option";
    break;
}
```

Always include a `default` branch to handle unexpected values.

## Practice Time

Try the following before moving on:

1. Prompt the user for an exam score between 0 and 100.
2. Categorise the result as `Outstanding`, `Great`, `Satisfactory`, or `Needs improvement` using `if`/`else if`.
3. Print the category prefixed with `Result:` on its own line.
4. If the score is invalid (outside 0–100) print `Result: Invalid score`.

When you are ready, open the exercise to reinforce conditional logic.
