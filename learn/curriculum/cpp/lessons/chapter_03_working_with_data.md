# Chapter 3: Working with Fundamental Data Types

## Learning goals

- Apply operator precedence and associativity rules without relying on a calculator.
- Manipulate bit patterns safely using bitwise and shift operators.
- Define enumerations, aliases, and scoped lifetimes to model simple systems.

## Preparation checklist

- Copy your Chapter 2 project into `~/tsi_cpp/ch3_working_with_data` so experiments stay contained.
- Print or copy a precedence table into your notes; you will consult it frequently.
- Review the glossary you have built so far and add entries for “lifetime” and “storage duration”.

## 3.1 Operator precedence in practice

Create `precedence.cpp` containing chained arithmetic, logical, and relational operators. Annotate each expression with comments explaining how the compiler groups it. Compile with `-Wall` and add parentheses until manual evaluation matches the program output.

```cpp
#include <iostream>

int main() {
    int score = 42;
    bool passed = (score >= 35 && score <= 50) || score == 60;
    std::cout << std::boolalpha << "passed? " << passed << std::endl;
}
```

Document any surprises in your learning journal; these notes help when the same pattern appears in later chapters.

## 3.2 Bitwise operators and masks

Practice representing permission flags with `std::uint8_t`. Create `bitmask.cpp` that toggles bits using AND, OR, XOR, NOT, and shift operators. Provide named constants via `constexpr` to keep the code readable, and display the results with `std::bitset<8>`. This reinforces the idea that bitwise code remains clear when paired with thoughtful naming.

## 3.3 Enumerations and type aliases

- Define `enum class AccessLevel { student = 1, lab = 2, professor = 4 };` to illustrate scoped enums.
- Overload `operator|` for the enumeration so that combining flags stays type-safe.
- Introduce `using DurationHours = unsigned int;` to showcase modern alias syntax.

These constructs help you avoid “magic numbers” and ease refactoring in larger projects.

## 3.4 Variable lifetime and scope

Build `scope_demo.cpp` with nested blocks that print both values and addresses of variables. Add a helper function that maintains a `static` counter to underline the difference between automatic and static storage duration. Compare the results to the journal entry you created during the preparation checklist.

## 3.5 Lab: access control panel

Consolidate the chapter’s ideas with a short project.

1. Create `access_panel.cpp`.
2. Declare permission bits as `constexpr std::uint8_t` and wrap them in an `enum class`.
3. Implement `grant`, `revoke`, and `has_permission` functions using bitwise logic.
4. Present a simple text menu that lets you toggle roles and prints the resulting mask via `std::bitset<8>`.
5. Compile and run:

   ```bash
   g++ -std=c++17 -Wall -Wextra -pedantic access_panel.cpp -o access_panel
   ```

6. Add assertions with `<cassert>` to confirm the helper functions behave as intended.

## 3.6 Self-check prompts

- Why is `enum class` safer than a plain `enum`?
- How does `operator>>` behave on signed integers compared with unsigned ones?
- When might a `static` local variable be preferable to global state?

## 3.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Unexpected precedence result | Missing parentheses | Add explicit grouping that mirrors your mental model. |
| Bit masks clip to unexpected values | Masks declared with a signed type | Use `std::uint8_t` or a wider unsigned type. |
| `enum class` values resist arithmetic | Strong typing by design | Apply `static_cast` when arithmetic is required. |

## Wrap-up

- [ ] You evaluated complex expressions using precedence rules from memory.
- [ ] You manipulated permission flags with bitwise operators and recorded the results.
- [ ] You demonstrated block scope and static lifetime in compiled code.

Next, Chapter 4 examines structured control flow with `if`, `switch`, and deliberate use of logical operators. Keep your precedence table handy; it will guide the guard conditions you write there.
