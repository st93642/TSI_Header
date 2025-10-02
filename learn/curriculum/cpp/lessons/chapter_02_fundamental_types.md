# Chapter 2: Introducing Fundamental Types of Data

## Learning goals

- Classify C++17 fundamental types such as integers, floating-point values, characters, and booleans.
- Practise literal notation in decimal, hexadecimal, octal, and binary forms.
- Measure ranges and precision offline using `<limits>` and record the results for future reference.

## Preparation checklist

- Create a workspace folder `~/tsi_cpp/ch2_fundamentals` so source files stay organised.
- Copy the compile command from Chapter 1 into a notes file for quick reuse.
- Keep a journal or spreadsheet ready to capture the ranges you observe locally.

## 2.1 Signed and unsigned integers

Signedness controls whether a type can represent negative values. Reproduce the following experiment for `short`, `int`, `long`, and `long long` to understand width and range:

```cpp
#include <iostream>
#include <limits>

int main() {
    std::cout << "sizeof(int): " << sizeof(int) << " bytes\n";
    std::cout << "int range: "
              << std::numeric_limits<int>::min() << " to "
              << std::numeric_limits<int>::max() << std::endl;
}
```

Repeat with the `unsigned` keyword and note how the lower bound moves to zero while the upper bound increases. Log each observation in your offline chart. These tables become a quick reference whenever you switch toolchains.

## 2.2 Literal forms

Literal prefixes make your intent clear to other humans. Create `literals.cpp` that prints the same value defined four ways: decimal, hexadecimal (`0x`), octal (`0`), and binary (`0b`). Print each literal and then sum them to confirm they evaluate to the same number. This reinforces that literal syntax changes readability, not the stored bits.

## 2.3 Floating-point nuances

Floating-point types trade range for precision. With `<cmath>` and `<limits>`, explore the following:

- `std::numeric_limits<double>::infinity()` represents overflow beyond the largest finite number.
- `std::numeric_limits<double>::quiet_NaN()` signals an undefined numeric result.
- Subtracting nearly equal numbers (e.g., `1.0 - 0.999999999`) reveals rounding error.

Capture each experiment and its output in your notebook. You will reuse these notes when debugging scientific code later in the course.

## 2.4 Lab: type fact sheet generator

Consolidate your knowledge with a reusable tool.

1. Create `type_fact_sheet.cpp` and include `<iomanip>`, `<limits>`, and `<string>`.
2. Write a helper function `print_type(const std::string& name, auto dummy)` that outputs aligned columns using `std::setw` and `std::left`.
3. Display size, minimum, maximum, and signedness for `int`, `unsigned int`, `long double`, and `char`.
4. Compile with:

   ```bash
   g++ -std=c++17 -Wall -Wextra -pedantic type_fact_sheet.cpp -o type_fact_sheet
   ```

5. Run the executable and paste the table into your notes.
6. Extend the program with a `constexpr bool` based on `std::numeric_limits<T>::is_iec559` to record whether each floating-point type uses IEEE 754 semantics.

## 2.5 Self-check prompts

- Why can an `unsigned int` never represent `-1`, and what happens if you assign it anyway?
- Which literal prefix signals binary notation? Hexadecimal?
- How does `std::numeric_limits<float>::epsilon()` help you reason about rounding error?

## 2.6 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| `numeric_limits` reports `0` for max | Missing `<limits>` include | Add the header and rebuild. |
| Binary literal rejected | Compiler default set to an older language standard | Recompile with `-std=c++17`. |
| Table columns misaligned | Formatter not configured | Use `std::setw` and `std::left` when printing. |

## Wrap-up

- [ ] You produced a local chart of sizes and ranges for fundamental types.
- [ ] You experimented with multiple literal notations and confirmed they represent the same value.
- [ ] You analysed floating-point precision limits using `epsilon`, `infinity`, and `NaN`.

Next, Chapter 3 explores operators, expressions, and how C++ stores automatic variables in memory. Prepare a fresh folder so your experiments remain neatly separated.
