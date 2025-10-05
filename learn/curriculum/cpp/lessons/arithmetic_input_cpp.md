# Arithmetic and User Input

Mixing arithmetic with live input unlocks interactive programs: grade calculators, budgeting tools, or engineering estimators. This expanded lesson revisits arithmetic operators, explores precision concerns, and shows how to validate user input before performing calculations.

## Learning goals

- Apply arithmetic operators and understand precedence, associativity, and short-circuit evaluation.
- Distinguish between integer and floating-point arithmetic, including truncation and rounding strategies.
- Safely gather numeric input from the console with validation loops.
- Use standard library helpers (`std::pow`, `std::round`, `std::fabs`) to expand beyond primitive operators.
- Present results with consistent formatting (fixed precision, alignment).

## Operator recap and nuances

| Operator | Description | Example |
| --- | --- | --- |
| `+` | Addition | `totalCredits = core + electives;` |
| `-` | Subtraction | `int remaining = goal - completed;` |
| `*` | Multiplication | `area = width * height;` |
| `/` | Division | `double avg = sum / count;` |
| `%` | Remainder | `bonus = points % 10;` |

- `%` works only with integers. For floating-point remainder use `std::fmod` from `<cmath>`.
- Unary `-` negates a value; unary `+` has no effect but can clarify intent.
- Compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`) updates a variable in place and can improve readability.

### Precedence and grouping

Order mirrors algebra: multiplication and division take priority over addition and subtraction. Use parentheses to highlight evaluation order and avoid subtle bugs:

```cpp
double average = static_cast<double>(sum) / count + bonus;         // bonus added after division
double correctedAverage = (static_cast<double>(sum) / count) + bonus;
```

The two statements are equivalent because division happens before addition, but the second is easier to read.

### Integer overflow and limits

Fundamental types have finite range. When calculations exceed that range, they wrap (unsigned) or overflow (implementation-defined for signed). Keep safeguards in mind:

```cpp
#include <limits>
int x = std::numeric_limits<int>::max();
int y = x + 1; // overflow, undefined behavior for signed integers
```

For critical financial or scientific applications consider arbitary precision libraries or 128-bit integers where available.

## Integer vs floating-point division

Division between integers truncates toward zero:

```cpp
int total{7};
int pieces{2};
int truncated = total / pieces;          // 3
double exact = static_cast<double>(total) / pieces; // 3.5
```

Convert at least one operand to `double` before dividing when you need fractional results. Use `static_cast<double>` or multiply by `1.0`.

### Rounding strategies

```cpp
#include <cmath>
double raw = 7.0 / 3.0; // ≈ 2.3333
double rounded = std::round(raw * 100.0) / 100.0; // 2.33 (two decimal places)
```

- `std::round` rounds half away from zero.
- `std::floor` and `std::ceil` step down or up to the nearest integer.
- `std::trunc` discards the fractional part like integer division but on doubles.

## Gathering multiple values

Encourage friendly prompts and handle errors gracefully.

```cpp
#include <iostream>
#include <limits>

int main() {
    int a{};
    int b{};

    std::cout << "Enter two integers separated by spaces: ";
    while (!(std::cin >> a >> b)) {
        std::cerr << "Please enter numeric values.\n";
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << "Enter two integers separated by spaces: ";
    }

    std::cout << "You entered " << a << " and " << b << '\n';
}
```

### Reading decimals safely

```cpp
double hours{};
std::cout << "Enter weekly study hours: ";
while (!(std::cin >> hours)) {
    std::cerr << "Hours must be numeric. Try again.\n";
    std::cin.clear();
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}
```

Always clear the stream after a failure before retrying.

## Arithmetic helper functions

```cpp
#include <cmath>

double base{2.0};
double exponent{3.0};
double power = std::pow(base, exponent);    // 8.0
double root = std::sqrt(49.0);              // 7.0
double absolute = std::fabs(-12.5);         // 12.5
```

- `<cmath>` functions operate on `double` by default; overloads for `float` and `long double` exist.
- Combine standard functions with arithmetic operators to create rich formulas without reimplementing math.

## Formatting the results

Leverage `<iomanip>` to display aligned tables:

```cpp
#include <iomanip>

std::cout << std::fixed << std::setprecision(1);
std::cout << std::setw(12) << "Sum" << " : " << sum << '\n';
std::cout << std::setw(12) << "Difference" << " : " << difference << '\n';
std::cout << std::setw(12) << "Product" << " : " << product << '\n';
std::cout << std::setw(12) << "Quotient" << " : " << quotient << '\n';
```

- `std::setw` aligns labels.
- `std::fixed` and `std::setprecision` control decimal places.
- Reset formatting with `std::cout.unsetf(std::ios::floatfield);` when done.

## Worked example: course load calculator

```cpp
#include <iomanip>
#include <iostream>
#include <limits>

int main() {
    int completed{};
    int target{};
    double weeklyHours{};

    std::cout << "Completed credits: ";
    while (!(std::cin >> completed)) {
        std::cerr << "Enter a whole number please.\n";
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << "Completed credits: ";
    }

    std::cout << "Target credits: ";
    while (!(std::cin >> target) || target <= completed) {
        std::cerr << "Target must exceed completed credits.\n";
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << "Target credits: ";
    }

    std::cout << "Weekly study hours: ";
    while (!(std::cin >> weeklyHours) || weeklyHours < 0.0) {
        std::cerr << "Hours must be non-negative.\n";
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << "Weekly study hours: ";
    }

    const int remaining = target - completed;
    constexpr double creditsPerSemester{30.0};
    const double semestersNeeded = static_cast<double>(remaining) / creditsPerSemester;
    const double hoursPerCredit = weeklyHours / creditsPerSemester;

    std::cout << '\n';
    std::cout << std::fixed << std::setprecision(1);
    std::cout << "Remaining credits : " << remaining << '\n';
    std::cout << "Semesters needed  : " << semestersNeeded << '\n';
    std::cout << "Hours per credit  : " << hoursPerCredit << '\n';
}
```

This sample ties together input validation, arithmetic conversions, and formatted output.

## Common pitfalls

- Dividing integers accidentally when you need precise averages.
- Ignoring the possibility of division by zero—always guard the denominator.
- Using `%` with negative operands; the sign of the remainder matches the dividend in C++.
- Forgetting to clear the input stream after a failed extraction, causing infinite loops.
- Repeating literal values (`30.0`, `42.5`) instead of naming them with `constexpr` constants.

## Practice time

1. **Trip cost estimator:** Ask for distance (km), fuel efficiency (litres per 100 km), and fuel price (per litre). Compute total fuel cost and average cost per kilometre.
2. **Grade calculator:** Read five assignment scores (double), drop the lowest, average the rest, and display the result to two decimal places.
3. **Time splitter:** Input a total number of minutes and output hours plus remaining minutes using integer division and modulus.
4. **Robust division:** Prompt for numerator and denominator. If the denominator is zero, keep prompting. Print quotient as double and remainder as integer when appropriate.

## Self-check questions

1. What is the difference between `/` and `%`, and when should you use `std::fmod`?
2. Why does `5 / 2` yield `2` while `5 / 2.0` yields `2.5`?
3. How do you prevent division-by-zero crashes during interactive input?
4. When should you prefer compound assignment operators like `+=`?
5. Which `<cmath>` function would you use to round a value to two decimal places?

Answer these before moving on to conditionals where arithmetic results drive branching logic.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Numeric I/O & Tools (Appendix — External Tools — arithmetic_input_cpp-appendix)

Short recipes for safe arithmetic input, parsing, and error handling in C++ along with references to cppreference for streams and numeric conversions.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Use</th><th>Reference</th></tr>
  </thead>
  <tbody>
    <tr><td>std::cin / iostream</td><td>Basic input/output</td><td><a href="https://en.cppreference.com/w/cpp/io">cppreference iostream</a></td></tr>
    <tr><td>std::stoi / std::stod</td><td>String to numeric conversion</td><td><a href="https://en.cppreference.com/w/cpp/string/basic_string/stol">cppreference conversions</a></td></tr>
    <tr><td>Robust parsing</td><td>Check stream state and exceptions</td><td>Use fail()/clear() or std::from_chars for performance</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: safe integer read

```cpp
#include <iostream>
#include <string>
#include <limits>

int main(){
    int x;
    if(!(std::cin >> x)){
        std::cerr << "Invalid input\n";
        return 1;
    }
    std::cout << x << '\n';
}
```

### Exercises (arithmetic_input_cpp-appendix)

1. Replace `std::cin` parsing with `std::from_chars` for a high-performance parser (C++17+), and document differences.
2. Add input validation tests that feed invalid input via a redirected stdin file.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Arithmetic & Input — Parsing, Validation & Fast IO (Appendix — arithmetic_input_cpp-appendix2)

Practical guidance for parsing numeric input robustly in C++, validating values, and using fast IO techniques for large input.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Parsing</td><td>std::stoi / std::stoll</td><td>Catch `std::invalid_argument` and `std::out_of_range`</td></tr>
    <tr><td>Validation</td><td>Range checks</td><td>Check limits before using values</td></tr>
    <tr><td>Fast IO</td><td>sync_with_stdio(false)</td><td>Use for competitive-style inputs</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: safe parse

```cpp
#include <string>
#include <stdexcept>

int safe_stoi(const std::string &s, bool &ok) {
  try { ok = true; return std::stoi(s); } catch(...) { ok = false; return 0; }
}
```

### Exercises (Appendix — arithmetic_input_cpp-appendix2)

1. Implement robust integer parsing with error flags and test with large and malformed inputs.
2. Benchmark reading numbers with `cin` vs a custom fast reader and report timings.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Arithmetic Input — Edge-case Tests & Validation (Appendix — arithmetic_input_cpp-appendix3)

Extra guidance for validating numeric inputs and writing tests that confirm behavior over malformed inputs and boundary values.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Malformed input</td><td>return error codes</td><td>Do not trust external input</td></tr>
    <tr><td>Boundary tests</td><td>use INT_MAX/INT_MIN</td><td>Ensure no overflow occurs</td></tr>
    <tr><td>Timeouts</td><td>validate before heavy work</td><td>Fail fast on bad inputs</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix — arithmetic_input_cpp-appendix3)

1. Add tests exercising `std::stoi` failure modes and boundary conditions.
2. Implement input validation that returns clear error codes and test with a harness.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Arithmetic & Input — Parsing, Overflow & Formatting (Appendix — arithmetic_input_cpp-appendix-20251005)

Practical details for safe numeric parsing, handling overflow and formatting for display.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Tip</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Parsing</td><td>Use `std::from_chars` where available</td><td>Provides error reporting without exceptions</td></tr>
    <tr><td>Overflow</td><td>Check ranges</td><td>Use wider types for accumulation</td></tr>
    <tr><td>Formatting</td><td>std::format / iomanip</td><td>Be careful with locales</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: from_chars

```cpp
#include <charconv>
int x;
auto [ptr, ec] = std::from_chars(buf, buf + len, x);
if (ec != std::errc()) {
  // handle parse error
}
```

### Exercises (Appendix — arithmetic_input_cpp-appendix-20251005)

1. Parse a list of integers robustly from input with error reporting.
2. Implement safe summation that avoids overflow by using `long long` or checked add.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
