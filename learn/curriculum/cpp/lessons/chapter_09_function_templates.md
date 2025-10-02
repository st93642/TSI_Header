# Chapter 9: Function Templates

## Learning goals

- Generalise algorithms with function templates and understand instantiation.
- Control template argument deduction, including nontype parameters.
- Provide explicit specialisations when behaviour must diverge.

## Preparation checklist

- Create `~/tsi_cpp/ch9_function_templates`.
- Reuse your Chapter 8 utilities; many functions are ready to become templates.
- List common helpers (such as `max` or `swap`) that you want to generalise.

## 9.1 Template basics

Declare a template `max_value` and inspect the compiled output to see how the compiler instantiates code for each type. If tooling is available, use `nm` or similar utilities to view the generated symbols and log them in your notes.

```cpp
#include <iostream>

template <typename T>
T max_value(T a, T b) {
    return (a < b) ? b : a;
}

int main() {
    std::cout << max_value(4, 9) << '\n';
    std::cout << max_value(4.2, 1.9) << '\n';
}
```

## 9.2 Multiple parameters and `auto`

Expand to templates with multiple parameters and return-type deduction using `auto`. Demonstrate how deduction works for mixed-type calls and note scenarios where you must specify template arguments explicitly.

## 9.3 Nontype template parameters

Implement `fixed_buffer<T, Size>` that wraps a `std::array<T, Size>`. Provide compile-time size information and highlight how the size becomes part of the type, enabling stronger compile-time guarantees.

## 9.4 Template specialisation and overloads

Show partial and full specialisations in `temperature_conversion.cpp`. Provide a general template for unit conversion and specialise for Fahrenheit/Celsius with precise formulas. Explain in comments why specialisation is necessary for nonlinear behaviour.

## 9.5 Lab: analytics helpers

1. Create `analytics.h`/`analytics.cpp` (or a header-only template file).
2. Provide a template `mean` function plus overloads for `std::vector<T>` and raw arrays.
3. Implement a `clamp` template with an optional comparator parameter.
4. Write tests in `main.cpp` verifying instantiation for `int`, `double`, and a custom struct with `operator<`.
5. Compile with full warnings and examine the diagnostics when comparator requirements are not met.

## 9.6 Self-check prompts

- Why must template definitions be visible to translation units that use them?
- When does template argument deduction fail, requiring explicit template arguments?
- What is the difference between partial specialisation and overloading?

## 9.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| “Undefined reference” when using a template | Definition hidden in a `.cpp` file | Move template definitions into headers. |
| Deduction failure | Types do not match the expected pattern | Specify template arguments manually. |
| Multiple definitions | Template defined in several translation units without `inline` | Keep definitions in headers or mark them `inline`. |

## Wrap-up

- [ ] You created generic algorithms and observed compiler instantiation.
- [ ] You experimented with nontype template parameters.
- [ ] You wrote at least one specialisation to customise behaviour.

Chapter 10 dives into translation units, headers, and preprocessing directives. Your template headers will become even more important as you organise larger projects offline.
