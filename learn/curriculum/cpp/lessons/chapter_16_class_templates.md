# Chapter 16: Class Templates

## Learning goals

- Build class templates with type and nontype parameters.
- Provide inline definitions and member templates correctly.
- Specialise templates when behaviour must diverge for certain types.

## Preparation checklist

- Create `~/tsi_cpp/ch16_class_templates`.
- Reuse the `fixed_buffer` idea from Chapter 9 as a starting point.
- Review your notes on partial specialisation before coding.

## 16.1 Template class skeleton

Declare a class template `Accumulator<T>` storing totals and counts. Provide member functions defined inline in the header to ensure instantiation at the point of use.

## 16.2 Member templates and iterators

Add member templates accepting iterator ranges. Demonstrate adding values from `std::vector<int>` and `std::list<double>` to show how deduction works for different containers.

## 16.3 Nontype template parameters

Extend `Accumulator<T, Size>` where `Size` limits stored history. Use `std::array` to keep the last `Size` elements and log behaviour when the buffer wraps.

## 16.4 Partial specialisation

Provide a specialisation for `Accumulator<std::string>` that concatenates strings with separators instead of numeric totals. Document why specialisation is necessary for this behaviour.

## 16.5 Lab: statistics toolkit

1. Create a header-only template `statistics.hpp`.
2. Implement a `Median<T>` class template computing the median for numeric types and a specialisation for strings (lexicographical middle).
3. Write tests verifying behaviour for `int`, `double`, and `std::string`.
4. Compile multiple files that include the header to ensure definitions are visible everywhere.

## 16.6 Self-check prompts

- Why do template class definitions typically live in headers?
- When would you choose partial specialisation over inheritance?
- How do member templates differ from class template parameters?

## 16.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Linker errors | Template definitions hidden in a `.cpp` file | Keep implementations in headers or mark them `inline`. |
| Deduction failure for member template | Iterator type missing requirements | Ensure iterators provide the needed operations or add concepts (C++20). |
| Specialisation ignored | Specialisation declared after use | Define specialisations before instantiation or include the header earlier. |

## Wrap-up

- [ ] You wrote a class template with both type and nontype parameters.
- [ ] You added member templates to accept iterator ranges.
- [ ] You created at least one specialisation for unique behaviour.

Chapter 17 explores move semantics—critical for performance in templated classes. Prepare to implement move constructors and assignment operators for your templates.
