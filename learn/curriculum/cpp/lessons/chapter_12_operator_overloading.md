# Chapter 12: Operator Overloading

## Learning goals

- Implement arithmetic, comparison, and stream operators for user-defined types.
- Decide between member and non-member overloads.
- Preserve class invariants while providing intuitive syntax.

## Preparation checklist

- Continue with the `Equipment` class from Chapter 11 in `~/tsi_cpp/ch12_operator_overloading`.
- Review operator categories (arithmetic, relational, logical, stream) in your notes.
- Prepare manual assertions or simple test harnesses to confirm operator behaviour offline.

## 12.1 Choosing an overload strategy

Create `inventory_item.cpp` defining arithmetic and comparison operators. Document why certain overloads belong as members (`operator[]`, `operator+=`) while others—like `operator<<`—work better as non-member functions.

## 12.2 Stream insertion and extraction

Implement `operator<<` and `operator>>` for your class. Handle formatting errors gracefully by checking the stream state and returning it. Make sure extraction validates input before mutating your object.

## 12.3 Compound assignment and conversion operators

Provide compound assignments (e.g., `+=`) plus an explicit conversion operator when necessary (for example, converting to `double` for cost). Note the pitfalls of implicit conversions and favour `explicit` to avoid surprises.

## 12.4 Lab: equipment ledger arithmetic

1. Extend `Equipment` with operators to combine maintenance time (`+=`, `-=`) and compare last service dates.
2. Implement `operator<<` to print a table row.
3. Provide a free function `total_runtime(const std::vector<Equipment>&)` that uses your overloaded operators to accumulate values.
4. Compile and run tests to ensure invariants hold (for example, no negative runtimes).

## 12.5 Self-check prompts

- When must `operator=` return a reference to `*this`?
- Why should stream insertion/extraction overloads take the stream by reference and return it?
- What risks accompany user-defined conversion operators?

## 12.6 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Infinite recursion in assignment | Self-assignment not handled | Add a self-check `if (this == &other) return *this;`. |
| Operator ambiguous | Multiple overloads match | Restrict the overload set or mark constructors `explicit`. |
| Stream output missing data | Stream not returned | End each overload with `return stream;`. |

## Wrap-up

- [ ] You overloaded at least one arithmetic and one comparison operator.
- [ ] You implemented `operator<<` and used it to print diagnostics.
- [ ] You considered conversion operators and documented the trade-offs.

Chapter 13 deepens inheritance hierarchies. Ready your classes to participate in base/derived relationships so you can reuse operators effectively.
