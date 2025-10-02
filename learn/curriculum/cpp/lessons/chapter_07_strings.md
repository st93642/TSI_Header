# Chapter 7: Working with Strings

## Learning goals

- Manipulate `std::string` objects using the Standard Library interfaces.
- Convert between numeric values and strings with stream utilities.
- Handle Unicode-aware workloads offline using wide-character types when necessary.

## Preparation checklist

- Prepare `~/tsi_cpp/ch7_strings` and collect sample text files (UTF-8 and ASCII) for experiments.
- Copy the `<string>` and `<sstream>` reference pages into your notes or keep them bookmarked for offline use.
- Review locale basics so that formatting behaviour does not surprise you mid-lab.

## 7.1 Constructing and modifying strings

Code `string_basics.cpp` that demonstrates construction from literals, repeated characters, and substrings. Use `append`, `insert`, `erase`, and `replace` while logging intermediate states. Observing each step helps you internalise how the buffer changes.

## 7.2 Accessing characters safely

Implement `char_access.cpp` to compare `operator[]` and `.at()`. Intentionally trigger an out-of-range exception and wrap it in a `try/catch` block to document the error offline. Mention in your notes how `std::string_view` offers non-owning access when you simply need to inspect characters.

## 7.3 Searching and slicing

Build `search_slice.cpp` that uses `find`, `find_first_of`, and `substr` to extract tokens from a log line. Comment each method call with the situations where it excels (e.g., `find_first_of` for delimiter sets).

## 7.4 String streams and conversions

Create `stringstream_demo.cpp` showing numeric-to-string and string-to-numeric conversions using `std::ostringstream` and `std::istringstream`. Check failure states with `if (!stream)` and log them so you can debug conversion errors later without additional tooling.

## 7.5 Lab: campus bulletin formatter

1. Create `bulletin_formatter.cpp`.
2. Read multiline input (simulate offline by hardcoding sample text) describing events.
3. Normalise whitespace, capitalise the first letter of each sentence, and convert numeric tokens into formatted currency using `std::stringstream` and manipulators.
4. Build the output using `std::ostringstream` and print the final bulletin.
5. Compile and test with different input strings to ensure robustness.

## 7.6 Self-check prompts

- When should you prefer `std::string_view` over `std::string`?
- How does `find` signal “not found”?
- What advantages do stream-based conversions offer compared with `std::stoi` or `std::stod`?

## 7.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Unexpected exception from `.at()` | Accessing out of range | Wrap calls in `try/catch` and validate indices. |
| UTF-8 characters cut in half | Iterating byte-by-byte | Use `std::wstring` or specialised libraries when handling multi-byte characters. |
| Stream conversion fails silently | Stream left in error state | Check `.fail()` and call `.clear()` before reuse. |

## Wrap-up

- [ ] You exercised major `std::string` modifiers and recorded their effects.
- [ ] You practised safe character access and logged exceptions.
- [ ] You converted values using string streams and reflected on locale nuances.

Chapter 8 returns to functions—structuring code for reuse. Keep your string utilities close; they make excellent helper routines for that chapter’s exercises.
