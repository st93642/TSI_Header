# Chapter 2: Introducing Fundamental Types of Data

## Learning goals

- Distinguish the core C++17 fundamental types: integers, floating-point values, characters, and booleans.
- Practise uniform initialization and recognise when the compiler catches narrowing conversions.
- Explore literal notations in decimal, hexadecimal, octal, and binary without relying on internet references.
- Record local measurements for size, range, and precision so future projects have offline data at hand.

## Preparation checklist

- Create a workspace folder named **~/tsi_cpp/ch2_fundamentals** to isolate this chapter’s source files.
- Copy the compile-and-run command from Chapter 1 into your lab journal for quick reuse.
- Set up a spreadsheet or markdown table in your notes to capture the ranges you discover on your machine.

---

## 2.1 Uniform initialization for integers

Think of braced initializers as the safest gatekeepers for integer variables. Define descriptive values like **int apple_count {15};** and **int orange_count {5};**, then combine them with **int total_fruit {apple_count + orange_count};**. Because the braces demand compatible types, the compiler warns you when a value would be truncated. Keep a tally of any diagnostics in your offline notebook so you recognise the patterns across different compilers.

Key reminders:

- **Braced initializers** reject obvious narrowing conversions, while functional **( )** or assignment **=** syntax might slip them through silently.
- When several related integers appear, favour one statement per line—even if the language allows **int foot_count {2}, toe_count {10};**—because readability matters more than brevity.

---

## 2.2 Signed versus unsigned thinking

Signed types model quantities that can dip below zero, unsigned types emphasise counts that stay non-negative. Sketch a simple ruler in your notes to visualise the difference:

- Signed range sketch: **| negative … -2 | -1 | 0 | +1 | +2 … positive |**
- Unsigned range sketch: **| 0 | +1 | +2 | +3 | … |**

Use **short**, **int**, **long**, and **long long** for the signed side, then repeat with the **unsigned** keyword. Record **sizeof(type)** alongside **std::numeric_limits&lt;type&gt;::min()** and **max()** so you can compare architectures. Highlight that **char** is a special case: the standard leaves its signedness implementation-defined, so reserve plain **char** for character data and pick **signed char** or **unsigned char** for numeric work.

> **Notebook task:** Build a two-column table titled *Type family* and *Typical range*. Fill it with signed and unsigned variants you inspect locally. This becomes your quick-reference chart when tackling later exercises.

---

## 2.3 Avoiding silent narrowing conversions

The book warns against fractional values sneaking into integers. Recreate the cautionary trio using bold inline notes:

- **int banana_count(7.5);**  → may compile, silently truncating to 7.
- **int coconut_count = 5.3;** → same behaviour, result becomes 5.
- **int papaya_count {0.3};**  → compiler must at least warn, often errors out.

Record the exact wording of the diagnostic produced by your compiler. Save it in **notes/narrowing_log.md** so you can cite it when mentoring classmates or debugging older codebases.

---

## 2.4 Zero initialization and compile-time constants

To emphasise known starting states, adopt empty braces: **int counter {};** automatically yields zero, mirroring the numeral’s shape. For immutable values, prepend **const**, as in **const unsigned toe_count {10};**, and explain in a comment why the value must never change. Capture a small checklist in your notes describing when you choose **const** (for example, configuration parameters, physical constants, inventory totals at the start of a simulation).

---

## 2.5 Integer literal tour

Create a literal gallery in your lab journal. Group them by base and annotate their intent:

- Decimal: **123**, **-1'234**, **22'333** (digit separators keep long numbers readable).
- Unsigned suffixes: **42U**, **99UL**, **15'000'000LL** (explain why uppercase L beats lowercase l).
- Hexadecimal: **0xFF00FF00**, **0xDEADBEEF**, **0xFF00'00FF'0001UL**.
- Octal: **012**, **077**, and a reminder that leading zeros change interpretation.
- Binary (C++14+): **0b11001010**, **0B1010'1010'0001**, with notes on grouping bits.

> **Offline exercise:** Write down a colour constant such as pure red using RGB bytes, e.g. **0xFF0000**, and describe what each pair of hex digits controls.

---

## 2.6 Quick-reference card for integer families

Translate Table 2-1 into your own words. For each signed type in the reference (signed char through long long), jot down:

- **Type name**
- **Typical width in bytes**
- **Value range**
- **Common usage hint** (for example, *long long* → large counters across datasets)

Then add a second list for the matching unsigned types and highlight that **unsigned** mainly documents intent rather than extending range dramatically.

Diagram suggestion to include in your notes:

Signed ladder:  signed char &lt; short &lt; int &lt; long &lt; long long
Unsigned ladder: unsigned char &lt; unsigned short &lt; unsigned int &lt; unsigned long &lt; unsigned long long

Copy the ladder as plain text—no need for code fences—and keep it near your study materials.

---

## 2.7 Character storage choices

The reference stresses that plain **char** is for textual characters, not general-purpose numbers. Write two contrasting examples in your notebook:

- **char initial {'R'};** → character intent.
- **unsigned char bytes_remaining {255};** → explicit unsigned storage.

Add a reminder that the macro **CHAR_MIN** in **climits** tells you whether plain **char** acts signed (negative range) or unsigned (zero-based).

---

## 2.8 Literal experiments without internet access

Create a file called **literals_walkthrough.txt** during your study session and document the following mini tasks:

1. Write four values that represent the same decimal number using decimal, hexadecimal, octal, and binary forms. Annotate which prefix each base uses.
2. Convert your favourite day of the month to binary and hex; verify the equality with calculator-free reasoning.
3. Demonstrate how digit separators aid readability by rewriting **1000000000** as **1'000'000'000** and describing why it’s clearer.

Include all steps in text so you can teach the concept offline.

---

## 2.9 Floating-point precision diary

Even though Chapter 2’s spotlight is on integers, the reference briefly introduces floating-point characteristics. Start a diary page summarising:

- What **std::numeric_limits&lt;double&gt;::infinity()** prints on your machine.
- How **std::numeric_limits&lt;double&gt;::quiet_NaN()** behaves when streamed to output.
- A subtraction example such as **1.0 - 0.999999999** showing rounding artefacts.

Note the exact strings written to your terminal and whether scientific notation appears. These observations become invaluable when diagnosing numerical bugs later.

---

## 2.10 Lab: Type fact sheet generator

This lab aligns with the Chapter 2 exercise and reinforces `std::numeric_limits` exploration.

1. Start **type_fact_sheet.cpp** and include headers **&lt;iomanip&gt;**, **&lt;limits&gt;**, and **&lt;string&gt;**.
2. Create a helper routine description in your own words (for example, *print_type* takes a label and a type instance, then writes size, min, max, and signedness using aligned columns).
3. List the types you must cover: **int**, **unsigned int**, **long double**, **char**, and add any platform-specific extras you find insightful.
4. Document the compile command in your lab notes as **g++ -std=c++17 -Wall -Wextra -pedantic type_fact_sheet.cpp -o type_fact_sheet**.
5. Paste the resulting table into **notes/type_fact_sheet.md** so you can compare future toolchains.
6. Extend the program with a line that records **std::numeric_limits&lt;T&gt;::is_iec559** for floating-point types and comment on whether your environment follows IEEE 754.

---

## 2.11 Offline micro missions inside VS Code

- **Mission A – Range journal:** After running your fact sheet, append the sizes and ranges to **notes/range_journal.md**. Highlight discrepancies from the values printed in the reference text.
- **Mission B – Narrowing alert:** Intentionally compile a short file containing **int mislabeled {0.7};** and capture the compiler’s message word for word in **notes/narrowing_log.md**.
- **Mission C – Literal scavenger hunt:** Search **ref.txt** for examples of binary, octal, and hexadecimal literals. Paraphrase each one into your own words, then copy the paraphrased versions into your study notebook.

Store all supporting files locally to maintain the offline study standard.

---

## 2.12 Self-check prompts

- Why will **unsigned int** never represent **-1**, and what exact numerical wrap-around occurs if you assign that value anyway?
- Which literal prefixes signal binary, hexadecimal, and octal, and how would you explain the difference to a beginner?
- How does **std::numeric_limits&lt;float&gt;::epsilon()** guide you when comparing floating-point results?
- When would you deliberately prefer **unsigned char** over **char**?

Record answers in **reflection/chapter_02_answers.md**.

---

## Troubleshooting guide

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| **numeric_limits reports 0 for max** | Header missing | Include **&lt;limits&gt;** and rebuild. |
| **Binary literal rejected** | Compiler default set to an older language standard | Recompile with **-std=c++17**. |
| **Table columns misaligned** | Output stream still in default alignment | Use **std::setw** and **std::left** before printing labels. |
| **Character range unexpected** | Plain **char** behaves as signed on this toolchain | Switch to **unsigned char** for byte-sized numeric data. |

---

## Wrap-up checklist

- [ ] You created a local chart of sizes and ranges for the integer families on your machine.
- [ ] You experimented with multiple literal notations and confirmed they evaluate to the same number.
- [ ] You documented floating-point behaviours such as infinity, NaN, and epsilon in your precision diary.
- [ ] You recorded at least one compiler diagnostic illustrating a failed narrowing conversion.

With these notes complete, move to Chapter 3 to explore how C++ evaluates expressions and stores automatic variables. Bring your range journal along—operators often reveal subtle type promotions best understood when backed by the data you captured here.
