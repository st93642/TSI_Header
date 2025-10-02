# Chapter 3: Working with Fundamental Data Types

## Learning goals

- Interpret operator precedence and associativity without relying on guesswork.
- Manipulate bit patterns safely using shifts, masks, and logical bitwise operations.
- Model access policies with enumerations and aliases while respecting storage duration rules.

## Preparation checklist

- Create **~/tsi_cpp/ch3_working_with_data** and copy over your Chapter 2 toolchain notes.
- Hand-copy the precedence ladder from this chapter into your notebook so you can consult it offline.
- Add glossary entries for **lifetime**, **storage duration**, **bit mask**, and **flag** before you start coding.

---

## 3.1 Mapping the precedence ladder

Table 3-1 in the reference orders every C++ operator. Reproduce the essentials in your notes using this condensed chart:

| Rank (high→low) | Operators to remember | Associativity reminder |
| --- | --- | --- |
| 1 | **::** | left |
| 2 | **()**, **[]**, member access, postfix **++/--** | left |
| 3 | unary **+ - ! ~**, prefix **++/--**, **&** (address-of), **\*** (indirection), **sizeof**, **new/delete** | right |
| 5 | **\*** **/** **%** | left |
| 6 | **+ -** | left |
| 7 | **<< >>** | left |
| 8 | **< <= > >=** | left |
| 9 | **== !=** | left |
| 13 | **&&** | left |
| 14 | **||** | left |
| 15 | conditional **?:**, assignments (**=, +=, &=** …) | right |
| 16 | comma **,** | left |

> Offline practice: write the expression **x * y / z - b + c - d** on paper. Insert parentheses step by step until it matches the evaluation order **((((x * y) / z) - b) + c) - d**. Record your reasoning beside the expression so you can review it before exams.

## 3.2 Parentheses rehearsal without a compiler

In your lab notebook, create a sequence of expressions mixing relational and logical operators. Example: **score >= 35 && score <= 50 || score == 60**. Predict whether the result is **true** or **false** for several score values, then confirm by compiling **precedence.cpp** with **g++ -std=c++17 -Wall -Wextra -pedantic**. Whenever your mental model disagrees with the program, add parentheses that mirror the grouping the compiler already used. The physical act of writing each layer enforces the precedence rules far better than reading a table passively.

Notebook reminder:

- Record three tricky expressions and the final parenthesised form.
- Note which operators surprised you so you can revisit them in Chapter 4 when crafting conditionals.

---

## 3.3 Bitwise shifts: visualising data movement

Shifts move bit patterns left or right. Copy this diagram (adapted from the reference) into your notes to illustrate a left and right shift on **unsigned short number {16387};**:

Original (16387): 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1  
Left shift by 2:  0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0  (high bits fall off, zeros enter on the right)  
Right shift by 2: 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0  (low bits fall off, zeros enter on the left)

Key takeaways to jot down next to the diagram:

- Shifting left by **n** multiplies by **2ⁿ** only if no significant bits are discarded. Losing high bits mimics integer overflow.
- Every operand smaller than **int** promotes to **int** before the operator executes. Preserve the original width by casting the result back, for example **static_cast&lt;unsigned short&gt;(number << 2)**.
- Prefer unsigned types when shifting; right-shifting signed negatives might propagate the sign bit depending on the implementation.

> Offline drill: calculate **static_cast&lt;unsigned short&gt;(number >> 3)** manually, then verify by printing the value. Record the intermediate binary form so you understand which bits disappeared.

## 3.4 Building and applying masks

Masks select or toggle specific bits. Recreate Figure 3-1 from the reference as an ASCII layout in your notebook:

[Style (8 bits)] [Unused] [Italic] [Bold] [Size (5 bits)]  
Example value (0x064C): 0000 0110 0 1 1 00

To isolate the size, AND with **0x001F**. To capture style, AND with **0xFF00** and shift right by eight positions. When you need a particular flag, derive it with a shift: **static_cast&lt;unsigned short&gt;(1u << 5)** selects the sixth bit. Highlight that **shifting by n** accesses the **(n + 1)**th bit because counting starts at zero.

Notebook task checklist:

- Define **unsigned short font {0x064C};** and compute **font & 0x001F** on paper.
- Show how **font |= italic_mask** turns on the italic bit without disturbing other fields.
- Demonstrate turning a bit off by ANDing with the complement mask, for example **font &= static_cast&lt;unsigned short&gt;(~italic_mask);**.

## 3.5 Signed versus unsigned shifts

Signed right shifts may fill vacated positions with either zeros or ones. Summarise the behaviour in your notes:

- Example setup: **signed char value {-104};** yields the binary sequence **10011000** on a two’s complement machine.
- After **value >>= 2;**, some platforms produce **11100110** (sign bit propagated), which equals **-26**.
- To preserve raw bit patterns, default to unsigned types for bitwise logic.

Write yourself a warning strip: *“Never rely on right-shifting negatives unless you have verified the compiler’s behaviour.”*

---

## 3.6 Enumerations, aliases, and combining flags

Scoped enumerations mirror the strongly typed flag sets described in the reference. Define **enum class AccessFlag : std::uint8_t { student = 0b0001, lab = 0b0010, researcher = 0b0100, admin = 0b1000 };** and provide helper functions that use bitwise operators under the hood.

Suggested helper signatures for your lab journal:

- **constexpr std::uint8_t to_mask(AccessFlag flag)** → returns the underlying bit.
- **constexpr std::uint8_t combine(std::uint8_t mask, AccessFlag flag)** → sets the corresponding bit.
- **constexpr bool has(std::uint8_t mask, AccessFlag flag)** → tests the bit via AND.

Add **using AccessMask = std::uint8_t;** so future refactors remain painless.

> Reflection prompt: why does **enum class** prevent accidental comparison with unrelated integers? Capture your answer in the margin for quick review later.

## 3.7 Storage duration and scope replay

Recreate the scenarios from Chapter 1 with more nuance:

- Automatic variables live from block entry to block exit. Demonstrate with nested blocks printing both values and addresses.
- Static local variables, declared with **static**, retain their value across calls. Log the counter before and after repeated invocations to observe persistence.
- Global objects begin before **main** and persist until program termination; remind yourself why they should be rare in student labs.

In your project journal, design a table with three columns: *Storage duration*, *Typical declaration*, *When to use*. Fill it with entries for automatic, static, thread, and dynamic storage durations. This table becomes a quick reference when Chapter 5 dives into memory management.

---

## 3.8 Lab: Lab Access Bitmask Analyzer

This lab matches the automated exercise bundled with the curriculum. Keep the starter blueprint close so your implementation aligns with the tests.

1. File name: **access_panel.cpp** inside **~/tsi_cpp/ch3_working_with_data**.
2. Required headers: **&lt;iostream&gt;**, **&lt;bitset&gt;**, **&lt;cstdint&gt;**, **&lt;string_view&gt;** (optional but helpful for labels).
3. Define **enum class AccessFlag** exactly as listed in Section 3.6 and create helper routines **grant**, **revoke**, and **has_permission** that wrap bitwise operations.
4. Read an unsigned integer mask from **std::cin**. Store it in **AccessMask current_mask {input_value};** to document intent.
5. Print the report using the following template (spacing and casing must match the tests):
    - **Mask: X**
    - **Binary: BBBB** where BBBB is **std::bitset&lt;4&gt;(current_mask)**
    - **Student Access: Yes/No**
    - **Lab Access: Yes/No**
    - **Researcher Access: Yes/No**
    - **Admin Access: Yes/No**
6. Verify manual cases before running the automated tests. Example log to copy into your notebook:

Input 5  
Mask: 5  
Binary: 0101  
Student Access: Yes  
Lab Access: No  
Researcher Access: Yes  
Admin Access: No

- Compile with **g++ -std=c++17 -Wall -Wextra -pedantic access_panel.cpp -o access_panel** and note the command in your offline build diary.
- Add assertions using **&lt;cassert&gt;** to guard **grant** and **revoke** helper functions while you experiment.

> Optional extension: log every mask change to **access_log.txt** so you can review how roles evolved during a session.

---

## 3.9 Self-check prompts

- How does adding parentheses change the evaluation of **a + b << 1**?
- Why must you cast the result of **std::uint8_t shifts** back to **std::uint8_t** after combining flags?
- When do you prefer **static** locals over globals while preserving state between calls?
- What behaviour would you expect from **AccessFlag::student** if you forget to cast before OR-ing with an integer?

Write answers in **reflection/chapter_03_answers.md** and revisit them before tackling Chapter 4.

## Troubleshooting guide

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Precedence result differs from prediction | Operator grouping misread | Add parentheses that mirror the desired order; confirm against the ladder in Section 3.1. |
| Bit mask loses data after a left shift | Cast omitted, causing temporary promotion to **int** | Apply **static_cast** to the final result so it fits the target type. |
| Right shift of negative numbers behaves unpredictably | Implementation-defined sign propagation | Restrict bitwise work to unsigned types or adjust the algorithm. |
| Enum flags fail to combine | Missing helper that converts to the underlying type | Implement **to_mask(AccessFlag)** and reuse it everywhere. |

## Wrap-up checklist

- [ ] You rewrote complex expressions with parentheses that mirror compiler evaluation.
- [ ] You created and used bit masks to toggle specific flags while preserving neighbouring bits.
- [ ] You practiced modelling permissions with **enum class** and helper functions.
- [ ] You observed how automatic and static storage durations differ by instrumenting sample programs.

Bring this chapter’s precedence ladder and bit-mask sketches into Chapter 4. They will accelerate your work when combining relational and logical operators inside control flow.
