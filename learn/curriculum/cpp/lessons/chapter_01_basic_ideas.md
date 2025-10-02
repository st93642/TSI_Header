# Chapter 1: Basic Ideas

## Learning goals

- Understand how comments, whitespace, and deliberate formatting guide readers through a C++ source file.
- Describe how preprocessing, compilation, and linking cooperate to create an executable program.
- Construct, run, and debug a minimal console application with clear output and status codes.
- Develop offline study routines using in-editor exercises, reflective prompts, and lab notes.

## Prerequisites

- A C++17-capable compiler such as **g++** (GCC 11+) or **clang++**.
- Terminal access on your workstation (Linux, macOS, or Windows Subsystem for Linux).
- A dedicated workspace folder, for example **~/tsi_cpp/chapter_01**.

---

## 1.1 Comments and whitespace discipline

Comments exist for humans. Two primary patterns appear throughout this course:

- **Line comments** begin with two forward slashes and extend to the end of the line.
- **Block comments** begin with a slash followed by an asterisk and close with an asterisk followed by a slash; use them for multi-line explanations or to disable a region temporarily.

Whitespace — spaces, tabs, blank lines — visually separates ideas. Compare these aligned declarations to appreciate how formatting clarifies meaning:

- Line A: **int answer {42}; // tidy annotation keeps intent obvious**
- Line B: **int question { 108 }; // internal spacing is flexible but deliberate**

> **Notebook task:** Write a compact, hard-to-read fragment, then rewrite it with spacing and indentation. Note how your comprehension improves without changing the behavior.

---

## 1.2 Preprocessing directives and headers

Lines that start with the hash symbol are handled before compilation. The directive **#include &lt;iostream&gt;** literally copies the Standard Library definitions for console input/output into your translation unit so the compiler already knows about **std::cout** and **std::endl**.

Preprocessing flow:

1. **Source file** requests a header with **#include**.
2. **Preprocessor** replaces the directive with the header contents and expands macros.
3. **Compiler** receives a unified translation unit ready for syntax analysis.

Avoid stray spaces between the angle brackets and the header name; strict toolchains may refuse **#include &lt; iostream &gt;**.

**Offline drill:** Maintain a running index in **notes/preprocessor_diary.md** listing each header you use and the capability it unlocks. This local reference replaces web searches when you are offline.

---

## 1.3 Functions and the **main()** entry point

Every C++ program is built from functions—named blocks that perform a specific task. Execution always begins with the function named **main()**.

Annotated outline of **main**:

- **Header:** **int main()** expresses that the function returns an integer status code.
- **Opening brace:** marks the start of the body.
- **Statements:** perform work such as printing greetings or orchestrating other functions.
- **Return statement:** **return 0;** signals normal termination to the operating system.
- **Closing brace:** ends the function scope.

> **Reminder:** Only **main()** inherits the rule that reaching the closing brace is equivalent to returning zero. Every other function with a non-void return type must explicitly provide a value.

---

## 1.4 Statements, variables, and blocks

A statement ends with a semicolon and executes in sequence. Variables reserve storage with a type and a name:

- **int answer {42};** defines an integer called *answer* with value 42.
- **int attempts { 3 };** stores how many tries remain.
- **{ … }** pairs introduce a block (compound statement) that can nest inside other blocks to create scope boundaries.

Blocks act like single statements, so you can place them wherever a plain statement fits. Practise breaking long statements into multiple lines with inline comments explaining each piece.

---

## 1.5 Stream-based input and output

Streams abstract the idea of reading from or writing to a device. The Standard Library provides:

- **std::cout** — default output stream (typically your terminal window).
- **std::cin** — default input stream (usually your keyboard).
- **std::endl** — manipulator that appends a newline and flushes the buffer.

Typical output flow (describe it aloud while you type to cement the steps):

1. Insert descriptive text into **std::cout**.
2. Insert a variable value on the next line.
3. Finish with **std::endl** to move to a new line and push buffered characters to the screen.

The double-angle operator **<<** sends data to an output stream; in Chapter 2 you will meet **>>** for reading input.

---

## 1.6 Program termination and status codes

Returning zero from **main()** advertises success. Non-zero codes communicate problems to scripts or build pipelines. Catalogue a few meaningful values in your lab notebook, such as **1 = invalid input** or **2 = configuration missing**.

Pattern to follow when reporting an error:

1. Write an explanatory message to **std::cerr** (the error stream).
2. Return an agreed-upon non-zero code.

Explicit returns reinforce the habit for every other function you will write later.

---

## 1.7 Namespaces and qualified identifiers

Large projects risk name collisions. Namespaces reach the rescue by qualifying identifiers. The Standard Library lives inside **std**, so you reference the output stream as **std::cout**.

Scope recap:

- **Global scope:** symbols with no namespace prefix live here.
- **std::** scope: all Standard Library facilities including **cout**, **cin**, and **endl**.
- **Your custom namespaces:** such as **tsi::telemetry()** or **tsi::report()** to keep institutional helpers grouped.

Reserve `using namespace std;` for short experiments. Production code benefits from explicit prefixes or narrow **using** declarations inside functions.

> **Glossary exercise:** Extend your offline glossary with the terms *namespace*, *scope resolution operator*, and *qualified name*.

---

## 1.8 Legal identifiers and reserved keywords

Identifiers may contain letters, digits, and underscores, but must begin with a letter or an underscore. C++ is case-sensitive, so **Result**, **result**, and **RESULT** represent three different names. Resist the temptation to start identifiers with underscores—many combinations are reserved and can clash with compiler internals.

Avoid keywords such as **class**, **throw**, or **constexpr** for your own names. Favor descriptive choices like **remaining_attempts** or **sensor_reading** so future you understands the code without consulting documentation.

---

## 1.9 Objects, classes, and templates at a glance

Chapter 1 plants the vocabulary you will expand in later modules:

- A **class** is the blueprint describing data and behavior (think **Student** or **SensorReading**).
- An **object** is a live instance occupying memory, created from a class.
- A **template** is a recipe the compiler uses to generate type-specific code, such as **std::vector<int>** and **std::vector<std::string>**.

Recognising these terms now helps you read Standard Library documentation where streams, containers, and algorithms all manifest as templated classes and objects.

---

## 1.10 Code appearance and style conventions

Consistent presentation accelerates comprehension. Choose a brace style and stick with it. Two common layouts:

- **Allman style:** braces on their own lines, nested blocks clearly stacked.
- **One true brace style:** opening brace on the same line as the function header, closing brace aligned with the first keyword.

Complement structural decisions with naming conventions:

- Functions → verbs (**calculate_average**).
- Classes → nouns (**TemperatureSample**).
- Constants → uppercase with underscores (**MAX_RETRIES**).

Document *why* blocks exist, not *what* syntax already states. Purpose-driven comments help teammates understand decisions without reverse engineering the code.

---

## 1.11 Build pipeline overview

Transforming source into an executable involves three collaborators:

1. **Preprocessor** resolves directives (including header inclusion and macros).
2. **Compiler** converts each translation unit into an object file containing machine code.
3. **Linker** merges object files and required libraries into a runnable program.

When a linker reports an “undefined reference,” it means the declaration was seen but no corresponding definition made it into the final link step. Keep a custom flowchart or checklist on paper near your workstation to reason through build failures without online help.

---

## 1.12 Guided lab: build the Chapter 1 program

Perform this lab entirely offline to cement the workflow:

1. **Scaffold the file.** Create **chapter01/ex1_main.cpp** and add comments in your own words summarising what headers contribute, what the program prints, and why each line ends with a newline.
2. **Compile with warnings enabled.** Use the command **g++ -std=c++17 -Wall -Wextra -pedantic ex1_main.cpp -o ex1_main** and capture the output in your notes.
3. **Log artefacts.** Record the command line and resulting executable name in **notes/build_log.md** so you can rerun the build without hunting through history.
4. **Induce an error.** Temporarily remove the header inclusion, rebuild, and paste the diagnostic message into your log. Reflect on how the error relates to missing declarations.
5. **Restore success.** Reintroduce the header, rebuild, and attach either a screenshot or a textual confirmation proving the program runs as expected.

---

## 1.13 Offline micro missions inside VS Code

- **Mission A – Comment makeover:** Open **learn_exercises/c/chapter_01_basic_ideas_c.c** and rewrite the instructional comments using your voice while preserving the blueprint. Confirm the compiler still accepts the file because you did not alter the executable statements.
- **Mission B – Style audit:** Use multi-cursor editing (Alt or Option plus click) to align the double-angle operators in your output statements. Capture before and after snapshots to document the readability gain.
- **Mission C – Glossary flashcards:** Create **notes/glossary.csv** with columns **term** and **definition**. Add entries for *translation unit*, *stream*, *namespace*, and *template*, then review them at the start of each study session.

Keep all artefacts locally so you can review them without internet access.

---

## 1.14 Self-check questions

Record your answers in **reflection/chapter_01_answers.md**:

1. Why is purposeful whitespace essential even though the compiler mostly ignores it?
2. How does the preprocessor cooperate with the compiler when you use header inclusions?
3. Under what circumstances would you intentionally return a non-zero value from **main()**?
4. What problem do namespaces solve in large collaborative codebases?
5. Which naming patterns will you adopt to keep your programs readable for future maintainers?

---

## Troubleshooting guide

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| **g++: command not found** | Compiler toolchain missing from the system PATH | Install build tools (for example **sudo apt install build-essential**, **xcode-select --install**, or the MSYS2/MinGW toolchain) and reopen the terminal. |
| **undefined reference to main** | Active translation units do not define **int main()** | Ensure exactly one source file provides **main()** and participates in the link step. |
| **std::cout not declared** | Missing header or namespace qualification | Include **&lt;iostream&gt;** and qualify names with **std::** or declare a narrow **using** inside the relevant scope. |
| **Output missing or delayed** | Forgetting newline or flush manipulator | End lines with **\n** or **std::endl** and flush as needed. |

---

## Wrap-up checklist

- [ ] Compiled and executed the Chapter 1 program with warnings enabled.
- [ ] Captured evidence of both a failing and successful build in your notes.
- [ ] Completed the glossary, build log, and reflection prompts to support offline review.

When these boxes are checked, continue to Chapter 2 to explore fundamental types. Reach out if any activity in this lesson needs clarification or additional examples.
