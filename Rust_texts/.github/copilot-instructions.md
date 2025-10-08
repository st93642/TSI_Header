```instructions
# AI Coding Agent Instructions for Rust Tutorial Project

## Project Overview
This is a comprehensive Rust programming tutorial organized as a series of markdown chapters. Each chapter builds on the previous ones, covering Rust fundamentals from installation to advanced concepts like ownership, structs, and beyond.

## Key Conventions
- **File Structure**: One chapter per .md file, with sequential numbering
- **Content Format**: Standard markdown with sections, code listings, and explanatory text
- **Code Examples**: Rust code in fenced code blocks with `rust` language tags
- **Listing References**: Code blocks labeled as "Listing X-Y" with filenames like `src/main.rs`
- **Rust Edition**: Target Rust 2024 edition (as seen in Chapter1.md Cargo.toml examples)

## Essential Patterns
- **Ownership Rules**: Always demonstrate proper ownership, borrowing, and lifetimes in examples
- **Error Handling**: Use `Result` and `Option` appropriately; avoid `unwrap()` in production-like code
- **Idiomatic Rust**: Prefer iterators over loops, pattern matching, and functional styles
- **Documentation**: Include clear explanations of why code works, not just what it does

## Code Example Guidelines
- **Compilation**: All Rust code blocks must compile successfully
- **Minimal but Complete**: Provide runnable examples with `fn main()` where appropriate
- **Progressive Complexity**: Start simple, build up concepts across chapters
- **Real-World Relevance**: Use practical examples (e.g., user accounts, rectangles) over abstract ones

## Editing Workflow
- **Chapter Continuity**: Ensure new content fits the tutorial progression
- **Cross-References**: Link to related sections in other chapters when introducing concepts
- **Consistency**: Maintain the same writing style, terminology, and formatting as existing chapters
- **Validation**: After edits, verify code examples still compile and demonstrate concepts correctly

## Common Pitfalls to Avoid
- **Outdated Syntax**: Use modern Rust features; avoid deprecated patterns
- **Memory Safety Violations**: Never demonstrate code that would cause borrow checker errors
- **Incomplete Explanations**: Always explain ownership moves, borrows, and why code is safe
- **Generic Advice**: Focus on this tutorial's specific Rust teaching approach, not general programming tips

```

## Style Guide

This project follows a consistent markdown and code style so readers have a predictable, high-quality learning experience. Additions to the style here should be conservative and backwards-compatible with the existing chapters.

High-level rules
- Use American English and the Oxford comma.
- Keep sentences clear and concise. Prefer active voice.
- Maintain the existing tutorial progression; new examples should not assume knowledge from later chapters.

Markdown formatting
- Top-level chapter files should start with a single H1 heading ("# Chapter X: Title" or similar). Do not wrap the entire file in a code fence.
- Use fenced code blocks with language tags for source and output examples: ```rust for Rust code and ```text for terminal/compile output.
- Always place a blank line before and after a fenced code block.
- Use inline code ticks for types, generics, and other code-like items in prose. Example: `Vec<T>`, `HashMap<K, V>`, `Option<i32>`.
- Do not use raw angle-bracket expressions (`Vec<T>`) in prose without backticks — they can be parsed as HTML by linters (MD033).
- In terminal output or backtrace examples within ```text blocks, backtick angle brackets (e.g., `<T>`) to avoid MD033/no-inline-html warnings.
- Preserve existing "Listing X-Y" captions and single-line filename references above or below listings when present.
- Avoid HTML in markdown; if you must include special characters, prefer backticks or HTML entities.

Code examples
- Rust examples must compile. Prefer minimal, focused examples that demonstrate a single concept.
- When demonstrating failing code, clearly mark it in the prose and in a fenced ```rust block comment (e.g. // [This code does not compile!]) and include the `cargo` output in a separate ```text block.
- Avoid `unwrap()` and other panic-prone helpers in production-like examples; prefer `match`, `if let`, or `?` where appropriate and explain the choice.
- Include `fn main()` for runnable snippets unless the listing is intentionally a fragment.

Linting and whitespace
- Do not leave trailing spaces at line ends (fixes MD009).
- Ensure blank lines surround fenced code blocks (fixes MD031).
- Wrap angle-bracketed types in backticks in prose and terminal output to avoid MD033/no-inline-html warnings.
- Preferred code-block style: fenced blocks with language tags for readability and syntax highlighting. If the repository linter flags MD046, update the repo markdownlint config to allow fenced blocks with language tags instead of converting examples to indented blocks.

Commands to run locally (recommended)
Run markdownlint (if you have it installed) and the Rust checks before opening PRs:

```bash
# lint markdown (example using markdownlint-cli)
npx markdownlint '**/*.md' --config .markdownlint.json

# check Rust examples (from the book root or an example crate)
cargo check

# run tests (if any)
cargo test
```

Editing workflow / PR conventions
- Re-read the target file immediately before applying an edit to avoid context mismatches when batching small patches.
- Make small, focused commits with clear messages (e.g., "Chapter 7: backtick generics in prose; convert Listing 8-1 to fenced rust block").
- If a change touches many chapters, split into multiple PRs organized by scope (formatting pass, then content edits, then tests).

Quality gates
- Before merging: ensure no syntactic markdownlinter errors remain and that any changed Rust examples compile (cargo check).
- Include a short PR description that maps changes to the style rules (e.g., "Wrap generics in backticks to fix MD033; convert indented listings to fenced blocks").

Contract (for editors and automated agents)
- Inputs: a markdown chapter file.
- Outputs: the same file with consistent formatting (backticked generics, fenced code blocks, no trailing spaces, blank lines around fences).
- Failure modes: edits that modify code semantics inside fenced code examples — avoid; linter rule conflicts (MD046) — surface and propose config change.

Common edge cases
- Don't change code inside fenced blocks. If you need to update code examples, ensure they compile after the change.
- When a listing is intentionally indented (to show YAML or a preformatted block that is not code), preserve that style.
- When in doubt about MD046 (code-block-style), prefer readability (fenced + language tag) and propose a lint config change.

Proactive extras
- When making formatting-only commits, include a brief smoke test: `cargo check` if examples are compiled in-repo, or a note in the PR if they are snippets only.
- Add a single-line note in the chapter's changelog (if present) describing the formatting pass.

If you want me to enforce any stricter/alternate rules (for example, convert all fenced blocks to indented blocks for linter parity), say so and I will apply that policy consistently across the chapters.

``` # AI Coding Agent Instructions for Rust Tutorial Project

## Project Overview
This is a comprehensive Rust programming tutorial organized as a series of markdown chapters. Each chapter builds on the previous ones, covering Rust fundamentals from installation to advanced concepts like ownership, structs, and beyond.

## Key Conventions
- **File Structure**: One chapter per .md file, with sequential numbering
- **Content Format**: Standard markdown with sections, code listings, and explanatory text
- **Code Examples**: Rust code in fenced code blocks with `rust` language tags
- **Listing References**: Code blocks labeled as "Listing X-Y" with filenames like `src/main.rs`
- **Rust Edition**: Target Rust 2024 edition (as seen in Chapter1.md Cargo.toml examples)

## Essential Patterns
- **Ownership Rules**: Always demonstrate proper ownership, borrowing, and lifetimes in examples
- **Error Handling**: Use `Result` and `Option` appropriately; avoid `unwrap()` in production-like code
- **Idiomatic Rust**: Prefer iterators over loops, pattern matching, and functional styles
- **Documentation**: Include clear explanations of why code works, not just what it does

## Code Example Guidelines
- **Compilation**: All Rust code blocks must compile successfully
- **Minimal but Complete**: Provide runnable examples with `fn main()` where appropriate
- **Progressive Complexity**: Start simple, build up concepts across chapters
- **Real-World Relevance**: Use practical examples (e.g., user accounts, rectangles) over abstract ones

## Editing Workflow
- **Chapter Continuity**: Ensure new content fits the tutorial progression
- **Cross-References**: Link to related sections in other chapters when introducing concepts
- **Consistency**: Maintain the same writing style, terminology, and formatting as existing chapters
- **Validation**: After edits, verify code examples still compile and demonstrate concepts correctly

## Common Pitfalls to Avoid
- **Outdated Syntax**: Use modern Rust features; avoid deprecated patterns
- **Memory Safety Violations**: Never demonstrate code that would cause borrow checker errors
- **Incomplete Explanations**: Always explain ownership moves, borrows, and why code is safe
- **Generic Advice**: Focus on this tutorial's specific Rust teaching approach, not general programming tips
