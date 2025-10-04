# Regular expressions

Regular expressions (regex) describe patterns in text. Ruby ships with the Onigmo engine, supporting advanced features like named captures, lookarounds, Unicode properties, and extended mode formatting. Mastering regex lets you validate input, parse logs, and transform strings concisely—all while keeping code maintainable.

## Learning goals

- Construct regex literals and understand `Regexp`, `String`, and `MatchData` interactions.
- Apply anchors, character classes, quantifiers, alternation, and grouping.
- Use named captures, lookarounds, and Unicode properties for precise matching.
- Combine regex with `scan`, `match?`, `sub`/`gsub`, and `split` to query and refactor text.
- Structure complex patterns with extended mode, reuse via `Regexp.union`, and avoid catastrophic backtracking.

## Creating patterns

```ruby
pattern = /ruby/           # literal notation
regex   = Regexp.new("ruby", Regexp::IGNORECASE)

pattern =~ "Ruby"         # => 0 (match index) or nil
"Ruby".match?(pattern)    # => true (boolean)
```

`match?` performs a match without creating a `MatchData` object—use it for fast boolean checks. To capture details, call `match` or use `=~` and inspect `$~` (special global holding the last match).

## Anchors and boundaries

```ruby
/^start/      # beginning of string
/end$/        # end of string
\A\w+\Z      # whole string (multiline safe)
\bword\b     # word boundary
```

Use `\A`/`\z` (lowercase z) to restrict to the entire string, even in multiline mode. `^`/`$` respect line breaks when the `m` flag is enabled.

## Character classes and Unicode properties

```ruby
/[abc]/       # any of a, b, c
/[0-9]/       # digit
/\d/          # digit shortcut
/\h/          # hex digit (Onigmo extension)

/\p{Han}/     # any CJK ideograph
/\p{L}/       # any letter
```

Negate classes with `^` inside the brackets: `/[^\d]/` matches any non-digit. Combine ranges and literals: `/[A-Fa-f0-9]/` for hex.

## Quantifiers and greediness

```ruby
/t.*t/        # greedy (matches the longest span)
/t.*?t/       # non-greedy (shortest span)
/a{3}/        # exactly three
/a{2,5}/      # between two and five
/a{2,}/       # at least two
```

Be mindful of greedy quantifiers when matching delimiters; switch to `*?`, `+?`, or more specific patterns to avoid overconsumption.

## Grouping and alternation

```ruby
/(cat|dog)/   # alternation
/(?:cat|dog)/ # non-capturing group
```

Use non-capturing groups `(?:...)` when you don’t need the submatch; this keeps `MatchData` indexes tidy.

## Capturing data

```ruby
text = "Price: $19.99"
match = text.match(/\$(?<dollars>\d+)\.(?<cents>\d{2})/)
match[:dollars] # => "19"
match[:cents]   # => "99"
match.captures  # => ["19", "99"]
```

Named captures and `MatchData` helpers (`pre_match`, `post_match`) make extraction concise. Reuse named captures across repetitions by referencing them with `\k<name>`.

## Lookarounds (zero-width assertions)

```ruby
/(?<=USD)\d+/     # lookbehind: digits preceded by "USD"
/\d+(?= items)/   # lookahead: digits followed by " items"
/(?<!not )ok/     # negative lookbehind
/ok(?!ay)/        # negative lookahead
```

Lookarounds validate context without consuming characters—ideal for selective replacements.

## Working with `MatchData`

```ruby
if (m = "abc123".match(/(?<letters>\w+?)(?<digits>\d+)/))
  m[0]          # "abc123"
  m[:letters]   # "abc"
  m[:digits]    # "123"
  m.named_captures # {"letters"=>"abc", "digits"=>"123"}
end
```

`MatchData` responds to `captures`, `offset`, `begin`, `end` to locate submatches precisely.

## String helpers

```ruby
text.scan(/\d+/)                        # all numeric substrings
text.gsub(/\s+/, " ")                  # collapse whitespace
text.sub(/\A\s+/, "")                 # remove leading whitespace once
tokens = text.split(/[\s,;]+/)         # tokenize by delimiters
```

`gsub` accepts a block or hash:

```ruby
message.gsub(/\b(yes|no)\b/, "yes" => "👍", "no" => "👎")
```

Inside a block, the current match is provided as a string argument; use `$~` if you need captures.

## Flags and extended mode

```ruby
regex = /
  \A                 # start
  (?<username>\w+)   # name
  @
  (?<domain>[\w.-]+) # domain
  \z
/x

regex.match?("user@example.com")
```

- `i` — case-insensitive
- `m` — multiline (`.` matches newline)
- `x` — extended/verbose (ignores unescaped whitespace and allows comments)
- `o` — interpolate once when using `#{}` inside a literal

Combine flags by suffixing them to the literal or passing to `Regexp.new`.

## Composition with `Regexp.union`

Build complex patterns safely:

```ruby
keywords = %w[ruby rails rack]
pattern = Regexp.union(*keywords)
pattern # => /ruby|rails|rack/
```

`Regexp.escape(string)` escapes metacharacters when interpolating user input.

## Avoiding pitfalls

- Watch for catastrophic backtracking in nested quantifiers (e.g., `(.*a){10}`); tighten patterns or use possessive quantifiers (`*+`, `++`) when available.
- Prefer `match?` over `match` when you don’t need submatches; it’s GC-friendly.
- Anchor patterns for validation (`\A...\z`) to avoid partial matches.
- Test patterns with representative data, including edge cases and Unicode input.

## Guided practice

1. **Log parser**
   - Extract timestamp, log level, and message from lines like `2025-10-03T12:00:00Z [WARN] Disk nearly full`.
   - Use named captures and convert matches into hashes.

2. **Markdown link replacer**
   - Convert `[text](url)` to HTML `<a>` tags using `gsub` with captures.
   - Handle nested brackets inside the link text.

3. **Feature flag tokenizer**
   - Split a string like `beta:on, dark_mode:off` into key/value pairs.
   - Normalize keys to symbols and values to booleans.

4. **Password validator**
   - Implement `valid_password?(string)` ensuring: ≥12 chars, at least one upper/lower/digit/symbol, and no whitespace.
   - Combine regex tests with `match?` and lookaheads.

5. **CSV sanitization**
   - Detect fields containing unescaped quotes or unexpected control characters using Unicode properties (`\p{Cntrl}`).
   - Replace invalid bytes with placeholders and report positions.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Regular Expressions (Appendix — regex-ruby-appendix)

Notes and quick recipes for using Ruby regular expressions, common flags, and testing patterns.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Why</th><th>Reference</th></tr>
  </thead>
  <tbody>
    <tr><td>Basic syntax</td><td>Pattern matching</td><td>Ruby Regexp docs</td></tr>
    <tr><td>Groups & captures</td><td>Extracting submatches</td><td>Use named captures for clarity</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (regex-ruby-appendix)

1. Write a small parser that extracts email addresses from a text sample and add tests verifying edge cases.
2. Demonstrate named captures and show how to use them in a transform pipeline.

<!-- markdownlint-disable MD010 -->

### Practical Appendix: Regular Expressions — Resources & Recipes (Appendix — External Links)

Authoritative docs and safe usage patterns for regex in Ruby.

- Ruby Regexp docs: [Ruby Regexp docs](https://ruby-doc.org/core/Regexp.html)
- Practical guide: [Rubular (interactive tester)](https://rubular.com/)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Link</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Basic syntax</td><td><a href="https://ruby-doc.org/core/Regexp.html">Regexp docs</a></td><td>Use non-capturing groups when you don't need captures</td></tr>
    <tr><td>Performance</td><td><a href="https://rubular.com/">Rubular</a></td><td>Test expressions interactively</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Replace a fragile parsing script with a dedicated regex using named captures and tests.
2. Add a performance checklist for regex-heavy paths (precompile, avoid catastrophic backtracking).

<!-- markdownlint-disable MD010 -->

## Self-check questions

1. When should you reach for `match?` instead of `match`, and what performance difference does it make?
2. How do named captures improve readability over positional captures, and how can you access them from `MatchData`?
3. What situations call for lookahead/lookbehind assertions, and how do they differ from consuming matches?
4. How does extended mode (`/x`) help manage complex patterns, and what must you escape when using it?
5. Which strategies help prevent catastrophic backtracking in large or user-supplied input?

Regex can be powerful but opaque—treat them like code: keep them readable, validate edge cases, and extract them into well-named constants. With thoughtful patterns, you’ll parse and validate text confidently across your Ruby projects.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Regular Expressions — Deep Dive (Appendix II — External Links)

Examples for named captures, non-capturing groups, and performance tips.

```ruby
# named captures example
m = /(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/.match('2025-10-05')
puts m[:year] if m
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Link</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Named captures</td><td><a href="https://ruby-doc.org/core/Regexp.html">Regexp docs</a></td><td>Use for clearer extraction</td></tr>
    <tr><td>Testing</td><td><a href="https://rubular.com/">Rubular</a></td><td>Interactive testing</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix II — regex)

1. Replace ad-hoc parsing with a named-capture regex and add tests.
2. Add a short performance note about avoiding catastrophic backtracking and provide an example.

<!-- markdownlint-disable MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Regular Expressions — Recipes & Tests (Appendix — regex-ruby-appendix2)

Named-capture examples, safe interpolation, and quick performance notes to avoid catastrophic backtracking.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Strategy</th><th>Why</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Use named captures</td><td>Readable extraction</td><td>m[:name] to access</td></tr>
    <tr><td>Escape user input</td><td>Avoid injection into patterns</td><td>Regexp.escape(user_input)</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Named-capture example

```ruby
m = /(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/.match('2025-10-05')
puts m[:year] if m
```

### Avoiding backtracking

- Prefer explicit bounded quantifiers over `.*` when possible.
- Use `\A` and `\z` to anchor whole-string validations.

### Exercises (Appendix — regex-ruby-appendix2)

1. Build a named-capture regex that extracts timestamp, level, and message from a log line and add tests for edge cases.
2. Replace a fragile parser with `Regexp.union` and ensure user input is escaped before interpolation.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
