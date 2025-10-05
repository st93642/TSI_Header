# Working with Strings

Strings are everywhere: configuration files, API payloads, command-line flags, UI messages…the list goes on. Ruby treats strings as first-class citizens, providing rich APIs for creation, interrogation, transformation, encoding, and performance tuning. This lesson digs deep to show you how to model text in idiomatic, expressive Ruby.

## Learning goals

- Create strings using literal syntax, heredocs, percent notation, and interpolation.
- Manipulate strings with core methods for case, substitution, slicing, and enumeration.
- Understand Ruby’s encoding model and how to work safely with Unicode and byte-based operations.
- Optimize string usage by freezing, reusing, or building efficiently with `StringIO`.
- Practice debugging and sanitizing strings that come from users or external systems.

## Creating strings

Ruby offers multiple literal syntaxes:

```ruby
single = 'Hello'
double = "World"
escaped = "He said \"hi\""

emoji = "😀"
multiline = "Line 1\nLine 2"

percent_q = %q{single-quoted behavior without escaping 'quotes'}
percent_Q = %Q{double-quoted with interpolation #{1 + 1}}
```

Use heredocs for multi-line text, especially when embedding indentation-sensitive content.

```ruby
report = <<~TEXT
  Dear #{recipient},
  Your balance is #{format('%.2f', balance)} credits.
TEXT

puts report
```

`<<~` (squiggly heredoc) strips common indentation, keeping your Ruby code tidy.

## Single vs. double quotes

- **Single quotes (`'`)** treat contents literally except for `\'` and `\\`.
- **Double quotes (`"`)** interpret escape sequences and allow interpolation via `#{...}`.

```ruby
name = "Bob"
"Hello, #{name}!"   # => "Hello, Bob!"
'Hello, #{name}!'    # => "Hello, #{name}!"

"Tab\tSeparated"    # => "Tab\tSeparated"
'Tab\tSeparated'     # => "Tab\tSeparated"
```

Prefer interpolation over manual concatenation; it’s clearer and avoids stray whitespace or conversion bugs.

## Inspecting and slicing

Strings support array-like access:

```ruby
word = "Ruby"
word[0]       # => "R"
word[-1]      # => "y"
word[1, 2]    # => "ub"
word[0..2]    # => "Rub"
word.slice!(0) # removes and returns "R"
```

Use `String#chars`, `#each_char`, or `#bytes` depending on whether you want characters or raw byte values.

```ruby
"Hi".bytes       # => [72, 105]
"こんにちは".bytes # => UTF-8 bytes of each multibyte character
```

## Core transformation methods

- `upcase`, `downcase`, `capitalize`, `swapcase`
- `strip`, `lstrip`, `rstrip` remove whitespace.
- `chomp` removes trailing newlines (great with `gets`).
- `gsub` and `sub` replace substrings or patterns; `gsub!` mutates in place.

```ruby
message = "  Hello, Ruby!  \n"
clean = message.strip
clean.gsub!(/Ruby/, "World")
puts clean # => "Hello, World!"
```

Use `tr` and `tr_s` for character-by-character translation and squeeze duplicates.

## Enumerating and searching

Strings include `Enumerable` via `each_line`: iterate over lines with ease.

```ruby
data = "first\nsecond\nthird"
data.each_line.with_index do |line, index|
  puts "#{index}: #{line.chomp}"
end
```

`include?`, `start_with?`, and `end_with?` answer substring questions quickly.

```ruby
path = "/usr/local/bin"
puts path.start_with?("/usr")  # true
puts path.end_with?("bin")      # true
```

## String + Regex: power duo

Regular expressions amplify strings. `scan`, `match`, and `split` parse complex input.

```ruby
email = "user@example.com"

if email.match?(/\A\w+@\w+\.\w+\z/)
  puts "Looks like an email!"
end

words = "ruby,rails,sinatra"
words.split(/[,;]/) # => ["ruby", "rails", "sinatra"]

"The price is $12.34".scan(/[0-9]+\.[0-9]{2}/)
# => ["12.34"]
```

Named captures make extraction readable:

```ruby
log = "GET /items/42 200"
pattern = /(?<verb>GET|POST) \/items\/(?<id>\d+)/

if (md = log.match(pattern))
  puts md[:verb] # "GET"
  puts md[:id]   # "42"
end
```

## Unicode and encoding awareness

Ruby treats strings as byte sequences tagged with encodings (UTF-8 by default). Check or change encodings with `#encoding` and `#encode`.

```ruby
greeting = "Olá"
puts greeting.encoding          # => #<Encoding:UTF-8>
puts greeting.encode("ISO-8859-1")
```

Use `force_encoding` only when you are certain the underlying bytes actually represent text in another encoding.

## Freezing and immutability

Strings are mutable by default. To prevent accidental changes (especially for constants), freeze them:

```ruby
APP_NAME = "TSI Portal".freeze
APP_NAME << "!" # raises FrozenError
```

Add `# frozen_string_literal: true` at the top of your Ruby files to auto-freeze all string literals in that file, improving performance by avoiding duplicate string allocations.

## Building strings efficiently

Repeated concatenation can allocate many intermediate strings. Prefer `String#<<`, `Array#join`, or `StringIO` when building large strings.

```ruby
buffer = StringIO.new
buffer << "Report\n"
buffer << "------\n"
items.each { |item| buffer << "- #{item}\n" }

puts buffer.string
```

Alternatively, use `Array#join`:

```ruby
lines = ["Report", "------"] + items.map { |item| "- #{item}" }
puts lines.join("\n")
```

## Secure and clean text processing

- Normalize whitespace with `squeeze(" ")` or custom regex replacements.
- Sanitize input before embedding into HTML using `CGI.escapeHTML` from the standard library.
- When interpolating into shell commands, use libraries like `Shellwords` to escape arguments.

## Internationalization helpers

Ruby’s `i18n` gem (used in Rails) leans heavily on strings. Keep translations external, but remember strings can store placeholders `%{name}` that you interpolate via `I18n.t("greeting", name: "Ada")`.

## Diagnostics and debugging

- `p` shows quoted strings; great when diagnosing invisible characters.
- `String#dump` returns a debugger-friendly representation with escape sequences.

```ruby
puts "Line\nBreak".dump # => "\"Line\\nBreak\""
```

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Strings — Encoding, Performance & Tests (Appendix — strings-ruby2)

Practical recipes and gotchas when working with Ruby strings in real projects: encoding, interpolation, templating, memory usage, and test strategies.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>When to use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Encoding</td><td>Multilingual input/output</td><td>Use `#encoding` and `String#force_encoding` carefully</td></tr>
    <tr><td>Interpolation</td><td>Templates / messages</td><td>Prefer interpolation over concatenation for readability</td></tr>
    <tr><td>Frozen strings</td><td>Performance & immutability</td><td>Use `#freeze` or `--enable-frozen-string-literal` where appropriate</td></tr>
    <tr><td>Byte processing</td><td>Binary protocols</td><td>Use `String#bytes` or `String#b` for binary-safe ops</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Encoding basics

Ruby strings carry an associated encoding (for example UTF-8). When reading files or network data, ensure the correct encoding is used:

```ruby
# read a UTF-8 file
text = File.read('doc.md', encoding: 'UTF-8')

# check encoding
puts text.encoding #=> #<Encoding:UTF-8>

# convert to UTF-8 safely
utf8 = text.encode('UTF-8')
```

If you receive a byte sequence in an unknown encoding, avoid `force_encoding` unless you know the input's bytes represent that encoding. Prefer `encode` with error handling:

```ruby
safe = raw_string.encode('UTF-8', invalid: :replace, undef: :replace, replace: '?')
```

### Interpolation vs concatenation

Interpolated strings are clearer and usually faster than repeated concatenation:

```ruby
name = 'Alice'
# interpolation
msg = "Hello, #{name}!"
# concatenation
msg2 = 'Hello, ' + name + '!'
```

For many fragments, use `String#<<` to avoid creating many intermediate strings in loops:

```ruby
out = String.new
items.each { |it| out << it << ',' }
```

### Freezing and deduplication

Freezing short, repeated strings can reduce GC pressure in long-running processes.

```ruby
KEYS = %w[id name email].map!(&:freeze)
# or enable frozen string literals with a magic comment
# frozen_string_literal: true
```

Ruby 3+ and modern toolchains deduplicate and optimize interned strings; measure before optimizing prematurely.

### Binary-safe operations

When operating on binary protocols or checksums, work on the byte level:

```ruby
bytes = str.b # ensures ASCII-8BIT (binary) view
checksum = Digest::SHA256.hexdigest(bytes)
```

### Regex tips for strings

- Use non-capturing groups `(?:...)` when captures are unnecessary.
- Prefer `` carefully — Unicode word boundaries depend on encoding.
- Use `String#match?` for boolean checks (avoids allocating MatchData):

```ruby
if str.match?(/\A\d{4}-\d{2}-\d{2}\z/)
  # date-like
end
```

### Performance considerations

- Avoid repeated `gsub` allocations in tight loops; consider `String#tr` for single-character transforms.
- For joining many fragments, build into an array and `join` once, or use `String#<<` to append.

Benchmark pattern example (micro-benchmark):

```ruby
require 'benchmark'

n = 50_000
Benchmark.bm do |x|
  x.report('concat') do
    s = ''
    n.times { s = s + 'a' }
  end

  x.report('<<') do
    s = ''
    n.times { s << 'a' }
  end

  x.report('array_join') do
    a = []
    n.times { a << 'a' }
    a.join
  end
end
```

### Templating & safe interpolation

When rendering user-provided input into templates, sanitize or escape to avoid injections (HTML/JSON). Prefer library templating helpers that escape automatically for the target (ERB with `h` helper or use a safe templating library).

### Tests for string behavior

- Use `assert_equal` for deterministic outputs.
- Use `assert_match` for regex checks.
- Use `refute` / `refute_match` for absence checks.

Example Minitest:

```ruby
require 'minitest/autorun'

class TestStrings < Minitest::Test
  def test_interpolation
    name = 'Zoë'
    assert_equal "Hello, Zoë!", "Hello, #{name}!"
  end

  def test_encoding_safe
    raw = "caf\xC3\xA9".b # bytes representing 'café' in UTF-8
    s = raw.encode('UTF-8')
    assert_equal 'café', s
  end
end
```

### Exercises (Appendix — strings-ruby2)

1. Write a `safe_truncate(str, max_len)` that truncates to a maximum number of characters without splitting multi-byte characters and write tests ensuring UTF-8 safety.
2. Implement a template renderer that safely escapes HTML by default and write tests verifying escaping occurs for user input but not for safe HTML fragments.
3. Benchmark three concatenation strategies over 100k iterations and report allocations and time; add tests asserting that the fastest strategy is at least 20% faster than the slowest on your machine.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

## Guided practice

1. **Template builder**
   - Create a heredoc email template with placeholders for name, due_date, and total.
   - Write a method `render_invoice(name:, due_date:, total:)` that interpolates values and returns the string.

2. **Log parser**
   - Given lines like `"INFO 2025-10-03T11:45:00Z Job completed in 3.42s"`, use regex named captures to extract the level, timestamp, and duration.
   - Convert the duration substring to a float using `to_f`.

3. **Unicode inspector**
   - Ask the user for a phrase containing emojis.
   - Print each character with its byte length and codepoint (`char.ord`).
   - Report whether the string is valid UTF-8 using `valid_encoding?`.

4. **String builder benchmark**
   - Measure the time taken to concatenate 10_000 numbers into a single string using `+=` versus `StringIO`.
   - Use `Benchmark.measure` (from `benchmark`) to compare.

5. **Sanitizer utility**
   - Write a method that trims whitespace, collapses multiple spaces into one, removes control characters (regex `\p{Cntrl}`), and optionally escapes HTML.
   - Test it on messy user input.

## Self-check questions

1. When should you choose heredocs over concatenated strings?
2. How do you safely insert variable content into a string that will later be displayed in HTML?
3. What is the difference between `gsub` and `gsub!`, and why might freeze prevent the latter from working?
4. How does Ruby represent encodings internally, and what methods help you detect encoding issues?
5. Why is `StringIO` often more efficient than using `+=` in large loops?

Master these string techniques and your Ruby programs will speak clearly, robustly, and efficiently—no matter where the text comes from or where it needs to go next.
