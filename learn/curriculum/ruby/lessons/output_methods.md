# Building and Displaying Strings in Ruby

Concatenation is only the beginning. In real applications you will combine
strings from user input, configuration files, network responses, and
calculations—all while keeping the code expressive and efficient. This lesson
expands your toolbox for creating, formatting, and outputting strings in Ruby.

## Learning goals

- Compare multiple strategies for combining strings and understand when each
  shines.
- Interpolate values, call `format`/`sprintf`, and leverage `Array#join` for
  templated output.
- Control newlines, whitespace, and encoding when printing to the console or
  files.
- Reuse buffers and builders to avoid unnecessary allocations in tight loops.
- Follow best practices for returning strings from methods while keeping
  presentation logic separate.

## Concatenation with `+` and `concat`

```ruby
first = "Hello"
last  = "World"
result = first + " " + last       # => "Hello World"

greeting = "Hello" + ", " + "Alice" + "!"
```

`+` creates a new string each time. When concatenating inside loops, that can create many intermediate objects. Prefer `concat` or `<<` (shown later) for in-place updates.

```ruby
message = "Hello"
message.concat(", ", "Ruby")
puts message # => "Hello, Ruby"
```

## String interpolation

The idiomatic Ruby approach: embed values with `#{...}` inside double-quoted strings.

```ruby
name = "Ada"
age  = 28
greeting = "Hello, #{name}! You are #{age} years old."

temperature_c = 21
puts "It feels like #{temperature_c * 9.0 / 5 + 32}°F outside."
```

Interpolation automatically calls `to_s` on embedded objects. Define your own `to_s` to control how custom objects render.

```ruby
Invoice = Struct.new(:id, :total) do
  def to_s = "Invoice ##{id} ($#{format('%.2f', total)})"
end

invoice = Invoice.new(42, 199.95)
puts "Processing #{invoice}"
```

## Append in place with `<<`

`<<` (also called the shovel operator) mutates the original string, avoiding extra allocations.

```ruby
buffer = "Hello"
buffer << ", " << "world"
buffer << "!"

puts buffer # => "Hello, world!"
```

`<<` works with other data types as long as they respond to `to_str` or `to_s`. Paired with loops or enumerators, it builds strings efficiently.

## Joining arrays and enumerables

`Array#join` is a clean, fast way to stitch collections together.

```ruby
names = ["Ada", "Grace", "Linus"]
puts names.join(", ")              # => "Ada, Grace, Linus"

lines = ["Report", "------", "Item 1"]
puts lines.join("\n")
```

The Enumerable method `map(&:to_s).join` converts arbitrary elements to strings before joining.

## Formatting with `format` / `sprintf`

When you need precise spacing or numeric alignment, use `format` (alias `sprintf`).

```ruby
name = "Alice"
balance = 1520.5
puts format("%-10s %10.2f", name, balance)
# => "Alice          1520.50"

percentage = 0.756
puts format("%6.2f%%", percentage * 100) # => " 75.60%"
```

`format` returns the string; `printf` prints immediately. Use `format` when you want to return strings from methods and let the caller decide how to display them.

## Heredocs and templates

Use heredocs to assemble structured text (emails, SQL queries, markdown
snippets) with embedded interpolation.

```ruby
def build_welcome(name:, plan:, features: [])
  <<~MESSAGE
    Hello #{name},

    Welcome to the #{plan} plan. You now have access to:
    #{features.map { |feature| "  • #{feature}" }.join("\n")}

    Happy coding!
  MESSAGE
end

puts build_welcome(name: "Maya", plan: "Pro", features: %w[Priority-Support Analytics])
```

Heredocs keep markup readable while preserving indentation.

## Formatting helpers beyond the core

- `String#ljust`, `#rjust`, `#center` align text.
- `String#prepend` adds to the front.
- `StringIO` (from the standard library) acts like a file-backed string buffer
  for efficient building.
- Libraries like `ERB` or `Mustache` provide templating engines when
  interpolation becomes too complex.

```ruby
column_title = "Score".rjust(6)
puts column_title # => " Score"
```

## Returning vs. printing

Keep logic and presentation separate. Methods should generally return strings,
leaving the caller to decide whether to print or log them.

```ruby
def greeting_for(name)
  "Hello, #{name}!"
end

puts greeting_for("Ada")      # print now
File.write("greeting.txt", greeting_for("Grace"))
```

Use `puts`, `print`, or `warn` only at IO boundaries (command line interfaces, logs, etc.). Returning strings makes code more testable.

## Handling whitespace and newlines

- `String#strip` removes leading/trailing whitespace.
- `String#chomp` removes a trailing newline without affecting other whitespace.
- Use `Array#join("\n")` instead of manual `"\n"` concatenation to ensure
  exactly one newline between items.

```ruby
lines = ["first", "second", "third"]
puts lines.join("\n")
```

## Encoding-aware concatenation

Ruby enforces encoding compatibility. Concatenating strings with incompatible encodings raises `Encoding::CompatibilityError`. Normalize encodings ahead of time:

```ruby
utf8 = "Olá"
latin = "Olá".encode("ISO-8859-1")

utf8 + latin # raises
utf8 + latin.encode("UTF-8") # works
```

## Performance tips

- Use `<<` or `StringIO` in loops to avoid excessive object creation.
- Freeze repeated string literals (`"OK".freeze`) or enable `#
  frozen_string_literal: true` to reuse them.
- Cache formatted strings when they recur frequently.

## Safe shell and HTML output

When composing command-line strings, use `Shellwords.join` to escape arguments. For HTML, `ERB::Util.h` or `CGI.escapeHTML` prevents injection.

```ruby
require "shellwords"
cmd = ["grep", "-r", "Hello World", "."].shelljoin
system(cmd)
```

## Guided practice

1. **Reusable greeting**
   - Write a method `personalized_greeting(name:, title: nil)` that returns a
     formatted string.
   - If `title` is provided, include it (`"Hello, Dr. Ada"`), otherwise use the
     name alone.

2. **Table renderer**
   - Given an array of hashes with `:label` and `:value`, build a two-column
     table aligned with `ljust`/`rjust`.
   - Return the table as a string; print it later.

3. **Log formatter**
   - Implement `format_log(level:, message:, timestamp: Time.now)` returning a
     string like `[2025-10-03 12:00:00][INFO] message`.
   - Ensure level is uppercased and padded to 5 characters.

4. **Bulk builder benchmark**
   - Compare building a 1,000-line report using `+=` vs. `StringIO`.
   - Print the elapsed time for each approach and discuss the difference.

5. **Email template with lists**
   - Create a heredoc template that lists next steps as bullet points by joining
     an array with newlines.
   - Ensure trailing newlines are trimmed with `strip` before returning the
     final string.

## Self-check questions

1. When is concatenation with `+` acceptable, and when should you prefer `<<` or
`StringIO`?
2. How does string interpolation choose which method (`to_s`, `to_str`) to call
   on embedded objects?
3. Why is it beneficial for functions to return strings instead of printing them
   directly?
4. How can `format` help you build tables or reports with aligned columns?
5. What steps prevent encoding or injection issues when combining strings from
   user-supplied data?

Mastering Ruby’s string composition patterns gives you the power to craft clear
messages, reusable templates, and world-class developer tools. Combine these
strategies thoughtfully to keep your output clean, safe, and efficient.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: External Tools & Examples (Appendix — External Tools — output_methods-ruby)

Short references for common output helpers in Ruby and examples showing `puts`, `print`, `p`, and `printf` behavior.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Method</th><th>Effect</th><th>Reference</th></tr>
  </thead>
  <tbody>
    <tr><td>puts</td><td>Appends newline</td><td>IO docs</td></tr>
    <tr><td>print</td><td>No newline</td><td>IO docs</td></tr>
    <tr><td>p</td><td>Inspect then newline</td><td>Kernel#p</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercise (output_methods-ruby)

1. Capture stdout with `StringIO` and write tests asserting the exact output of
`puts`, `print`, and `p` for the same inputs.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Output Methods — Logging, Formatting & Test Capture (Appendix — output_methods-ruby2)

Practical recipes for structured logging, formatting output for different audiences, and capturing output in tests.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Use</th><th>Tool</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Simple output</td><td>puts / print</td><td>Human-readable, not structured</td></tr>
    <tr><td>Logging</td><td>Logger</td><td>Use levels and rotate files in production</td></tr>
    <tr><td>Test capture</td><td>capture_io / Open3</td><td>Assert stdout/stderr in tests</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: Logger

```ruby
require 'logger'
log = Logger.new(STDOUT)
log.level = Logger::INFO
log.info('Started')
```

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — output_methods-ruby2)

1. Replace ad-hoc `puts` calls with `Logger` and add tests that capture log
   output with different levels.
2. Format tabular data for console output and write tests that assert column
   alignment for sample data.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Output Methods — I18n & Formatting Libraries (Appendix — output_methods-ruby3)

Notes on using formatting libraries and simple internationalization patterns for
formatting dates, numbers, and messages.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Area</th><th>Library</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Numbers</td><td>number_to_currency (Rails) or custom formatter</td><td>Locale-aware formatting</td></tr>
    <tr><td>Dates</td><td>I18n.l / strftime</td><td>Use locale-aware date formats if supported</td></tr>
    <tr><td>Messages</td><td>I18n.t</td><td>Keep message interpolation safe</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — output_methods-ruby3)

1. Add a number formatting helper for currencies with tests for different
   locales.
2. Replace `puts` date outputs with locale-aware formatting and test sample
   locales.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Output Methods — Structured Output & Exporting (Appendix — output_methods-ruby4)

Guidance for exporting structured data (CSV/JSON), streaming large exports, and
ensuring testable output formats.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Format</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>CSV</td><td>Tabular exports</td><td>Use `CSV.generate` and stream rows</td></tr>
    <tr><td>JSON</td><td>API responses</td><td>Use `JSON.generate` with symbolize_names as needed</td></tr>
    <tr><td>Streaming</td><td>Large exports</td><td>Write rows incrementally to avoid memory spikes</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: streaming CSV

```ruby
require 'csv'
CSV($stdout) do |csv|
  data.each { |row| csv << row }
end
```

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — output_methods-ruby4)

1. Implement a CSV exporter that streams rows to STDOUT and add tests using
   Tempfile to capture output.
2. Produce JSON output for a nested data structure and write tests asserting key
   ordering where necessary. <!-- markdownlint-disable MD033 MD034 MD040 MD010
   -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Output Methods — `puts`, `p`, `print`, `printf` (Appendix — output_methods-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Method</th><th>Behavior</th><th>Insider tip</th></tr>
  </thead>
  <tbody>
    <tr><td>`puts`</td><td>Converts to `to_s`, adds newline</td><td>Good for user-facing output</td></tr>
    <tr><td>`p`</td><td>Calls `inspect`</td><td>Great for debugging complex objects</td></tr>
    <tr><td>`printf`/`format`</td><td>Formatted output</td><td>Use for aligned columns or numeric formatting</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Tips & Tricks

- For consistent logging, prefer `Logger` over `puts` so you can change levels
  and destinations later.
- Use `STDOUT.sync = true` when writing progress indicators to ensure immediate
  flushing.
- When testing, capture `$stdout` with `StringIO` instead of relying on console
  output.

### Example

```ruby
STDOUT.sync = true
printf("%10s %5d\n", "item", 42)
```

<!-- markdownlint-disable MD013 -->
### Appendix — Exercises

1. Replace `puts` in a small script with `Logger` and observe how log level
   changes affect output.
2. Write a test that captures `STDOUT` and asserts on formatted output produced
   by `printf`.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Output Methods — STDOUT Patterns & Testability (Appendix — output_methods-appendix-2)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Method</th><th>Behavior</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr><td>`puts`</td><td>Adds newline</td><td>Use for human-facing lines; avoid in libraries</td></tr>
    <tr><td>`print`</td><td>No newline</td><td>Good for progress bars</td></tr>
    <tr><td>`StringIO`</td><td>In-memory IO</td><td>Great to capture output in tests without touching STDOUT</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```ruby
require 'stringio'
buf = StringIO.new
$stdout = buf
puts 'hello'
$stdout = STDOUT
buf.rewind
puts buf.read # => "hello\n"
```

<!-- markdownlint-disable MD013 -->
### Appendix exercises

1. Replace a test that inspects `STDOUT` by using `StringIO` to capture output
   and assert on the buffer.
2. Refactor a library function to avoid printing directly and instead return a
   string for the caller to decide how to render.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
