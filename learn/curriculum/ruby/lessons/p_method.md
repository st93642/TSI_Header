# Debugging with `p`

Ruby’s `p` method is a fast, expressive way to inspect values while you code. It combines printing with inspection, making it perfect for tracing state, verifying assumptions, and understanding unfamiliar objects. Learning when—and how—to use `p` can dramatically speed up your debugging sessions.

## Learning goals

- Compare `p`, `puts`, `print`, and `pp` to choose the right inspection tool.
- Understand how `p` leverages `Object#inspect` and how to customize it.
- Use `tap`, `then`, and block constructs to log intermediate values without
  changing control flow.
- Capture inspected output for logs or tests, and redirect it safely.
- Apply `p` in real-world debugging scenarios, from API responses to complex
  enumerables.

## What `p` does

`p` prints an object using `inspect` and returns the object itself. It writes to `STDOUT` by default.

```ruby
value = "Hello"
returned = p(value)
# STDOUT: "Hello"
# returned == "Hello"
```

Because `p` returns the original object, you can chain it or sprinkle it in expression pipelines without breaking the result.

## `p` vs. `puts` vs. `pp`

```ruby
puts "Hello"   # => Hello (no quotes)
p "Hello"      # => "Hello" (uses inspect)
require "pp"
pp "Hello"     # => "Hello" (pretty-print; useful for nested structures)
```

- **`puts`**: great for user-facing output. Converts to string and adds a
  newline per argument.
- **`p`**: inspection output with quotes, escape characters, and nested objects
  on one line.
- **`pp`**: pretty-print nested arrays/hashes; ideal when a single line wraps
  awkwardly.

## Inspecting common structures

```ruby
arr = [1, [2, 3], { ok: true }]
puts arr
# 1
# [2, 3]
# {:ok=>true}

p arr
# [1, [2, 3], {:ok=>true}]

require "pp"
pp arr
# [1,
#  [2, 3],
#  {:ok=>true}]
```

Choose the style that keeps data legible and context clear.

## Customizing inspection

`p` calls `inspect`. Override `inspect` (and ideally `pretty_print`) in your classes for clearer diagnostics.

```ruby
class Token
  attr_reader :value, :expires_at

  def initialize(value:, expires_at:)
    @value = value
    @expires_at = expires_at
  end

  def inspect
    "#<Token value=**** expires_at=#{expires_at.iso8601}>"
  end
end

p Token.new(value: "secret", expires_at: Time.now + 3600)
# #<Token value=**** expires_at=2025-10-03T14:00:00+00:00>
```

Custom inspection keeps sensitive data safe and highlights what matters.

## Inline debugging with `tap`

`Kernel#tap` yields the object to a block and returns the object. Combine it with `p` to log intermediate values without altering flow.

```ruby
result = users
  .select { |u| u.active? }
  .tap { |list| p active_users: list.size }
  .map(&:email)
```

`ActiveSupport#debug` and `Object#yield_self`/`then` offer similar patterns.

## Capturing output

Redirect `STDOUT` temporarily or use `StringIO` when you need to capture `p` output in tests.

```ruby
require "stringio"

io = StringIO.new
$stdout = io
p({ status: :ok })
$stdout = STDOUT

io.string # => "{:status=>:ok}\n"
```

Always restore `$stdout` inside an `ensure` block to avoid affecting later code.

## Logging contexts

Use `p` liberally during development, but swap to structured logging before shipping. One pattern: guard debug output with an environment check.

```ruby
p response if ENV["DEBUG"]
```

For production-grade logs, prefer `logger.debug` or `Rails.logger.debug` with JSON payloads.

## Beyond the basics

- `Kernel#pp` (pretty print) formats nested data across multiple lines.
- Gems like `awesome_print` or `pry` offer colored, annotated dumps.
- `Object#pretty_inspect` returns the pretty-printed string without
  printing—handy for building log messages.
- `Kernel#caller` combined with `p` reveals stack traces when needed.

## Guided practice

1. **API response trace**
   - Fetch JSON from an API (or mock the response).
   - Use `p` to inspect the parsed hash, focusing on nested structures.
   - Override `inspect` on a wrapper class to hide large payload fields.

2. **Enumerator pipeline**
   - Chain `map`, `select`, and `reduce` over an array.
   - Insert `tap { |value| p stage: "after map", value: }` to observe each
     phase.

3. **Custom inspect implementation**
   - Build a `User` struct whose `inspect` masks `password_digest` and includes
     role/count metadata.
   - Demonstrate before/after output using `p`.

4. **Capture in tests**
   - Write a mini helper that captures output from `p` calls.
   - Assert that debug statements include key information without leaking
     sensitive data.

5. **Conditional debug flag**
   - Implement a helper `debug_dump(label, value)` that calls `p` only when
     `ENV["DEBUG"]` is truthy.
   - Ensure it still returns `value` so callers can chain.

## Self-check questions

1. How does `p` differ from `puts` in terms of return value and visual output?
2. Why is overriding `inspect` useful, and what should you watch out for when
   doing so?
3. How can `tap` help you view intermediate states without altering the final
   result of an expression?
4. When should you upgrade from `p` to structured logging or dedicated debugging
   tools?
5. What techniques let you capture or suppress `p` output during automated
   tests?

`p` is the Rubyist’s quick flashlight—turn it on to illuminate confusing code paths, then switch to more targeted instrumentation once you’ve found the issue. With thoughtful use, it becomes an indispensable part of your debugging toolkit.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: `p` & Debugging — Quick Inspectors & Strategies (Appendix — p_method-ruby2)

When to use `p`, `puts`, `pp`, and logging for debugging; how to keep debug code out of production and test for side effects.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>p</td><td>Quick inspect with `inspect`</td><td>Good for quick REPL debug; avoid in production logs</td></tr>
    <tr><td>puts</td><td>User-facing output</td><td>Prefer for formatted output</td></tr>
    <tr><td>pp</td><td>Pretty-print complex objects</td><td>Useful for nested structures</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example debugger helpers

```ruby
def debug(obj)
  warn obj.inspect
end

# Use 'warn' to send to STDERR so tests can capture/redirect it
```

### Testing debug output

- Capture STDOUT/STDERR in tests when asserting side-effecting output.

```ruby
require 'minitest/autorun'

class TestDebug < Minitest::Test
  def test_debug
    out = capture_io { debug('x') }
    assert_match /x/, out.join
  end
end
```

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — p_method-ruby2)

1. Implement a `with_debug` helper that yields a block and logs entry/exit with
   timing; write tests that assert timing behaviour and that logs are written to
   STDERR.
2. Replace ad-hoc `p` calls in a small codebase with a configurable logger and
   test that log level controls emission.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Debugging Helpers (Appendix)

A short appendix showing `p`, `pp`, `logger` usage and a table comparing output helpers.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Helper</th><th>Output</th><th>When to use</th></tr>
  </thead>
  <tbody>
    <tr><td>p</td><td>inspect</td><td>Quick debugging</td></tr>
    <tr><td>pp</td><td>pretty</td><td>Readable complex objects</td></tr>
    <tr><td>Logger</td><td>levels/structured</td><td>Production logging</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-enable MD013 -->
### Exercises (Appendix)

1. Replace a `puts` call in a small script with `logger` and add a `--verbose`
   flag.
2. Add a `debug` helper that prints file and line context before `p` output.

<!-- markdownlint-disable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Debugging Shortcuts (Appendix II)

Quick tips for using `p`, `pp`, `logger`, and a table showing when to pick each.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>When</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr><td>p</td><td>Quick inspect</td><td>p obj</td></tr>
    <tr><td>pp</td><td>Readable complex</td><td>pp obj</td></tr>
    <tr><td>logger</td><td>Production</td><td>logger.info 'msg'</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix II — p_method)

1. Add a debug helper that prints stack context and the inspected object.
2. Replace ad-hoc `p` calls with a centralized debug helper in one small
   example.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Debug Printing & `p` (Appendix — External Links)

Quick references for common debug printing helpers and safe production alternatives.

- Ruby `p` docs: [Kernel#p](https://ruby-doc.org/core/Kernel.html#method-i-p)
- Debugging guide: [Using Pry/Byebug in development](https://github.com/pry/pry)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Link</th><th>Use</th></tr>
  </thead>
  <tbody>
    <tr><td>`p`</td><td><a href="https://ruby-doc.org/core/Kernel.html#method-i-p">Kernel#p</a></td><td>Quick inspect; not for production logs</td></tr>
    <tr><td>Pry</td><td><a href="https://github.com/pry/pry">Pry</a></td><td>Interactive debugging REPL</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-enable MD013 -->
### Exercises (External Resources)

1. Replace `puts` based debug prints with `p` for a complex object and write a
   test that asserts structure.
2. Add a conditional debug helper that only triggers when an environment
   variable is set.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: `p` & Inspectors — Deep Dive (Appendix II — External Links)

Notes on `p`, `inspect`, and using pretty-print (`pp`) for complex objects.

```ruby
require 'pp'
obj = {a: [1,2,3], b: {x: 1}}
pp obj
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Link</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>`p`</td><td><a href="https://ruby-doc.org/core/Kernel.html#method-i-p">Kernel#p</a></td><td>Quick debugging output</td></tr>
    <tr><td>`pp`</td><td><a href="https://ruby-doc.org/stdlib/pp/rdoc/PP.html">PrettyPrint</a></td><td>Better readability for nested objects</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix II)

1. Replace ad-hoc string inspection with `pp` in a complex test fixture and
   update expected outputs.
2. Add a conditional debug printer that respects an environment flag.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Debug Print Helpers — `p`, `pp` & Replace with Logger (Appendix — p_method-ruby-appendix-20251005-02)

Hands-on recipes to turn ad-hoc `p` debugging into structured, testable logging and debug helpers.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Helper</th><th>Behavior</th><th>Replacement</th></tr>
  </thead>
  <tbody>
    <tr><td>p</td><td>inspect</td><td>Logger.debug(obj.inspect)</td></tr>
    <tr><td>pp</td><td>pretty-print</td><td>Logger.debug(pp(obj))</td></tr>
    <tr><td>puts</td><td>to_s</td><td>Logger.info</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-enable MD013 -->
### Example: toggleable debug

```ruby
require 'logger'
logger = Logger.new(STDOUT)
logger.level = Logger::WARN
logger.debug { obj.inspect }
```

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — p_method-ruby-appendix-20251005-02)

1. Replace `p` calls in a small script with `Logger` and add tests asserting log
   messages are emitted at the correct levels.
2. Implement a debug helper that respects an environment variable to enable
   verbose output.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: `p`, `puts` & `print` — Debugging Output Choices (Appendix — p_method-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Use</th><th>Behavior</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr><td>`p`</td><td>Calls `inspect` and prints newline</td><td>Good for quick debugging; prints raw representations</td></tr>
    <tr><td>`puts`</td><td>Calls `to_s`, adds newline</td><td>User-facing, human-readable output</td></tr>
    <tr><td>`print`</td><td>No newline</td><td>Useful for progress or same-line output</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-enable MD013 -->
### Quick examples

```ruby
p [1, 2, 3]      # => "[1, 2, 3]"
puts [1, 2, 3]   # => prints each element on new line via to_s
print 'hello'    # => no newline
```

### Testing tip

Capture `STDOUT` or stub `Kernel#p` when asserting debug output in unit tests to prevent brittle tests.

### Exercises

1. Replace a `puts` used during debugging with `p` and write a test that asserts
   the debug output contains expected inspected values.
2. Create a small helper `debug(obj, logger: $stderr)` that calls
`logger.puts(obj.inspect)` so you can redirect output in tests.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Fast Inspection & Debugging with `p` (Appendix — p_method-appendix-2)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Best for</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr><td>`p`</td><td>Quick inspect during development</td><td>Returns the object; safe for chaining in pipelines</td></tr>
    <tr><td>`pp`</td><td>Nested/hierarchical data</td><td>Pretty prints multi-line structures</td></tr>
    <tr><td>`warn`/`logger`</td><td>Production diagnostics</td><td>Prefer structured logging for production; use `warn` to write to STDERR</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```ruby
# Insert quick probes into a pipeline
users.select(&:active?).tap { |u| p active_count: u.size }.map(&:email)

# Capture p output in tests
require 'stringio'
io = StringIO.new
$stdout = io
p({ status: :ok })
$stdout = STDOUT
puts io.string  # => "{:status=>:ok}\n"
```

<!-- markdownlint-disable MD013 -->
### Appendix — Exercises

1. Add a `debug_dump` helper that uses `warn` and only prints when
`ENV['DEBUG']` is set; ensure it returns the original value so it can be chained.
2. Implement a safe `inspect` override for a small domain class that hides
   sensitive fields and test the output via captured IO.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
