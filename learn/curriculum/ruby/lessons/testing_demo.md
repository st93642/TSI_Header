# Testing System Demo

## Overview

This lesson demonstrates the enhanced testing capabilities of the TSI Header extension. The testing system now supports multiple types of tests to validate different aspects of your code behavior.

## Learning Goals

- Distinguish between the four supported test types: output, exception, side effect, and return value.
- Understand how the harness captures STDOUT/STDERR without interfering with your code.
- Structure code so it’s testable—clear separation of computation, IO, and error handling.
- Apply cleanup strategies so side-effect tests remain deterministic.
- Anticipate cross-language portability when exercises span Ruby, Python, and JavaScript.

## Test Types

### 1. Output Tests

Output tests capture what your code prints to the console using `puts`, `print`, or `p`. This is essential for validating user-facing output and debugging information.

```ruby
def print_greeting(name)
  puts "Hello, #{name}!"
end

# Harness (simplified)
require "stringio"
buffer = StringIO.new
$stdout = buffer
print_greeting("Alice")
$stdout = STDOUT
raise "Unexpected" unless buffer.string == "Hello, Alice!\n"
```

### Tips

- `puts` appends newlines—match them exactly. Use `strip` or `chomp` only if the spec allows.
- Keep formatting deterministic (no timestamps unless asked). For dynamic values, tests may use regex on the captured string.

### 2. Exception Tests

Exception tests verify that your code properly raises errors when given invalid input. This ensures robust error handling.

```ruby
def validate_age(age)
  raise ArgumentError, "Age cannot be negative" if age.negative?
  age
end

assert_raises(ArgumentError) { validate_age(-5) }
```

Guidance:

- Raise specific exception classes—`ArgumentError`, `RuntimeError`, custom types.
- Include meaningful error messages; tests can assert on `exception.message`.

### 3. Side Effect Tests

Side effect tests validate operations that modify external state, such as file creation, database updates, or other persistent changes.

```ruby
def create_log_file(message)
  File.open("test.log", "a") { |f| f.puts message }
end

# Before each test
File.delete("test.log") if File.exist?("test.log")

create_log_file("Test message")
assert File.exist?("test.log")
assert_includes File.read("test.log"), "Test message"
```

Best practices:

- Keep side effects isolated in helper methods; they’re easier to set up and tear down.
- Use temporary directories or test-specific filenames to avoid clobbering real data.
- Clean up in ensure/teardown blocks so one failure doesn’t cascade.

### 4. Regular Return Value Tests

Traditional tests that validate return values from method calls.

```ruby
def calculate_total(items)
  items.sum
end

assert_equal 10, calculate_total([1, 2, 3, 4])
```

Return tests encourage pure functions—no printing, no global state. Favor this style whenever possible; output and side-effect tests layer on top when necessary.

## Implementation Details

- **Ruby**: Minitest with `StringIO` for STDOUT, `capture_io` for dual stream capture, temporary directories for side effects.
- **Python**: Pytest’s `capsys` fixture, `pytest.raises`, and tmp-path fixtures provide analogous behaviors.
- **JavaScript**: Custom harness wraps console methods, uses Node’s `fs` module with temporary directories.

Understanding these internals helps you diagnose failing tests (e.g., missing newline, wrong exception type).

## Practice Prompts

1. **Output + return combo**
   - Write a method `report_sum(a, b)` that prints `"Sum: X"` but returns the numeric sum.
   - Ensure tests could capture and assert both aspects separately.

2. **Exception edge cases**
   - Implement `parse_age(string)` that converts strings to integers, raising `ArgumentError` for non-numeric or negative input.
   - Craft tests covering whitespace, `nil`, and large numbers.

3. **Side-effect cleanup**
   - Create `append_audit(entry, path:)` that writes to a file and ensures a trailing newline.
   - In tests, delete the file before each run, call the method twice, and assert the file contains two lines.

4. **Cross-language mindset**
   - Imagine porting `parse_age` to Python. List the equivalent assertion helpers and exception types.
   - Evaluate how string-to-int conversion differs (`int(value)` vs `Integer(value, exception: false)`).

5. **Harness extension**
   - Design a new test type (e.g., HTTP call recording) and outline how you’d capture behavior in Ruby, Python, and JavaScript.
   - Consider how to stub external services while keeping tests deterministic.

## Self-Check Questions

1. Why can relying solely on printed output make code harder to reuse, and how do return-value tests promote better design?
2. What steps ensure side-effect tests remain deterministic, even when run in parallel or repeatedly?
3. How does the harness capture STDOUT without losing the original stream, and what pitfalls occur if you forget to restore it?
4. When testing exceptions, why is it important to assert on both the class and the message?
5. How would you adapt these patterns when writing tests in another language—what stays the same, and what changes?

Lean on this testing toolbox every time you practice: keep logic pure when possible, isolate IO, and let the harness confirm your code behaves exactly as intended.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Testing in CI and Local Workflows

This appendix provides a reproducible CI configuration, sample test helpers, and instructor notes to make tests reliable across environments.

### GitHub Actions example

```yaml
name: Ruby test
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Setup Ruby
        uses: ruby/setup-ruby@v1
        with:
          ruby-version: '3.2'
      - name: Install
        run: bundle install
      - name: Run tests
        run: bundle exec rake test
```

### Test helpers

Provide a tiny helper to capture stdout/stderr in Minitest-style tests.

```ruby
require 'stringio'

def capture_stdout
  old_stdout = $stdout
  $stdout = StringIO.new
  yield
  $stdout.string
ensure
  $stdout = old_stdout
end
```

### CI triage table (HTML)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Check</th><th>Purpose</th><th>Remediation</th></tr>
  </thead>
  <tbody>
    <tr><td>Bundle install</td><td>Ensure gems are installable</td><td>Pin gem versions in Gemfile</td></tr>
    <tr><td>Unit tests</td><td>Verify behavior</td><td>Run locally with `rake test`</td></tr>
    <tr><td>Lint</td><td>Style and consistency</td><td>Run `rubocop` and fix offenses</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example test pattern

```ruby
require 'minitest/autorun'
require_relative '../lib/my_code'

class MyCodeTest < Minitest::Test
  def test_sum
    assert_equal 3, sum(1,2)
  end
end
```

### Exercises

1. Add tests for edge cases (nil inputs, empty arrays).
2. Convert an output-based test to a return-value test and explain the benefits.
3. Add a `rake` task to run tests and wire it into CI.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Testing Helpers & Selective Runs (Appendix)

Add examples of `test_helper.rb`, running single tests, and a small runner comparison table.

```ruby
# run a single test file
ruby -Ilib:test test/test_foo.rb

# run a single test method via minitest
ruby -Ilib:test test/test_foo.rb --name /test_method_name/
```
<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Runner</th><th>Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>rake test</td><td>Project tasks</td><td>Convenient</td></tr>
    <tr><td>ruby test</td><td>Single file</td><td>Debugging</td></tr>
    <tr><td>CI</td><td>Full suite</td><td>Gate PRs</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix)

1. Add a `test_helper.rb` that configures temporary dirs and reporter formatting.
2. Document how to run a single test locally and in CI.

<!-- markdownlint-enable MD010 -->

## Practical Appendix: External Tools & Examples (Appendix — External Tools — testing_demo-ruby)

Quick recipes for testing in Ruby: Minitest skeletons, RSpec pointers, and CI tips for running tests in GitHub Actions.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Use</th><th>Link</th></tr>
  </thead>
  <tbody>
    <tr><td>Minitest</td><td>Lightweight unit tests</td><td><a href="https://github.com/seattlerb/minitest">Minitest</a></td></tr>
    <tr><td>RSpec</td><td>Behaviour-driven tests</td><td><a href="https://rspec.info/">RSpec</a></td></tr>
    <tr><td>GitHub Actions</td><td>CI for running tests</td><td><a href="https://docs.github.com/actions">GH Actions</a></td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Minitest example

```ruby
require 'minitest/autorun'

def add(a,b) a+b end

class TestAdd < Minitest::Test
  def test_add
    assert_equal 5, add(2,3)
  end
end
```

### Exercises (testing_demo-ruby)

1. Convert one of the lesson's examples into a Minitest and ensure it runs under `ruby -r minitest/autorun`.
2. Add a simple GitHub Actions workflow that runs `bundle exec rake test` or `ruby -r minitest/autorun` on push.
