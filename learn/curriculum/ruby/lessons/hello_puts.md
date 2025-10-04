# Hello world with `puts`

Every Ruby journey begins with `puts`. Understanding how it behaves—what it prints, what it returns, and how it interacts with other output helpers—sets the tone for idiomatic Ruby scripts. This lesson keeps the “hello world” tradition but digs deeper into the nuances of sending text to standard output.

## Learning goals

- Use `puts` to write strings (and other objects) to standard output with automatic newlines.
- Distinguish between printing a value and returning a value from a method.
- Compare `puts` with `print`, `p`, and string interpolation to choose the right tool.
- Manage whitespace, newlines, and prompts for user input.
- Practice small scripts that greet users and log work while keeping business logic separate from presentation.

## Meet `puts`

`puts` stands for “put string”. It prints each argument on its own line and converts objects to strings with `to_s`.

```ruby
puts "Hello, world!"
puts 42
puts [:ruby, :rails]
```

Output:

```text
Hello, world!
42
ruby
rails
```

Notice arrays are expanded line by line; this is handy for quick exploration.

## Printing vs. returning

Printing shows information to the user; returning hands back a value so the caller can use it. `puts` always returns `nil`.

```ruby
def greet_print(name)
  puts "Hello, #{name}!"
end

def greet_return(name)
  "Hello, #{name}!"
end

print_result = greet_print("Ada")
return_result = greet_return("Ada")

print_result.nil?   # => true
return_result       # => "Hello, Ada!"
```

Methods that compute results should return values and let callers decide whether to print.

## `puts` vs. `print` vs. `p`

```ruby
print "Prompt: "  # no newline
STDOUT.flush
answer = gets.chomp
puts "You typed #{answer}"  # newline added
p "Answer"                  # inspection output with quotes
```

- `print` leaves the cursor on the same line—perfect for prompts.
- `puts` appends a newline and calls `to_s`.
- `p` uses `inspect`, showing quotes and escaping characters (great for debugging).

## Interpolation beats concatenation

```ruby
name = "Maya"
puts "Hello, #{name}!"       # concise
puts "Hello, " + name + "!"  # works, but noisier
```

Interpolation calls `to_s` automatically and avoids manual spacing errors.

## Multiple arguments and arrays

`puts` accepts multiple arguments; each prints on its own line.

```ruby
puts "Line 1", "Line 2", "Line 3"
# Line 1
# Line 2
# Line 3

puts ["alpha", "beta"]
# alpha
# beta
```

`puts` flattens arrays one level. Wrap output in `p` if you want to see brackets.

## Controlling whitespace

Use `\n` explicitly when embedding newlines inside a single string.

```ruby
puts "Hello\nworld"  # prints two lines
```

Trim user-entered strings with `strip` or `chomp` before incorporating them into output.

```ruby
print "Name: "
name = gets.chomp  # removes trailing newline
puts "Nice to meet you, #{name}."
```

## Separation of concerns

Keep logic and presentation separate by returning strings from helpers and printing at the edges of your program.

```ruby
def greeting(name)
  "Hello, #{name}!"
end

if __FILE__ == $PROGRAM_NAME
  print "Name: "
  name = gets.chomp
  puts greeting(name)
end
```

This pattern makes `greeting` easy to test.

## Directed practice

1. **Greeter script**
   - Prompt for a first and last name using `print` + `gets`.
   - Build a greeting string and print it with `puts`.
   - Ensure your method returns the greeting so tests could assert on it later.

2. **Checklist logger**
   - Write a method `log_step(step)` that returns `"✔ #{step}"`.
   - Iterate through an array of steps, printing each with `puts`.
   - Confirm the method returns the string so you could reuse it (e.g., in logs).

3. **Newline explorer**
   - Compare `puts "Hello"` versus `puts "Hello\n"` versus `print "Hello"`.
   - Use `p` to inspect the strings you’re printing to see embedded `\n` characters.

4. **Prompt helper**
   - Build `prompt(label)` that prints `"#{label}: "`, flushes STDOUT, and returns the user’s input without the newline.
   - Demonstrate it by asking for favorite language and framework.

5. **Return-only function**
   - Create `build_banner(message, width:)` returning an ASCII banner string.
   - Write a small driver script that calls `puts build_banner(...)`.
   - Verify the banner builds correctly without the helper itself printing anything.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Printing Patterns and Tests

This appendix offers helpers for test-friendly printing and a small HTML table comparing output helpers.

```ruby
def prompt(label)
  print "#{label}: "
  STDOUT.flush
  gets.chomp
end

# Capture print output in tests
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

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Helper</th><th>Behavior</th><th>When to use</th></tr>
  </thead>
  <tbody>
    <tr><td>puts</td><td>Newline after each arg</td><td>User-friendly output</td></tr>
    <tr><td>print</td><td>No newline</td><td>Interactive prompts</td></tr>
    <tr><td>p</td><td>inspect output</td><td>Debugging</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises

1. Implement `prompt` and test it by simulating STDIN and capturing STDOUT.
2. Build a banner generator function that returns a string to be printed externally.

<!-- markdownlint-enable MD010 -->

## Self-check questions

1. Why is returning a string from a method more flexible than printing inside the method?
2. How does `puts` handle arrays differently from `p`, and when would each be preferable?
3. What happens if you rely on `puts` inside library code? How might that affect users embedding your library in a larger app?
4. How can you ensure a prompt is visible before `gets` waits for input?
5. When combining output and business logic, what strategies keep your code testable and maintainable?

A solid grasp of `puts` and its relatives keeps your scripts tidy from day one. Print at the boundaries, return values elsewhere, and you’ll build Ruby programs that are both friendly to users and friendly to tests.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Script Smoke Tests & CI (Appendix)

Add a CI snippet to run small scripts and a table comparing script types.

```yaml
name: Script Smoke
on: [push]
jobs:
  smoke:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run hello
        run: ruby bin/hello.rb || true
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Type</th><th>Test</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Utility</td><td>Smoke run</td><td>Quick verification</td></tr>
    <tr><td>Library</td><td>Unit tests</td><td>More coverage</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix)

1. Add a smoke test that runs `ruby bin/hello.rb` in CI.
2. Add a `--name` flag and test the output formatting in CI.

<!-- markdownlint-enable MD010 -->

## Practical Appendix: External Tools & Examples (Appendix — External Tools — hello_puts-ruby)

A short appendix with I/O examples, ruby-doc references, and tiny exercises focused on `puts`/`print` and output formatting.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Why</th><th>Reference</th></tr>
  </thead>
  <tbody>
    <tr><td>IO and puts</td><td>Simple output & newline rules</td><td><a href="https://ruby-doc.org/core/IO.html">IO docs</a></td></tr>
    <tr><td>Formatting</td><td>sprintf/format</td><td><a href="https://ruby-doc.org/core/String.html">String docs</a></td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Quick examples

```ruby
puts "hello"          # prints with newline
print "hello"         # prints without newline
printf("%04d", 5)    # formatted output
```

### Exercises (hello_puts-ruby)

1. Create a short script demonstrating the difference between `puts`, `print`, and `printf`, and add tests that assert expected output using `StringIO` to capture stdout.
2. Document how `$stdout.sync = true` affects realtime output behavior in scripts.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: puts/print — Tests & Scripts (Appendix — hello_puts-ruby2)

Test-friendly helpers, quick scripts, and CI examples for I/O-focused exercises.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Helper</th><th>Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>capture_stdout</td><td>Test printed output</td><td>Use StringIO to isolate output</td></tr>
    <tr><td>prompt helper</td><td>Interactive scripts</td><td>Flush STDOUT before gets</td></tr>
    <tr><td>CI smoke</td><td>Run example scripts</td><td>Make scripts idempotent</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### capture_stdout helper

```ruby
require 'stringio'

def capture_stdout
  old = $stdout
  $stdout = StringIO.new
  yield
  $stdout.string
ensure
  $stdout = old
end
```

### CI smoke example

```yaml
name: hello smoke
on: [push]
jobs:
  smoke:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run hello script
        run: ruby learn/curriculum/ruby/lessons/hello_puts_sample.rb || true
```

### Exercises (Appendix — hello_puts-ruby2)

1. Implement `prompt(label)` that flushes stdout and returns chomped input; unit-test it by stubbing `$stdin` and capturing `$stdout`.
2. Add a smoke script that prints a greeting and ensure CI runs it on push.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
