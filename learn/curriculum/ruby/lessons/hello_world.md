# Hello World and Output

Ruby is famous for rewarding curiosity—your very first script can be useful, expressive, and fun. This lesson goes far beyond a one-line "Hello, World!" by exploring how Ruby talks to the outside world, how the runtime executes scripts, and how to build confidence with your development tools.

## Learning goals

- Understand how Ruby programs execute from the command line and within interactive consoles.
- Master the nuanced differences between `puts`, `print`, `p`, `warn`, `printf`, and `format`.
- Control newlines, whitespace, and encodings in output so your programs behave consistently on every platform.
- Use comments, documentation, and debugging helpers to communicate intent and trace issues.
- Practice with guided exercises that simulate real-world workflows, from quick prototypes to logging.

## A quick tour of Ruby

Ruby, created by Yukihiro "Matz" Matsumoto in the mid-1990s, combines ideas from Perl, Smalltalk, Lisp, and Python. Its guiding principle is “optimized for programmer happiness.” That means the language syntax is designed to read like English, the standard library favors clarity over ceremony, and Rubyists lean on expressive code rather than verbose boilerplate.

### Installation check

Confirm Ruby is installed:

```bash
ruby --version
```

You'll see output such as `ruby 3.2.2p20 (2024-04-23 revision ...) [x86_64-linux]`. If Ruby is missing, install it through a package manager (e.g., `sudo apt install ruby-full` on Ubuntu) or a version manager like `rbenv` or `rvm` so you can switch versions per project.

## Writing your first Ruby script

Create a file named `hello.rb` with the following content:

```ruby
puts "Hello, World!"
```

Run it from the terminal:

```bash
ruby hello.rb
```

You should see `Hello, World!` printed with a trailing newline. Ruby executes scripts top to bottom, interpreting each line into bytecode before running it. For multi-file projects you require other files, but for now a single script is perfect.

### Running Ruby interactively with IRB

IRB (Interactive Ruby) is a REPL (Read-Eval-Print Loop) bundled with Ruby. Launch it by typing `irb`. Then experiment:

```ruby
irb(main):001> puts "Hello from IRB"
Hello from IRB
=> nil
```

IRB immediately evaluates each expression and echoes the result after `=>`. Use it to try syntax ideas, inspect objects, or test library calls without creating files.

## Output methods deep dive

### `puts`: appended newline output

`puts` prints each argument followed by a newline. When given multiple arguments, it prints each on its own line and returns `nil` (as most Ruby output methods do).

```ruby
puts "Welcome to Ruby!"
puts "Each call ends with a newline."
puts 42, :symbols, ["arrays", "too"]
```

### `print`: inline output

`print` writes text without adding a newline. You'll often use `print` with `STDOUT.flush` when prompting for input so the user sees the prompt immediately.

```ruby
print "Enter your name: "
STDOUT.flush
name = gets.chomp
puts "Hello, #{name}!"
```

### `p`: inspect-friendly output

`p` is shorthand for `Kernel#inspect`. It prints the Ruby representation of each argument with type-specific formatting. Great for debugging arrays, hashes, and nested data:

```ruby
p "Hello"
p [1, 2, 3]
p({ nested: { sample: true } })
```

### `pp`: pretty-print complex objects

Require `pp` to pretty-print deeply nested data structures, especially hashes and arrays:

```ruby
require "pp"
pp({
  name: "Ada",
  skills: ["Ruby", "Systems"],
  meta: { active: true, score: 98.5 }
})
```

### `warn`: write to STDERR

Use `warn` to send messages to the standard error stream. This keeps logs separate from normal program output, which is essential when composing scripts in pipelines.

```ruby
warn "Configuration file not found; using defaults."
```

### `printf` and `format`: templated output

When you need precise formatting, use `printf` or its sibling `format`:

```ruby
printf "%-10s %6.2f\n", "total", 19.845
message = format("%-10s %04d", "ID", 42)
puts message
```

`printf` prints immediately, while `format` returns the formatted string.

## Strings and encodings

Ruby strings are mutable sequences of bytes associated with an encoding (UTF-8 by default). Understanding strings early helps you avoid subtle bugs.

- **Single quotes (`'`)**: treat contents literally, except for `\'` and `\\`.
- **Double quotes (`"`)**: support interpolation (`"Hello, #{name}"`) and escape sequences (`\n`, `\t`).
- **Heredocs**: excellent for multi-line text.

```ruby
message = <<~TEXT
  Welcome aboard!
  Today's date: #{Time.now.strftime("%Y-%m-%d")}
TEXT

puts message
```

Always be mindful of encoding when reading or writing files. You can check a string's encoding with `message.encoding` and convert it using `encode`.

## Comments, documentation, and style

Ruby ignores everything after `#` on a line. Comments explain *why* code exists, not just what it does.

```ruby
# Greet the user politely; fails gracefully if name is missing.
def greet(name)
   puts "Hello, #{name || "stranger"}!"
end
```

Use `=begin` / `=end` sparingly for multi-line comments; they interrupt indentation and some style guides discourage them. Prefer RDoc/YARD docstrings:

```ruby
# Greets a user by name.
#
# @param name [String] the user's name
# @return [void]
def greet(name)
   puts "Hello, #{name}!"
end
```

## Debugging early scripts

- **`p` and `pp`** reveal structures at a glance.
- **`warn`** outputs to STDERR so you can separate diagnostics from data.
- **`Kernel#binding.irb`** drops you into an interactive REPL at runtime.
- Add timestamps with `Time.now` to trace execution in time-sensitive scripts.

Example:

```ruby
def debug_example
   warn "[#{Time.now.iso8601}] About to greet"
   puts "Greetings"
end

debug_example
```

## Working with STDOUT and STDERR

Ruby exposes the standard IO streams as global constants:

- `STDOUT` for regular output.
- `STDERR` for errors and logs.
- `STDIN` for input.

Redirect them to capture output in files or swap them for testing:

```ruby
File.open("log.txt", "w") do |file|
   $stdout = file
   puts "This goes into the file"
ensure
   $stdout = STDOUT
end
```

Restoring `$stdout` in an `ensure` block guarantees your program keeps working even if an exception occurs.

## Internationalisation (I18n) considerations

Ruby handles UTF-8 seamlessly, but console fonts and encodings can trip you up. Test with non-Latin characters:

```ruby
puts "こんにちは世界"   # Japanese
puts "¡Hola mundo!"    # Spanish with punctuation
puts "Привет, мир!"    # Russian
```

If you see garbled output, ensure your terminal uses UTF-8 and your source files are saved with UTF-8 encoding. Add `# frozen_string_literal: true` at the top of Ruby files (a common style) to make strings immutable, improving performance.

## Common pitfalls and their fixes

- **Accidentally omitting newlines**: `print` without `\n` can cause prompts and inputs to jumble together—explicitly add `"\n"` or use `puts`.
- **Mixing `puts` with `p`**: remember `p` shows inspected strings, including quotes; this is handy for debugging but confusing for user-facing text.
- **Forgetting to flush output**: when running scripts that prompt users, call `STDOUT.flush` after `print`.
- **Encoding mismatches**: specify the encoding in file headers or use `force_encoding` cautiously when interacting with external systems.

## Guided practice

1. **Hello, Universe!**
   - Create `hello_universe.rb`.
   - Print the phrases “Hello, Solar System!” and “Hello, Milky Way!” on separate lines.
   - Use `warn` to send “Launching greeting sequence…” to STDERR before printing anything else.

2. **Prompting user input**
   - Write a script that prints “What is your favorite language? ” without a newline (use `print`).
   - Flush STDOUT, capture the response with `gets.chomp`, and then output friendly messages with both `puts` and `p` to highlight the difference.

3. **Formatted receipts**
   - Ask for an item name, quantity, and unit price.
   - Use `format` to build a receipt line like `"Widget      x  3  @  19.99  ->  59.97"`.
   - Send the receipt to STDOUT and log a detailed line to STDERR with timestamp.

4. **Localization check**
   - Print the same greeting translated into at least three languages.
   - Verify the script runs correctly when the terminal locale changes (e.g., `LANG=ja_JP.UTF-8 ruby greetings.rb`).

5. **Mini logger**
   - Implement a helper method `log(level, message)` that prints `"[LEVEL] message"` to STDERR with `warn`.
   - Call it with levels `INFO`, `WARN`, and `ERROR` to see how output interleaves when piping your script.

## Self-check quiz

1. What are the functional differences between `puts`, `print`, and `p`? When would you choose one over the others?
2. Why is `STDOUT.flush` sometimes necessary immediately after `print`?
3. How do `printf` and `format` differ, and when is it beneficial to use them instead of string interpolation?
4. What steps would you take if your script prints unreadable characters when handling emojis or accented letters?
5. Why might you prefer `warn` over `puts` for logging warnings in a command-line tool?

## Where to go next

Take your time with the practice prompts to build muscle memory around output methods and debugging. When ready, jump to the next lesson to explore variables and data types. You'll start storing values, combining them with output, and crafting scripts that feel interactive and alive.

Happy coding—and remember, every experiment you try in Ruby brings you one step closer to fluency! 🚀

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Hello World — Script Basics & RSpec/Minitest Notes (Appendix — hello_world-ruby2)

Practical tips for small Ruby scripts: shebangs, encoding headers, simple CLI parsing, and testing tiny scripts.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Shebang</td><td>`#!/usr/bin/env ruby`</td><td>Makes script executable on PATH</td></tr>
    <tr><td>Encoding</td><td>`# frozen_string_literal: true`</td><td>Consider for small scripts to reduce allocations</td></tr>
    <tr><td>Testing</td><td>Minitest/RSPEC</td><td>Wrap logic in methods for testability</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example script

```ruby
#!/usr/bin/env ruby
# frozen_string_literal: true

name = ARGV.first || 'world'
puts "Hello, #{name}!"
```

### Testing scripts

- Extract core logic into a method so unit tests can call it without forking a process.
- For integration tests, use `Open3.capture3` to run the script and assert stdout/stderr and exit codes.

### Exercises (Appendix — hello_world-ruby2)

1. Convert a simple script into a testable library by moving logic into a method and adding unit tests.
2. Write an integration test that runs the script with different arguments and verifies output and exit status.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Hello World — Packaging & CLI Helpers (Appendix — hello_world-ruby3)

Short guidance for turning small scripts into gems or CLI executables and handling common argument parsing patterns.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Goal</th><th>Tool</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>CLI parsing</td><td>OptionParser</td><td>Use for robust flag handling</td></tr>
    <tr><td>Packaging</td><td>Gem skeleton</td><td>Wrap logic in a lib file and provide an executable in `bin/`</td></tr>
    <tr><td>Testing</td><td>Open3 for integration</td><td>Assert exit codes and stdout/stderr</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: OptionParser

```ruby
require 'optparse'
options = { }
OptionParser.new do |opts|
  opts.on('-nNAME', '--name=NAME', 'Name to greet') { |v| options[:name] = v }
end.parse!
puts "Hello, #{options[:name] || 'world'}!"
```

### Packaging notes

- Move reusable code into `lib/` and keep `bin/` scripts thin.
- Provide `executables` in the gemspec and use `bundle gem` to scaffold.

### Exercises (Appendix — hello_world-ruby3)

1. Convert the Hello World script into a tiny gem with a CLI entrypoint and add a test that runs the executable using `Open3.capture3`.
2. Add OptionParser flags for `--shout` and `--repeat` and test the behavior for several combinations.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
