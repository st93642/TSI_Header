# Blocks and Procs

Blocks are chunks of code that can be passed to methods. They are fundamental to
Ruby's iterator pattern.

## What is a Block?

A block is code enclosed in `do...end` or curly braces `{}`:

```ruby
# Block with do...end
[1, 2, 3].each do |num|
  puts num
end

# Block with curly braces (single line)
[1, 2, 3].each { |num| puts num }
```

## Convention

- Use `{}` for single-line blocks
- Use `do...end` for multi-line blocks

```ruby
# Single line - use {}
numbers.each { |n| puts n }

# Multiple lines - use do...end
numbers.each do |n|
  squared = n ** 2
  puts "Square of #{n} is #{squared}"
end
```

## Block Parameters

```ruby
# Single parameter
[1, 2, 3].each { |num| puts num }

# Multiple parameters
{ a: 1, b: 2 }.each { |key, value| puts "#{key}: #{value}" }
```

## yield Keyword

Methods can execute blocks using `yield`:

```ruby
def greet
  puts "Before block"
  yield
  puts "After block"
end

greet do
  puts "Inside block"
end

# Output:
# Before block
# Inside block
# After block
```

## Passing Arguments to Blocks

```ruby
def greet_twice
  yield("Alice")
  yield("Bob")
end

greet_twice { |name| puts "Hello, #{name}!" }

# Output:
# Hello, Alice!
# Hello, Bob!
```

## block_given?

Check if a block was provided:

```ruby
def maybe_yield
  if block_given?
    yield
  else
    puts "No block provided"
  end
end

maybe_yield                  # => "No block provided"
maybe_yield { puts "Hi!" }   # => "Hi!"
```

## What are Procs?

Procs are objects that hold blocks of code:

```ruby
# Create a Proc
my_proc = Proc.new { |name| puts "Hello, #{name}!" }

# Call the Proc
my_proc.call("Alice")   # => "Hello, Alice!"

# Alternative syntax
my_proc.("Bob")         # => "Hello, Bob!"
my_proc["Charlie"]      # => "Hello, Charlie!"
```

## Procs as Method Parameters

```ruby
def execute_proc(my_proc)
  my_proc.call
end

greeting = Proc.new { puts "Hello!" }
execute_proc(greeting)  # => "Hello!"
```

## Converting Blocks to Procs

Use `&` to convert block to Proc:

```ruby
def run_block(&block)
  block.call
end

run_block { puts "Hi!" }  # => "Hi!"
```

## Practical Example

```ruby
# Timer method that times block execution
def benchmark
  start_time = Time.now
  yield
  end_time = Time.now
  puts "Time taken: #{end_time - start_time} seconds"
end

benchmark do
  sum = 0
  1_000_000.times { |i| sum += i }
  puts "Sum: #{sum}"
end
```

## Key Takeaways

- Blocks are code chunks: `{}` or `do...end`
- Use `{}` for one line, `do...end` for multiple
- Methods execute blocks with `yield`
- Check for blocks with `block_given?`
- Procs are objects containing code blocks
- Create with `Proc.new { code }`
- Call Procs with `.call()`
- Convert blocks to Procs with `&block`

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Blocks & Procs — Resources (Appendix — External Links)

Recommended reading and quick recipes for blocks, procs, and lambda differences.

- Ruby docs: [Blocks, Procs and Lambdas](https://ruby-doc.org/core-3.2.0/doc/syntax/lambdas_rdoc.html)
- Practical guide: [Ruby closures and performance notes](https://dev.to/ruby)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Doc</th><th>Quick note</th></tr>
  </thead>
  <tbody>
    <tr><td>Proc vs lambda</td><td><a href="https://ruby-doc.org/core-3.2.0/doc/syntax/lambdas_rdoc.html">Ruby lambda docs</a></td><td>Difference in return/arity</td></tr>
    <tr><td>Performance</td><td><a href="https://dev.to/ruby">Ruby community posts</a></td><td>Benchmark when using in hot loops</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Replace a block with an explicit `Proc.new` and observe differences with
`return` in tests.
2. Benchmark invoking a lambda vs a method for a tight loop.

<!-- markdownlint-disable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Blocks & Procs — Deep Dive (Appendix II — External Links)

Advanced examples and patterns for passing blocks, converting to Procs, and performance considerations.

- Official docs: [Ruby Blocks & Procs](https://ruby-doc.org/core-3.2.0/doc/syntax/lambdas_rdoc.html)

```ruby
# Example: converting a block to a Proc and calling it later
def call_with_block(&blk)
  stored = blk
  stored.call(42)
end

call_with_block { |v| puts "Value: #{v}" }
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Link</th><th>Note</th></tr>
  </thead>
  <tbody>
    <tr><td>Proc vs lambda</td><td><a href="https://ruby-doc.org/core-3.2.0/doc/syntax/lambdas_rdoc.html">lambda docs</a></td><td>Arity and return semantics differ</td></tr>
    <tr><td>Performance</td><td><a href="https://dev.to/ruby">Community posts</a></td><td>Benchmark in tight loops</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix II — blocks_procs)

1. Implement a memoizing wrapper that accepts a block and caches results.
2. Measure overhead of Proc allocations in a benchmark and optimize.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: External Tools & Examples (Appendix — External Tools — blocks_procs-ruby)

This compact appendix provides quick references and recipes when working with
Ruby blocks, procs, and common developer tools used in exercises (testing, REPL,
and IO examples). Links point to the official Ruby docs and Enumerable reference
for authoritative guidance.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tool</th><th>Use</th><th>Reference</th></tr>
  </thead>
  <tbody>
    <tr><td>Ruby Core Docs</td><td>Method & IO reference</td><td><a href="https://ruby-doc.org/core/">Ruby Core Docs</a></td></tr>
    <tr><td>Enumerable</td><td>Block-related helpers</td><td><a href="https://ruby-doc.org/core/Enumerable.html">Enumerable</a></td></tr>
    <tr><td>Minitest / RSpec</td><td>Unit testing</td><td><a href="https://github.com/seattlerb/minitest">Minitest</a> / <a href="https://rspec.info/">RSpec</a></td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Quick recipes

- Run a single-file test with Minitest:

```bash
ruby -r minitest/autorun my_test.rb
```

- Start an interactive REPL session with documentation lookup:

```bash
irb --simple-prompt
# inside irb: `require 'rdoc'` and `RDoc::ri` is available on some systems; otherwise visit ruby-doc.org`
```

### Example Minitest skeleton

```ruby
require 'minitest/autorun'
require_relative '../lib/my_library'

class TestBlocks < Minitest::Test
  def test_block_yields
    result = MyLibrary.invoke_with_block { |x| x * 2 }
    assert_equal 4, result
  end
end
```

### Exercises (blocks_procs-ruby)

1. Write a small Minitest that verifies a method yields expected values to a
   block; include an assertion for block return behavior.
2. Use `Enumerable#chunk` or `each_with_index` to solve a small data-grouping
   problem and document your approach in the lesson.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Blocks & Procs — Recipes and References (Appendix — blocks_procs-ruby2)

This appendix collects quick-reference recipes, test harness patterns, and portability notes for working with blocks, Procs, and lambdas in Ruby. It is intentionally append-only and safe for linting.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Why it matters</th><th>Reference</th></tr>
  </thead>
  <tbody>
    <tr><td>Proc vs Lambda</td><td>Return/arity differences</td><td><a href="https://ruby-doc.org/core/Proc.html">Ruby Proc docs</a></td></tr>
    <tr><td>Block to Proc</td><td>Reify block when you need an object</td><td><a href="https://ruby-doc.org/core/Kernel.html#method-i-caller">Kernel/Proc notes</a></td></tr>
    <tr><td>Testing patterns</td><td>Capture stdout, assert side-effects</td><td><a href="https://github.com/seattlerb/minitest">Minitest</a></td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-disable MD013 -->
### Quick recipes (Appendix — blocks_procs-ruby2)

- Capture stdout in tests (useful when blocks print):

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

- Convert a block into a Proc and store it for later execution:

```ruby
stored = nil

def store_block(&b)
  stored = b
end

store_block { |x| puts x }
# later: stored.call(42)
```

<!-- markdownlint-enable MD013 -->
### CI / Smoke snippet

Include a smoke job that runs a quick script exercising block behaviour. This
keeps lessons runnable in minimal CI.

```yaml
# .github/workflows/lesson-smoke.yml
name: Lesson smoke
on: [push]
jobs:
  smoke:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run examples
        run: ruby -e "require './learn/curriculum/ruby/lessons/blocks_procs.rb' if File.exist?('learn/curriculum/ruby/lessons/blocks_procs.rb') || true"
```

### Advanced example: memoizing a block wrapper

```ruby
def memoize_block(&blk)
  cache = {}
  lambda do |arg|
    return cache[arg] if cache.key?(arg)
    cache[arg] = blk.call(arg)
  end
end

fib = memoize_block do |n|
  return n if n < 2
  fib.call(n - 1) + fib.call(n - 2)
end
```

> Note: The example above shows pattern intent; real memoization for recursive
blocks requires careful scoping to avoid immediate recursion issues.

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — blocks_procs-ruby2)

1. Implement a small benchmark comparing invoking a Proc vs calling a method in
   a tight loop. Report average time over many iterations.
2. Write a Minitest that asserts a method yields to a block with expected
   arguments. Use `capture_stdout` helper to verify printed output.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Blocks, Procs & Lambdas — Calling Conventions & Tips (Appendix — blocks_procs-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concept</th><th>Behavior</th><th>Recommendation</th></tr>
  </thead>
  <tbody>
    <tr><td>Block</td><td>Implicit; passed to method</td><td>Use for short callbacks; use `yield` for speed</td></tr>
    <tr><td>Proc</td><td>Object wrapper; lenient arity</td><td>Use when you need first-class callable but loose arity</td></tr>
    <tr><td>Lambda</td><td>Callable with strict arity and `return` semantics</td><td>Use when you need function-like behavior</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-enable MD013 -->
### Examples

```ruby
def with_logging
  yield
end

p = Proc.new { |x, y| x }
lam = ->(x) { x }
```

### Exercises

1. Convert a block-based API to accept an explicit `&block` and write tests for
   both forms.
2. Demonstrate arity differences with small examples and assert behavior.
