# Blocks and Procs

Blocks are chunks of code that can be passed to methods. They are fundamental to Ruby's iterator pattern.

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
