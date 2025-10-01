# Hello World with Puts

Welcome to your first Ruby lesson! Let's learn about the `puts` method and the important difference between printing and returning values.

## What is Puts?

`puts` (short for "put string") is Ruby's most common way to display output. It:

- Displays text to the console
- Automatically adds a newline at the end
- Returns `nil` (not the string you printed!)

## Basic Usage

```ruby
puts "Hello, World!"
# Output: Hello, World!
# (cursor moves to next line)
```

## String Interpolation

You can insert variables into strings using `#{}`:

```ruby
name = "Alice"
puts "Hello, #{name}!"
# Output: Hello, Alice!
```

## Multiple Lines

Each `puts` starts on a new line:

```ruby
puts "Line 1"
puts "Line 2"
puts "Line 3"
# Output:
# Line 1
# Line 2
# Line 3
```

## Important: Printing vs Returning

There's a crucial difference in Ruby:

- **puts** displays output but returns `nil`
- **return** gives back a value that can be used

```ruby
# Printing (not useful for method results)
def greet_print(name)
  puts "Hello, #{name}!"  # Shows output, returns nil
end

# Returning (correct for methods that produce values)
def greet_return(name)
  "Hello, #{name}!"  # Returns the string
end

result = greet_print("Alice")  # Shows "Hello, Alice!" but result is nil
result = greet_return("Alice") # result is "Hello, Alice!" (can be used)
```

## Try It Yourself

Now complete the exercise to practice **returning** strings with interpolation!
