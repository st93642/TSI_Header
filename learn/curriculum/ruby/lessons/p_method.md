# Using P for Debugging

The `p` method is a powerful debugging tool in Ruby.

## What is P?

`p` (short for "puts with inspect") is used for debugging. It:

- Displays the raw representation of objects
- Shows quotes around strings
- Returns the object (unlike puts which returns nil)
- Useful for seeing data types

## Examples

### With Strings

```ruby
puts "Hello"
# Output: Hello

p "Hello"
# Output: "Hello" (with quotes)
```

### With Numbers

```ruby
puts 2024
# Output: 2024

p 2024
# Output: 2024 (same for numbers)
```

### With Arrays

```ruby
arr = [1, 2, 3]

puts arr
# Output:
# 1
# 2
# 3

p arr
# Output: [1, 2, 3] (single line, raw format)
```

## Why Use P?

Perfect for debugging because:

- See data types clearly (strings have quotes, numbers don't)
- See entire arrays/hashes on one line
- Quickly inspect object values
- Returns the value so you can chain methods

## Common Pattern

```ruby
# Debugging in the middle of code
result = some_method.tap { |x| p x }.other_method

# Or simpler:
x = calculate_something
p x  # See what x is
```

## Try It Yourself

Complete the exercise to practice returning values!
