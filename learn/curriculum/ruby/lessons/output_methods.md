# String Concatenation

Learn how to combine strings in Ruby using different methods.

## Why Combine Strings?

Often you need to build strings from multiple pieces:

- Creating messages with user input
- Formatting output
- Building file paths or URLs

## Method 1: Using the + Operator

The simplest way to combine strings:

```ruby
first = "Hello"
last = "World"
result = first + " " + last
# result is "Hello World"

# You can chain multiple additions
greeting = "Hello" + ", " + "Alice" + "!"
# greeting is "Hello, Alice!"
```

## Method 2: String Interpolation

More elegant and powerful - use `#{}` inside double quotes:

```ruby
name = "Alice"
greeting = "Hello, #{name}!"
# greeting is "Hello, Alice!"

# Works with expressions
age = 25
message = "You are #{age} years old"
# message is "You are 25 years old"

# Can include calculations
price = 10
tax = 2
total = "Total: $#{price + tax}"
# total is "Total: $12"
```

## When to Use Each

### Use + operator when

- Combining simple strings
- You prefer explicit concatenation

### Use interpolation when

- Including variables in strings
- Combining multiple elements
- Working with expressions or calculations
- More readable code (preferred in Ruby)

## Important Notes

**Return vs Print:**

- Methods should **return** strings (not print them)
- Return the combined string so it can be used by other code
- Use `puts` or `print` only when you specifically want to display output

```ruby
# Good: Returns the string
def make_greeting(name)
  "Hello, #{name}!"  # Returns the string
end

# Less useful: Only prints, returns nil
def print_greeting(name)
  puts "Hello, #{name}!"  # Prints but returns nil
end
```

## Try It Yourself

Complete the exercise to practice combining strings and returning the results!
