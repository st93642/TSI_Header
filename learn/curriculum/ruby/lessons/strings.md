# Working with Strings

Strings are one of the most fundamental data types in Ruby. They represent sequences of characters and are used extensively in every Ruby program.

## What is a String?

A string is a sequence of characters enclosed in either single quotes (`'`) or double quotes (`"`). Strings can contain letters, numbers, symbols, and whitespace.

```ruby
# Single quotes
name = 'Alice'

# Double quotes
greeting = "Hello, World!"

# Strings with numbers and symbols
code = "Ruby 3.0 is amazing!"
```

## Single vs Double Quotes

The main difference between single and double quotes is that double quotes allow:

- **String interpolation** (embedding variables and expressions)
- **Escape sequences** (like `\n` for newline, `\t` for tab)

```ruby
name = "Bob"

# String interpolation (only works with double quotes)
message = "Hello, #{name}!"  # => "Hello, Bob!"

# Escape sequences
multi_line = "Line 1\nLine 2\nLine 3"
```

Single quotes treat everything literally:

```ruby
name = "Alice"
message = 'Hello, #{name}!'  # => "Hello, #{name}!" (literal)
```

## Common String Methods

### Changing Case

```ruby
text = "Ruby Programming"
text.upcase      # => "RUBY PROGRAMMING"
text.downcase    # => "ruby programming"
text.capitalize  # => "Ruby programming"
```

### String Concatenation

```ruby
first_name = "John"
last_name = "Doe"
full_name = first_name + " " + last_name  # => "John Doe"
```

### Accessing Characters

```ruby
word = "Ruby"
word[0]      # => "R"
word[-1]     # => "y"
word[0..2]   # => "Rub"
```

## Key Takeaways

- Strings can use single (`'`) or double (`"`) quotes
- Double quotes enable interpolation with `#{}`
- Ruby has extensive built-in string methods
- Use `.upcase`, `.downcase`, `.capitalize` for case changes
- String interpolation is cleaner than concatenation
