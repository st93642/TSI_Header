# Regular Expressions (Regex)

Master pattern matching with regular expressions in Ruby.

## What are Regular Expressions?

Regular expressions (regex) are powerful patterns for matching text. They let you:

- Validate formats (email, phone, postal codes)
- Search and find specific patterns
- Extract data from text
- Replace text patterns

## Basic Syntax

In Ruby, regex patterns are written between forward slashes:

```ruby
/pattern/
```

## Simple Patterns

Match exact text:

```ruby
text = "Hello, World!"
text =~ /World/  # Returns 7 (position where match starts)
text =~ /Ruby/   # Returns nil (no match)

# Use match? for boolean result
text.match?(/World/)  # true
text.match?(/Ruby/)   # false
```

## Special Characters

### The Dot (.) - Any Single Character

```ruby
/c.t/.match?("cat")  # true
/c.t/.match?("cot")  # true
/c.t/.match?("cut")  # true
/c.t/.match?("cart") # false (two characters between c and t)
```

### Anchors - Start and End

```ruby
# ^ = start of string
/^Hello/.match?("Hello World")  # true
/^World/.match?("Hello World")  # false

# $ = end of string
/World$/.match?("Hello World")  # true
/Hello$/.match?("Hello World")  # false

# Both anchors - exact match
/^test$/.match?("test")   # true
/^test$/.match?("testing") # false
```

### Character Classes - Options

```ruby
# [abc] = matches a, b, or c
/[aeiou]/.match?("hello")  # true (matches 'e')

# [0-9] = any digit
/[0-9]/.match?("Room 123") # true

# [a-z] = any lowercase letter
/[a-z]/.match?("Hello")    # true

# [A-Z] = any uppercase letter
/[A-Z]/.match?("hello")    # false
```

## Quantifiers - How Many?

```ruby
# * = zero or more
/ab*c/.match?("ac")    # true (zero b's)
/ab*c/.match?("abc")   # true (one b)
/ab*c/.match?("abbbc") # true (three b's)

# + = one or more
/ab+c/.match?("ac")    # false (needs at least one b)
/ab+c/.match?("abc")   # true

# ? = zero or one (optional)
/colou?r/.match?("color")   # true
/colou?r/.match?("colour")  # true

# {n} = exactly n times
/\d{3}/.match?("123")   # true (exactly 3 digits)
/\d{3}/.match?("12")    # false

# {n,m} = between n and m times
/\d{2,4}/.match?("12")    # true
/\d{2,4}/.match?("1234")  # true
/\d{2,4}/.match?("12345") # true (matches first 4)
```

## Shorthand Character Classes

Ruby provides shortcuts for common patterns:

```ruby
\d  # digit [0-9]
\w  # word character [a-zA-Z0-9_]
\s  # whitespace (space, tab, newline)

\D  # NOT a digit
\W  # NOT a word character
\S  # NOT whitespace

# Examples
/\d{3}-\d{4}/.match?("555-1234")  # true (phone number)
/\w+@\w+\.\w+/.match?("test@example.com")  # true (simple email)
```

## Extraction with Capture Groups

Use parentheses to capture parts of the match:

```ruby
text = "My phone is 555-1234"
match = text.match(/(\d{3})-(\d{4})/)

match[0]  # "555-1234" (full match)
match[1]  # "555" (first capture group)
match[2]  # "1234" (second capture group)

# Named captures for clarity
match = text.match(/(?<area>\d{3})-(?<number>\d{4})/)
match[:area]    # "555"
match[:number]  # "1234"
```

## Common Patterns

### Email Validation (Simple)

```ruby
email_pattern = /\A[\w+\-.]+@[a-z\d\-]+(\.[a-z\d\-]+)*\.[a-z]+\z/i
email_pattern.match?("user@example.com")  # true
```

### Phone Number

```ruby
phone_pattern = /\A\d{3}-\d{3}-\d{4}\z/
phone_pattern.match?("555-123-4567")  # true
```

### URL

```ruby
url_pattern = /\Ahttps?:\/\/[\w\-.]+(\/[\w\-.]*)*\z/
url_pattern.match?("https://example.com/page")  # true
```

## String Methods with Regex

### scan - Find All Matches

```ruby
text = "The prices are $10, $25, and $100"
text.scan(/\$\d+/)  # ["$10", "$25", "$100"]
```

### gsub - Replace All Matches

```ruby
text = "Hello World"
text.gsub(/[aeiou]/, "*")  # "H*ll* W*rld"

# With block for custom replacement
text.gsub(/\w+/) { |word| word.capitalize }  # "Hello World"
```

### split - Split by Pattern

```ruby
"one,two;three:four".split(/[,;:]/)  # ["one", "two", "three", "four"]
```

## Flags

Modify regex behavior with flags:

```ruby
/pattern/i   # case-insensitive
/pattern/m   # multiline mode (. matches newlines)
/pattern/x   # extended mode (allows comments and whitespace)

# Example
/hello/i.match?("HELLO")  # true (case-insensitive)
```

## Try It Yourself

Complete the exercise to practice regex pattern matching!
