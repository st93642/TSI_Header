# TSI Student Portal Authentication System

## Introduction

In Ruby, boolean values represent truth and falsehood. Understanding booleans is essential for making decisions in your programs. Ruby has two boolean values: `true` and `false`, plus a special value called `nil` that represents "nothing."

## Boolean Values

Ruby has two primary boolean values:

- `true` - represents truth
- `false` - represents falsehood

```ruby
is_ruby_fun = true
is_programming_hard = false

puts is_ruby_fun     # true
puts is_programming_hard  # false
```

## Comparison Operators

Comparison operators compare values and return boolean results:

```ruby
# Equality
puts 5 == 5    # true
puts 5 == 6    # false

# Inequality
puts 5 != 6    # true
puts 5 != 5    # false

# Greater than
puts 10 > 5    # true
puts 5 > 10    # false

# Less than
puts 5 < 10    # true
puts 10 < 5    # false

# Greater than or equal
puts 5 >= 5    # true
puts 5 >= 4    # true
puts 5 >= 6    # false

# Less than or equal
puts 5 <= 5    # true
puts 5 <= 6    # true
puts 5 <= 4    # false
```

## Logical Operators

Logical operators combine boolean values:

### AND (&&)

Returns `true` only if both operands are true:

```ruby
puts true && true    # true
puts true && false   # false
puts false && true   # false
puts false && false  # false

# Practical example
age = 25
has_license = true
can_drive = age >= 18 && has_license
puts can_drive  # true
```

### OR (||)

Returns `true` if at least one operand is true:

```ruby
puts true || true    # true
puts true || false   # true
puts false || true   # true
puts false || false  # false

# Practical example
is_weekend = false
is_holiday = true
can_sleep_in = is_weekend || is_holiday
puts can_sleep_in  # true
```

### NOT (!)

Reverses the boolean value:

```ruby
puts !true   # false
puts !false  # true

# Practical example
is_raining = false
should_take_umbrella = !is_raining
puts should_take_umbrella  # true
```

## Truthiness and Falsiness

In Ruby, every value is either "truthy" or "falsy" in conditional expressions. Only two values are falsy:

- `false` - the boolean false value
- `nil` - represents "nothing" or "no value"

Everything else is truthy, including:

- `true`
- Numbers (even 0)
- Strings (even empty strings)
- Arrays (even empty arrays)

```ruby
# Falsy values
puts "false is falsy:"    # false is falsy:
puts !!false              # false
puts "nil is falsy:"      # nil is falsy:
puts !!nil                # false

# Truthy values
puts "true is truthy:"    # true is truthy:
puts !!true               # true
puts "0 is truthy:"       # 0 is truthy:
puts !!0                  # true
puts "empty string is truthy:"  # empty string is truthy:
puts !!""                 # true
puts "empty array is truthy:"   # empty array is truthy:
puts !![]                 # true
```

## The nil Value

`nil` represents the absence of a value. It's commonly returned when a method doesn't find what it's looking for:

```ruby
# Examples of nil
result = [1, 2, 3].find { |n| n > 5 }
puts result  # nil (no number > 5)

name = nil
puts name    # nil

# Checking for nil
puts name.nil?  # true
puts result.nil? # true
```

## Basic Conditional Expressions

Boolean values are essential for control flow. Here's a preview of how they're used:

```ruby
age = 20
is_adult = age >= 18

if is_adult
  puts "You can vote!"
else
  puts "You cannot vote yet."
end

# Output: You can vote!
```

## Common Patterns

### Checking for nil

```ruby
name = get_user_name()  # This might return nil

if name
  puts "Hello, #{name}!"
else
  puts "Hello, Guest!"
end
```

### Multiple conditions

```ruby
temperature = 75
is_raining = false

if temperature > 70 && !is_raining
  puts "Perfect weather for a picnic!"
end
```

## Summary

- Ruby has two boolean values: `true` and `false`
- Comparison operators (`==`, `!=`, `<`, `>`, `<=`, `>=`) return boolean values
- Logical operators (`&&`, `||`, `!`) combine boolean values
- Only `false` and `nil` are falsy; everything else is truthy
- `nil` represents the absence of a value
- Boolean logic is fundamental to conditional programming

Understanding these concepts will help you write more effective Ruby code and make logical decisions in your programs.
