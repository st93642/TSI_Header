# Conditionals and Control Flow

Control flow statements allow your program to make decisions based on conditions. Ruby provides elegant syntax for conditional logic.

## The `if` Statement

```ruby
age = 18
if age >= 18
  puts "You are an adult"
end
```

## `if-else` Statement

```ruby
temperature = 25

if temperature > 30
  puts "It's hot!"
else
  puts "Weather is pleasant"
end
```

## `if-elsif-else` Statement

```ruby
score = 85

if score >= 90
  puts "Grade: A"
elsif score >= 80
  puts "Grade: B"
elsif score >= 70
  puts "Grade: C"
else
  puts "Grade: F"
end
```

## Inline `if` Modifier

Ruby allows concise one-line conditions:

```ruby
puts "You can vote!" if age >= 18
send_email if user_subscribed
```

## Comparison Operators

```ruby
5 == 5      # => true  (equal to)
5 != 3      # => true  (not equal to)
5 > 3       # => true  (greater than)
5 < 3       # => false (less than)
5 >= 5      # => true  (greater or equal)
5 <= 3      # => false (less or equal)
```

## Logical Operators

```ruby
# AND - both must be true
if age >= 18 && has_license
  puts "You can drive"
end

# OR - at least one must be true
if is_weekend || is_holiday
  puts "No work today!"
end

# NOT - negates condition
if !raining
  puts "Let's go outside"
end
```

## Truthiness in Ruby

Only `false` and `nil` are falsy - everything else is truthy:

```ruby
if 0
  puts "0 is truthy!"  # This executes
end

if ""
  puts "Empty string is truthy!"  # This executes
end
```

## Ternary Operator

```ruby
age = 20
status = age >= 18 ? "Adult" : "Minor"
```

## Key Takeaways

- `if`, `elsif`, `else` control program flow
- Inline modifiers: `puts "Hi" if condition`
- Only `false` and `nil` are falsy
- Ternary: `condition ? true_value : false_value`
- Logical operators: `&&` (AND), `||` (OR), `!` (NOT)
- Comparison: `==`, `!=`, `>`, `<`, `>=`, `<=`
