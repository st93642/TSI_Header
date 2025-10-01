# Case When Statements

The `case` statement is Ruby's way of handling multiple conditions on the same value. It's more elegant than multiple `if-elsif` statements.

## Basic Case Statement

```ruby
day = "Monday"

case day
when "Monday"
  puts "Start of work week"
when "Tuesday", "Wednesday", "Thursday"
  puts "Midweek"
when "Friday"
  puts "Almost weekend!"
when "Saturday", "Sunday"
  puts "Weekend!"
else
  puts "Invalid day"
end
```

## Case with Return Values

```ruby
grade = 85

result = case grade
when 90..100
  "A"
when 80..89
  "B"
when 70..79
  "C"
else
  "F"
end

puts "Your grade: #{result}"
```

## Case with Ranges

```ruby
age = 25

category = case age
when 0..2
  "Baby"
when 3..12
  "Child"
when 13..19
  "Teenager"
when 20..64
  "Adult"
else
  "Senior"
end
```

## Key Takeaways

- `case` is cleaner than multiple `if-elsif`
- Use `when` for each condition
- Can match multiple values: `when "a", "b"`
- Works with ranges: `when 1..10`
- Can return values like any expression
