# Range Objects in Ruby

Master Ruby ranges for representing sequences and intervals.

## What are Ranges?

A range represents an interval with a beginning and end:

```ruby
(1..5)    # Inclusive range: 1, 2, 3, 4, 5
(1...5)   # Exclusive range: 1, 2, 3, 4
('a'..'z') # Letter range: a through z
```

## Two Dots vs Three Dots

```ruby
# Two dots (..) - includes the end value
(1..5).to_a   # [1, 2, 3, 4, 5]

# Three dots (...) - excludes the end value
(1...5).to_a  # [1, 2, 3, 4]
```

## Creating Ranges

```ruby
# Number ranges
age_range = 18..65
small_numbers = 1..10
negative_range = -5..5

# Letter ranges
alphabet = 'a'..'z'
vowels = 'a'..'e'

# Using Range.new
range1 = Range.new(1, 10)      # Same as (1..10)
range2 = Range.new(1, 10, true) # Same as (1...10) - true excludes end
```

## Converting Ranges to Arrays

```ruby
(1..5).to_a        # [1, 2, 3, 4, 5]
('a'..'e').to_a    # ["a", "b", "c", "d", "e"]
```

## Checking Membership

```ruby
age_range = 18..65

age_range.include?(25)  # true
age_range.include?(70)  # false

# cover? is faster for simple checks
age_range.cover?(25)    # true
age_range.cover?(70)    # false

# Difference: cover? only checks bounds
(1..10).cover?(5.5)     # true
(1..10).include?(5.5)   # false (5.5 not in sequence)
```

## Range Methods

```ruby
range = 1..10

# Get endpoints
range.first      # 1
range.last       # 10
range.begin      # 1
range.end        # 10

# Check if exclusive
range.exclude_end?  # false

# Get size (for numeric ranges)
range.size       # 10
range.count      # 10

# Get min and max
range.min        # 1
range.max        # 10
```

## Iterating Over Ranges

```ruby
# Using each
(1..5).each do |num|
  puts num
end

# With map
squared = (1..5).map { |n| n * n }
# [1, 4, 9, 16, 25]

# Select even numbers
evens = (1..10).select { |n| n.even? }
# [2, 4, 6, 8, 10]
```

## Ranges in Case Statements

```ruby
def grade(score)
  case score
  when 90..100
    'A'
  when 80..89
    'B'
  when 70..79
    'C'
  when 60..69
    'D'
  else
    'F'
  end
end

grade(85)  # "B"
```

## Ranges in Conditionals

```ruby
age = 25

if (18..65).include?(age)
  puts "Working age"
end

# Check multiple conditions
valid_ports = 1024..65535
if valid_ports.include?(port)
  # Port is in valid range
end
```

## Array Slicing with Ranges

```ruby
numbers = [10, 20, 30, 40, 50, 60]

numbers[1..3]    # [20, 30, 40]
numbers[1...3]   # [20, 30]
numbers[2..-1]   # [30, 40, 50, 60] (to end)
numbers[0..2]    # [10, 20, 30]
```

## String Ranges

```ruby
# Check if character is in range
('a'..'z').include?('m')  # true
('A'..'Z').include?('m')  # false

# Generate sequences
('a'..'f').to_a  # ["a", "b", "c", "d", "e", "f"]
```

## Endless Ranges (Ruby 2.6+)

```ruby
# Starts at value, no end
(1..)     # 1, 2, 3, ... infinity
(10..)    # 10, 11, 12, ... infinity

# Useful with array slicing
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
numbers[5..]   # [6, 7, 8, 9, 10] (from index 5 to end)

# Beginless ranges (Ruby 2.7+)
(..10)    # -infinity ... 10
(...10)   # -infinity ... 9
```

## Practical Examples

### Age Validation

```ruby
ADULT_AGE = 18..120

def adult?(age)
  ADULT_AGE.include?(age)
end
```

### Date Ranges

```ruby
require 'date'

start_date = Date.new(2024, 1, 1)
end_date = Date.new(2024, 12, 31)

year_2024 = (start_date..end_date)

today = Date.today
if year_2024.include?(today)
  puts "Date is in 2024"
end
```

### Filtering Arrays

```ruby
numbers = [1, 15, 23, 45, 67, 89, 12, 34]

# Get numbers in range
teens = numbers.select { |n| (13..19).include?(n) }
# [15]

# Get numbers outside range
not_teens = numbers.reject { |n| (13..19).include?(n) }
# [1, 23, 45, 67, 89, 12, 34]
```

### Character Validation

```ruby
def lowercase_letter?(char)
  ('a'..'z').include?(char)
end

def digit?(char)
  ('0'..'9').include?(char)
end
```

## Performance Tips

```ruby
# For simple numeric checks, cover? is faster
range = 1..1000000
range.cover?(500000)   # Fast - just checks bounds
range.include?(500000) # Slower - iterates

# Use ranges instead of arrays when possible
(1..1000000)           # Memory efficient
(1..1000000).to_a      # Creates huge array!
```

## Common Patterns

```ruby
# Generate sequence
(1..10).to_a

# Loop n times
(1..5).each { |i| puts "Iteration #{i}" }

# Check if value in interval
(0..100).include?(score)

# Array slice
array[2..5]

# Case statement
case value
when 1..10 then "low"
when 11..20 then "medium"
else "high"
end
```

## Try It Yourself

Complete the exercise to practice using ranges!
