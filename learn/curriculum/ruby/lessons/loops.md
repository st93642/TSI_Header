# Loops and Iteration

Loops allow you to execute code repeatedly. Ruby provides several ways to create loops, each suited for different situations.

## The `while` Loop

Executes code as long as a condition is true:

```ruby
count = 1

while count <= 5
  puts "Count: #{count}"
  count += 1
end

# Output:
# Count: 1
# Count: 2
# Count: 3
# Count: 4
# Count: 5
```

**Important:** Always ensure the condition will eventually become false to avoid infinite loops!

```ruby
# Infinite loop (BAD - will run forever!)
# while true
#   puts "This never stops!"
# end

# Safe version with break
count = 0
while true
  puts count
  count += 1
  break if count >= 5  # Exit condition
end
```

## The `until` Loop

The opposite of `while` - runs until condition becomes true:

```ruby
count = 1

until count > 5
  puts "Count: #{count}"
  count += 1
end

# Same result as the while loop above
```

## The `for` Loop

Iterate over a range or collection:

```ruby
# Iterate over range
for num in 1..5
  puts "Number: #{num}"
end

# Iterate over array
fruits = ["apple", "banana", "orange"]
for fruit in fruits
  puts fruit
end
```

**Note:** Ruby developers prefer iterators (`.each`, `.times`) over `for` loops.

## The `loop` Method

Creates an infinite loop that must be broken with `break`:

```ruby
loop do
  puts "Enter 'quit' to exit:"
  input = gets.chomp
  break if input == "quit"
  puts "You entered: #{input}"
end
```

## The `.times` Iterator

Execute code a specific number of times:

```ruby
# Simple repetition
5.times do
  puts "Hello!"
end

# With block parameter (index)
5.times do |i|
  puts "Iteration #{i}"
end
# Output: Iteration 0, 1, 2, 3, 4 (starts at 0)
```

## Breaking Out of Loops

### `break` - Exit loop immediately

```ruby
count = 0
while count < 10
  puts count
  break if count == 5  # Exit when count is 5
  count += 1
end
# Output: 0, 1, 2, 3, 4, 5
```

### `next` - Skip to next iteration

```ruby
1.upto(10) do |i|
  next if i.even?  # Skip even numbers
  puts i
end
# Output: 1, 3, 5, 7, 9
```

### `redo` - Restart current iteration

```ruby
count = 0
5.times do |i|
  count += 1
  puts "i: #{i}, count: #{count}"
  redo if count < 3  # Redo first iteration twice
end
```

## Iterating with Ranges

Ranges provide powerful iteration:

```ruby
# Inclusive range (includes end)
(1..5).each do |num|
  puts num
end
# Output: 1, 2, 3, 4, 5

# Exclusive range (excludes end)
(1...5).each do |num|
  puts num
end
# Output: 1, 2, 3, 4

# Countdown
5.downto(1) do |i|
  puts "#{i}..."
end
puts "Blast off!"

# Count up
1.upto(5) do |i|
  puts i
end

# Step through range
(0..10).step(2) do |i|
  puts i  # Prints even numbers: 0, 2, 4, 6, 8, 10
end
```

## Loop Modifiers

Create compact one-line loops:

```ruby
# Inline while
count = 0
puts count += 1 while count < 5

# Inline until
count = 0
puts count += 1 until count >= 5
```

## Practical Examples

### Countdown timer

```ruby
10.downto(1) do |i|
  puts i
  sleep(1)  # Wait 1 second
end
puts "Happy New Year!"
```

### Sum of numbers

```ruby
sum = 0
num = 1

while num <= 10
  sum += num
  num += 1
end

puts "Sum of 1-10: #{sum}"  # => 55
```

### User input validation

```ruby
password = nil

until password == "secret"
  puts "Enter password:"
  password = gets.chomp
  puts "Wrong password!" unless password == "secret"
end

puts "Access granted!"
```

### Multiplication table

```ruby
number = 5

1.upto(10) do |i|
  result = number * i
  puts "#{number} x #{i} = #{result}"
end
```

### Find first even number

```ruby
numbers = [1, 3, 5, 7, 8, 10, 11]
found = nil

for num in numbers
  if num.even?
    found = num
    break  # Stop at first even number
  end
end

puts "First even number: #{found}"
```

## Nested Loops

Loops can be nested for multi-dimensional iteration:

```ruby
# Multiplication table
1.upto(5) do |i|
  1.upto(5) do |j|
    print "#{i * j}\t"
  end
  puts  # New line after each row
end

# Pattern printing
5.times do |i|
  (i + 1).times do
    print "*"
  end
  puts
end
# Output:
# *
# **
# ***
# ****
# *****
```

## Performance Considerations

```ruby
# Less efficient (for loop)
for i in 1..1000
  # do something
end

# More efficient (each with range)
(1..1000).each do |i|
  # do something
end

# Most efficient for simple repetition
1000.times do |i|
  # do something
end
```

## Best Practices

1. **Prefer iterators** (`.each`, `.times`) over `for` loops
2. **Use `break`** to exit loops early when condition met
3. **Avoid infinite loops** - always have exit condition
4. **Use meaningful loop variable names**
5. **Use `next`** to skip iterations instead of deep nesting
6. **Use ranges** for numeric sequences
7. **Consider `.upto`, `.downto`, `.step`** for readability

## Common Patterns

```ruby
# Loop with counter
counter = 0
loop do
  counter += 1
  break if counter > 10
end

# Loop until user quits
loop do
  puts "Menu: 1) Option A  2) Option B  3) Quit"
  choice = gets.chomp.to_i
  break if choice == 3
  # Handle other choices
end

# Retry pattern
attempts = 0
max_attempts = 3

until success || attempts >= max_attempts
  result = try_operation
  attempts += 1
end
```

## Key Takeaways

- `while` loops continue while condition is true
- `until` loops continue until condition is true
- `for` loops iterate over collections (rarely used in Ruby)
- `.times` repeats code a specific number of times
- `break` exits loop, `next` skips iteration, `redo` repeats
- Ranges provide elegant iteration: `1..10`, `1.upto(10)`
- Always have an exit condition to prevent infinite loops
- Ruby developers prefer iterators over traditional loops

## Practice

Try these exercises:

1. Print all numbers from 1 to 100
2. Calculate factorial of a number using a loop
3. Create FizzBuzz: print numbers 1-100, but "Fizz" for multiples of 3, "Buzz" for 5, "FizzBuzz" for both
4. Find all prime numbers up to 50
5. Build a number guessing game with limited attempts
