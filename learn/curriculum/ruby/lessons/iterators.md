# Iterators in Ruby

Iterators are methods that loop through collections. Ruby provides powerful iterator methods that make working with arrays and hashes elegant and expressive.

## The `.each` Iterator

The most common iterator - processes each element:

```ruby
# Array iteration
fruits = ["apple", "banana", "orange"]

fruits.each do |fruit|
  puts fruit
end

# Hash iteration
person = { name: "Alice", age: 30, city: "NYC" }

person.each do |key, value|
  puts "#{key}: #{value}"
end
```

## `.each_with_index`

Iterate with element and index:

```ruby
colors = ["red", "green", "blue"]

colors.each_with_index do |color, index|
  puts "#{index}: #{color}"
end

# Output:
# 0: red
# 1: green
# 2: blue
```

## `.map` (Transform Elements)

Creates a new array by transforming each element:

```ruby
numbers = [1, 2, 3, 4, 5]

# Double each number
doubled = numbers.map { |n| n * 2 }
# => [2, 4, 6, 8, 10]

# Convert to strings
strings = numbers.map { |n| n.to_s }
# => ["1", "2", "3", "4", "5"]

# Uppercase names
names = ["alice", "bob", "charlie"]
upper = names.map { |name| name.upcase }
# => ["ALICE", "BOB", "CHARLIE"]
```

## `.select` (Filter Elements)

Returns elements that meet a condition:

```ruby
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Get even numbers
evens = numbers.select { |n| n.even? }
# => [2, 4, 6, 8, 10]

# Get numbers greater than 5
large = numbers.select { |n| n > 5 }
# => [6, 7, 8, 9, 10]
```

## `.reject` (Inverse of Select)

Returns elements that DON'T meet a condition:

```ruby
numbers = [1, 2, 3, 4, 5, 6]

# Reject even numbers (keep odds)
odds = numbers.reject { |n| n.even? }
# => [1, 3, 5]
```

## `.reduce` (Accumulate)

Combines all elements into a single value:

```ruby
numbers = [1, 2, 3, 4, 5]

# Sum all numbers
sum = numbers.reduce(0) { |total, n| total + n }
# => 15

# Shorter version
sum = numbers.reduce(:+)
# => 15

# Product of all numbers
product = numbers.reduce(1) { |prod, n| prod * n }
# => 120
```

## `.find` (First Match)

Returns first element matching condition:

```ruby
numbers = [1, 3, 5, 8, 9, 10]

# Find first even number
first_even = numbers.find { |n| n.even? }
# => 8
```

## `.any?` and `.all?`

```ruby
numbers = [2, 4, 6, 8]

# Check if any are even
numbers.any? { |n| n.even? }  # => true

# Check if all are even
numbers.all? { |n| n.even? }  # => true

# Check if all are greater than 5
numbers.all? { |n| n > 5 }    # => false
```

## `.times` Iterator

Repeat code N times:

```ruby
5.times { puts "Hello!" }

# With index
3.times do |i|
  puts "Iteration #{i}"
end
```

## Range Iterators

```ruby
# Count up
1.upto(5) { |i| puts i }

# Count down
5.downto(1) { |i| puts i }

# Step through
(0..10).step(2) { |i| puts i }  # Even numbers
```

## Chaining Iterators

```ruby
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Chain multiple operations
result = numbers
  .select { |n| n.even? }  # Get evens: [2,4,6,8,10]
  .map { |n| n * 2 }        # Double: [4,8,12,16,20]
  .reduce(:+)               # Sum: 60

puts result  # => 60
```

## Key Takeaways

- `.each` - iterate over all elements
- `.map` - transform each element, return new array
- `.select` - filter elements that match condition
- `.reject` - filter elements that don't match
- `.reduce` - accumulate into single value
- `.find` - return first matching element
- `.any?` / `.all?` - check conditions
- Iterators can be chained for powerful operations
