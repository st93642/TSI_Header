# Advanced Enumerables

## Overview

Ruby's `Enumerable` module provides powerful methods for working with collections. While basic iteration with `each` is common, advanced enumerable methods allow for sophisticated data transformations, filtering, grouping, and aggregation operations.

## Key Advanced Methods

### each_with_index

Iterates over elements while providing access to their index position.

```ruby
fruits = ['apple', 'banana', 'cherry']

fruits.each_with_index do |fruit, index|
  puts "#{index + 1}. #{fruit}"
end
# Output:
# 1. apple
# 2. banana
# 3. cherry
```

### reduce (inject)

Combines all elements of a collection into a single value using an accumulator.

```ruby
# Sum all numbers
numbers = [1, 2, 3, 4, 5]
sum = numbers.reduce(0) { |total, num| total + num }
puts sum  # => 15

# Find the longest word
words = ['cat', 'elephant', 'dog', 'hippopotamus']
longest = words.reduce do |memo, word|
  memo.length > word.length ? memo : word
end
puts longest  # => hippopotamus

# Build a hash from an array
pairs = [['name', 'Alice'], ['age', 30], ['city', 'NYC']]
hash = pairs.reduce({}) do |result, (key, value)|
  result[key] = value
  result
end
puts hash  # => {"name"=>"Alice", "age"=>30, "city"=>"NYC"}
```

### select (filter)

Returns a new array containing elements that match a condition.

```ruby
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Even numbers
evens = numbers.select { |n| n.even? }
puts evens.inspect  # => [2, 4, 6, 8, 10]

# Numbers greater than 5
big_numbers = numbers.select { |n| n > 5 }
puts big_numbers.inspect  # => [6, 7, 8, 9, 10]

# Filter hash values
person = { name: 'Alice', age: 30, city: 'NYC', salary: 75000 }
high_values = person.select { |key, value| value.is_a?(Integer) && value > 25 }
puts high_values.inspect  # => {:age=>30, :salary=>75000}
```

### reject

Returns a new array containing elements that do NOT match a condition (opposite of select).

```ruby
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Remove even numbers
odds = numbers.reject { |n| n.even? }
puts odds.inspect  # => [1, 3, 5, 7, 9]

# Remove short words
words = ['cat', 'elephant', 'dog', 'hippopotamus', 'ant']
long_words = words.reject { |word| word.length < 4 }
puts long_words.inspect  # => ["elephant", "hippopotamus"]
```

### map (collect)

Transforms each element and returns a new array with the transformed values.

```ruby
numbers = [1, 2, 3, 4, 5]

# Square each number
squares = numbers.map { |n| n ** 2 }
puts squares.inspect  # => [1, 4, 9, 16, 25]

# Convert to strings with formatting
formatted = numbers.map { |n| "Number: #{n}" }
puts formatted.inspect  # => ["Number: 1", "Number: 2", "Number: 3", "Number: 4", "Number: 5"]

# Transform hash values
person = { name: 'Alice', age: 30, city: 'NYC' }
uppercased = person.map { |key, value| [key, value.to_s.upcase] }.to_h
puts uppercased.inspect  # => {:name=>"ALICE", :age=>"30", :city=>"NYC"}
```

### group_by

Groups elements by a common characteristic, returning a hash where keys are the grouping criteria and values are arrays of matching elements.

```ruby
words = ['cat', 'dog', 'elephant', 'ant', 'bird', 'cow']

# Group by word length
by_length = words.group_by { |word| word.length }
puts by_length.inspect
# => {3=>["cat", "dog", "cow"], 8=>["elephant"], 3=>["ant"], 4=>["bird"]}

# Group numbers by parity
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
by_parity = numbers.group_by { |n| n.even? ? :even : :odd }
puts by_parity.inspect
# => {:odd=>[1, 3, 5, 7, 9], :even=>[2, 4, 6, 8, 10]}

# Group people by age range
people = [
  { name: 'Alice', age: 25 },
  { name: 'Bob', age: 35 },
  { name: 'Charlie', age: 28 },
  { name: 'David', age: 42 }
]

by_age_group = people.group_by do |person|
  case person[:age]
  when 0..29 then 'young'
  when 30..39 then 'middle'
  else 'senior'
  end
end
puts by_age_group.inspect
# => {"young"=>[{"name"=>"Alice", "age"=>25}, {"name"=>"Charlie", "age"=>28}], "middle"=>[{"name"=>"Bob", "age"=>35}], "senior"=>[{"name"=>"David", "age"=>42}]}
```

### sort_by

Sorts elements based on a custom criteria, often more efficient than sort when the sort key is expensive to compute.

```ruby
words = ['Ruby', 'Python', 'JavaScript', 'Java', 'C++', 'Go']

# Sort by length (shortest first)
by_length = words.sort_by { |word| word.length }
puts by_length.inspect  # => ["Go", "Java", "Ruby", "C++", "Python", "JavaScript"]

# Sort by length (longest first)
by_length_desc = words.sort_by { |word| -word.length }
puts by_length_desc.inspect  # => ["JavaScript", "Python", "Ruby", "Java", "C++", "Go"]

# Sort people by age
people = [
  { name: 'Alice', age: 25 },
  { name: 'Bob', age: 35 },
  { name: 'Charlie', age: 28 }
]

by_age = people.sort_by { |person| person[:age] }
puts by_age.inspect
# => [{"name"=>"Alice", "age"=>25}, {"name"=>"Charlie", "age"=>28}, {"name"=>"Bob", "age"=>35}]
```

### take and drop

`take(n)` returns the first n elements, `drop(n)` returns all elements except the first n.

```ruby
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# First 3 elements
first_three = numbers.take(3)
puts first_three.inspect  # => [1, 2, 3]

# All except first 3
rest = numbers.drop(3)
puts rest.inspect  # => [4, 5, 6, 7, 8, 9, 10]

# Top 2 students by score
students = [
  { name: 'Alice', score: 95 },
  { name: 'Bob', score: 87 },
  { name: 'Charlie', score: 92 }
]

top_two = students.sort_by { |s| -s[:score] }.take(2)
puts top_two.inspect
# => [{"name"=>"Alice", "score"=>95}, {"name"=>"Charlie", "score"=>92}]
```

### chunk

Groups consecutive elements that share a common property.

```ruby
numbers = [1, 2, 3, 5, 6, 7, 9, 10]

# Group consecutive numbers
consecutive = numbers.chunk_while { |i, j| j == i + 1 }
consecutive.each { |group| puts group.inspect }
# Output:
# [1, 2, 3]
# [5, 6, 7]
# [9, 10]

# Group by parity changes
parity_groups = numbers.chunk { |n| n.even? ? :even : :odd }
parity_groups.each { |key, group| puts "#{key}: #{group.inspect}" }
# Output:
# odd: [1]
# even: [2]
# odd: [3]
# odd: [5]
# even: [6]
# odd: [7]
# odd: [9]
# even: [10]
```

### partition

Divides elements into two groups based on a condition: those that match and those that don't.

```ruby
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Separate even and odd
evens, odds = numbers.partition { |n| n.even? }
puts "Evens: #{evens.inspect}"  # => Evens: [2, 4, 6, 8, 10]
puts "Odds: #{odds.inspect}"    # => Odds: [1, 3, 5, 7, 9]

# Separate passing and failing grades
grades = [85, 92, 78, 96, 88, 73, 89]
passing, failing = grades.partition { |grade| grade >= 80 }
puts "Passing: #{passing.inspect}"  # => Passing: [85, 92, 96, 88, 89]
puts "Failing: #{failing.inspect}"  # => Failing: [78, 73]
```

### zip

Combines multiple arrays element-wise, creating arrays of corresponding elements.

```ruby
names = ['Alice', 'Bob', 'Charlie']
ages = [25, 30, 35]
cities = ['NYC', 'LA', 'Chicago']

# Combine into arrays
combined = names.zip(ages, cities)
puts combined.inspect
# => [["Alice", 25, "NYC"], ["Bob", 30, "LA"], ["Charlie", 35, "Chicago"]]

# Create hashes
people = names.zip(ages).map { |name, age| { name: name, age: age } }
puts people.inspect
# => [{:name=>"Alice", :age=>25}, {:name=>"Bob", :age=>30}, {:name=>"Charlie", :age=>35}]

# Handle different lengths
short = [1, 2]
long = [10, 20, 30, 40]
zipped = short.zip(long)
puts zipped.inspect  # => [[1, 10], [2, 20]]
```

## Method Chaining

One of the most powerful aspects of enumerable methods is that they can be chained together to perform complex operations in a readable way.

```ruby
# Complex data processing example
data = [
  { name: 'Alice', age: 25, score: 95, active: true },
  { name: 'Bob', age: 30, score: 87, active: false },
  { name: 'Charlie', age: 28, score: 92, active: true },
  { name: 'David', age: 35, score: 78, active: true }
]

# Find active students with high scores, sorted by age
result = data
  .select { |person| person[:active] && person[:score] >= 90 }
  .sort_by { |person| person[:age] }
  .map { |person| "#{person[:name]} (#{person[:age]})" }

puts result.inspect  # => ["Alice (25)", "Charlie (28)"]

# Group people by age range and count them
age_groups = data
  .group_by do |person|
    case person[:age]
    when 20..29 then '20s'
    when 30..39 then '30s'
    end
  end
  .transform_values { |group| group.size }

puts age_groups.inspect  # => {"20s"=>2, "30s"=>2}
```

## Performance Considerations

- `sort_by` is often more efficient than `sort` when the comparison key is expensive to compute
- Methods like `lazy` can be used for large datasets to avoid creating intermediate arrays
- Some methods like `inject` can be memory-efficient as they don't create intermediate collections

## Common Patterns

### Transforming and Filtering

```ruby
# Get uppercase versions of long words
words = ['cat', 'dog', 'elephant', 'hippopotamus']
result = words.select { |w| w.length > 3 }.map(&:upcase)
puts result.inspect  # => ["ELEPHANT", "HIPPOPOTAMUS"]
```

### Grouping and Aggregating

```ruby
# Count words by length
words = ['cat', 'dog', 'elephant', 'ant', 'bird']
length_counts = words.group_by(&:length).transform_values(&:size)
puts length_counts.inspect  # => {3=>3, 8=>1, 4=>1}
```

### Finding Extremes

```ruby
# Find the person with the highest score
people = [
  { name: 'Alice', score: 95 },
  { name: 'Bob', score: 87 },
  { name: 'Charlie', score: 92 }
]

top_scorer = people.max_by { |p| p[:score] }
puts "#{top_scorer[:name]} scored #{top_scorer[:score]}"  # => Alice scored 95
```

Advanced enumerable methods are essential for writing clean, efficient Ruby code. They allow you to express complex data transformations and queries in a readable, functional style.
