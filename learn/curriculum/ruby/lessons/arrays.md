# Arrays: Collections in Ruby

Arrays are ordered collections that can store multiple values. They are one of the most commonly used data structures in Ruby.

## Creating Arrays

```ruby
# Empty array
my_array = []

# Array with integers
numbers = [1, 2, 3, 4, 5]

# Array with strings
names = ["Alice", "Bob", "Charlie"]

# Mixed types (Ruby allows this)
mixed = [1, "hello", 3.14, true]
```

## Accessing Elements

Arrays use zero-based indexing:

```ruby
numbers = [5, 7, 1, 3, 4]

numbers[0]   # => 5 (first element)
numbers[1]   # => 7 (second element)
numbers[-1]  # => 4 (last element)
numbers[-2]  # => 3 (second to last)
```

## Adding Elements

```ruby
books = []

# Using push
books.push("The Effective Engineer")
books.push("Zero to One")

# Using << operator
books << "Lean Startup"
books << "Hooked"

puts books
# => ["The Effective Engineer", "Zero to One", "Lean Startup", "Hooked"]
```

## Common Array Methods

### Size and Length
```ruby
numbers = [1, 2, 3, 4, 5]
numbers.length  # => 5
numbers.size    # => 5 (same as length)
numbers.empty?  # => false
```

### First and Last
```ruby
numbers.first   # => 1
numbers.last    # => 5
```

### Include?
```ruby
numbers.include?(3)  # => true
numbers.include?(10) # => false
```

### Removing Elements
```ruby
numbers = [1, 2, 3, 4, 5]

numbers.pop    # => 5 (removes and returns last)
numbers.shift  # => 1 (removes and returns first)
numbers.delete(3)  # Removes element with value 3
```

## Iterating Over Arrays

```ruby
fruits = ["apple", "banana", "orange"]

# Each iterator
fruits.each do |fruit|
  puts fruit
end

# With index
fruits.each_with_index do |fruit, index|
  puts "#{index}: #{fruit}"
end
```

## Array Operations

```ruby
arr1 = [1, 2, 3]
arr2 = [3, 4, 5]

arr1 + arr2    # => [1, 2, 3, 3, 4, 5] (concatenation)
arr1 - arr2    # => [1, 2] (difference)
arr1 & arr2    # => [3] (intersection)
arr1 | arr2    # => [1, 2, 3, 4, 5] (union)
```

## Useful Methods

```ruby
numbers = [5, 2, 8, 1, 9]

numbers.sort      # => [1, 2, 5, 8, 9]
numbers.reverse   # => [9, 1, 8, 2, 5]
numbers.uniq      # Removes duplicates
numbers.max       # => 9
numbers.min       # => 1
numbers.sum       # => 25
```

## Key Takeaways

- Arrays store ordered collections: `[1, 2, 3]`
- Zero-based indexing: first element is `[0]`
- Negative indices count from end: `[-1]` is last
- Add with `.push()` or `<<` operator
- Remove with `.pop`, `.shift`, `.delete`
- Iterate with `.each` method
- Many built-in methods: `.sort`, `.reverse`, `.uniq`
