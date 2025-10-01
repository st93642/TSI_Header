# Lambdas in Ruby

Lambdas are anonymous functions that can be stored in variables and passed around. They are similar to Procs but with stricter argument checking.

## Creating Lambdas

```ruby
# Lambda syntax
greet = lambda { |name| puts "Hello, #{name}!" }

# Shorthand syntax (stabby lambda)
greet = ->(name) { puts "Hello, #{name}!" }

# Call the lambda
greet.call("Alice")  # => Hello, Alice!
```

## Lambdas vs Procs

### Argument Checking

```ruby
# Lambda - strict argument count
my_lambda = ->(x, y) { x + y }
my_lambda.call(1, 2)     # => 3
my_lambda.call(1)        # ArgumentError

# Proc - flexible argument count
my_proc = Proc.new { |x, y| x.to_i + y.to_i }
my_proc.call(1, 2)       # => 3
my_proc.call(1)          # => 1 (y is nil)
```

### Return Behavior

```ruby
# Lambda - returns from lambda only
def lambda_test
  l = lambda { return "from lambda" }
  l.call
  "from method"
end

lambda_test  # => "from method"

# Proc - returns from enclosing method
def proc_test
  p = Proc.new { return "from proc" }
  p.call
  "from method"
end

proc_test  # => "from proc"
```

## Lambda with Multiple Lines

```ruby
calculate = lambda do |x, y|
  sum = x + y
  product = x * y
  [sum, product]
end

result = calculate.call(5, 3)
puts result  # => [8, 15]
```

## Passing Lambdas to Methods

```ruby
def execute_operation(a, b, operation)
  operation.call(a, b)
end

add = ->(x, y) { x + y }
multiply = ->(x, y) { x * y }

puts execute_operation(5, 3, add)       # => 8
puts execute_operation(5, 3, multiply)  # => 15
```

## Lambdas with Arrays

```ruby
numbers = [1, 2, 3, 4, 5]

# Double each number
double = ->(n) { n * 2 }
doubled = numbers.map(&double)
puts doubled  # => [2, 4, 6, 8, 10]

# Filter even numbers
is_even = ->(n) { n.even? }
evens = numbers.select(&is_even)
puts evens  # => [2, 4]
```

## Closures

Lambdas can access variables from their surrounding scope:

```ruby
def create_multiplier(factor)
  lambda { |n| n * factor }
end

times_two = create_multiplier(2)
times_three = create_multiplier(3)

puts times_two.call(5)    # => 10
puts times_three.call(5)  # => 15
```

## Key Takeaways

- Lambdas are anonymous functions: `lambda { code }`
- Shorthand syntax: `->() { code }`
- Call with `.call()` method
- Strict argument checking (unlike Procs)
- Return only exits the lambda
- Can be stored in variables
- Useful for callbacks and functional programming
- Can capture variables from surrounding scope
