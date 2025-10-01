# Defining Methods in Ruby

Methods are reusable blocks of code that perform specific tasks. They help organize your code and avoid repetition.

## Basic Method Definition

```ruby
# Simple method
def greet
  puts "Hello, World!"
end

# Call the method
greet  # => Hello, World!
```

## Methods with Parameters

```ruby
def greet(name)
  puts "Hello, #{name}!"
end

greet("Alice")  # => Hello, Alice!
greet("Bob")    # => Hello, Bob!
```

## Methods with Multiple Parameters

```ruby
def add(a, b)
  a + b
end

result = add(5, 3)
puts result  # => 8
```

## Return Values

Ruby methods automatically return the last evaluated expression:

```ruby
def multiply(a, b)
  a * b
end

result = multiply(4, 5)
puts result  # => 20

# Explicit return (optional)
def divide(a, b)
  return 0 if b == 0  # Guard clause
  a / b
end
```

## Default Parameters

```ruby
def greet(name = "Guest")
  "Hello, #{name}!"
end

greet         # => "Hello, Guest!"
greet("Alice") # => "Hello, Alice!"
```

## Methods with Multiple Return Values

```ruby
def min_max(array)
  [array.min, array.max]
end

numbers = [5, 2, 8, 1, 9]
minimum, maximum = min_max(numbers)
puts "Min: #{minimum}, Max: #{maximum}"
# => Min: 1, Max: 9
```

## Method Naming Conventions

```ruby
# Use snake_case
def calculate_average(numbers)
  numbers.sum.to_f / numbers.size
end

# Question mark for boolean methods
def even?(number)
  number % 2 == 0
end

# Exclamation mark for dangerous methods
def reverse!
  # Modifies object in place
end
```

## Variable Arguments

```ruby
def sum(*numbers)
  numbers.sum
end

sum(1, 2, 3)        # => 6
sum(1, 2, 3, 4, 5)  # => 15
```

## Keyword Arguments

```ruby
def create_user(name:, age:, city: "Unknown")
  {
    name: name,
    age: age,
    city: city
  }
end

user = create_user(name: "Alice", age: 30)
# => {:name=>"Alice", :age=>30, :city=>"Unknown"}
```

## Key Takeaways

- Define methods with `def method_name`
- Methods automatically return last expression
- Use parameters: `def greet(name)`
- Default parameters: `def greet(name = "Guest")`
- Use snake_case for method names
- `?` suffix for boolean methods
- `*args` for variable number of arguments
- Keyword arguments: `def method(key: value)`
