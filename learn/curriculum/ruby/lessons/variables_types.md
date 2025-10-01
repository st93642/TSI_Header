# Variables and Data Types

Now that you can display output, let's learn how to store and work with data using variables!

## What are Variables?

Variables are like labeled containers that hold information. You can store data in a variable and use it throughout your program.

Think of a variable as a box with a label on it - you can put something inside and refer to it by its label.

## Creating Variables

In Ruby, creating a variable is super simple - just give it a name and assign a value:

```ruby
name = "Alice"
age = 25
height = 1.75
```

No need to declare types or use special keywords - Ruby figures it out!

## Variable Naming Rules

Ruby has some rules for variable names:

- Start with a lowercase letter or underscore
- Use lowercase letters, numbers, and underscores
- Use snake_case for multi-word names
- Make names descriptive and meaningful

```ruby
# Good variable names
first_name = "Bob"
user_age = 30
total_score = 95

# Bad variable names (but valid)
x = "Bob"
n = 30
ts = 95
```

> **Best Practice**: Use descriptive names that explain what the variable contains!

## Data Types

Ruby has several built-in data types. Let's explore the most common ones:

### Strings

Text enclosed in quotes (single or double):

```ruby
name = "Alice"
greeting = 'Hello'
message = "Welcome to Ruby!"
```

### Integers

Whole numbers without decimals:

```ruby
age = 25
year = 2025
temperature = -5
big_number = 1_000_000  # Underscores make numbers readable
```

### Floats

Numbers with decimal points:

```ruby
height = 1.75
price = 19.99
pi = 3.14159
```

### Booleans

True or false values:

```ruby
is_student = true
has_graduated = false
is_raining = true
```

### Nil

Represents "nothing" or "no value":

```ruby
middle_name = nil
unknown_value = nil
```

## Checking Data Types

Use the `.class` method to see what type a variable is:

```ruby
name = "Alice"
puts name.class  # String

age = 25
puts age.class  # Integer

price = 19.99
puts price.class  # Float

is_active = true
puts is_active.class  # TrueClass
```

## Type Conversion

You can convert between different types:

```ruby
# String to Integer
age_string = "25"
age_number = age_string.to_i  # 25

# Integer to String
score = 100
score_text = score.to_s  # "100"

# String to Float
price_string = "19.99"
price_number = price_string.to_f  # 19.99

# Integer to Float
whole = 5
decimal = whole.to_f  # 5.0
```

## Variable Reassignment

You can change what's stored in a variable:

```ruby
score = 10
puts score  # 10

score = 20
puts score  # 20

score = score + 5
puts score  # 25
```

## Multiple Assignment

Ruby lets you assign multiple variables at once:

```ruby
# Assign same value to multiple variables
x = y = z = 0

# Assign different values
name, age, city = "Alice", 25, "Riga"
puts name  # Alice
puts age   # 25
puts city  # Riga
```

## Constants

Variables in ALL_CAPS are treated as constants (shouldn't change):

```ruby
PI = 3.14159
MAX_USERS = 100
COMPANY_NAME = "TSI"
```

Ruby will warn you if you try to change a constant (but won't stop you).

## String Interpolation

Use double quotes and `#{}` to insert variables into strings:

```ruby
name = "Alice"
age = 25

# With interpolation (double quotes only!)
message = "My name is #{name} and I am #{age} years old"
puts message  # My name is Alice and I am 25 years old

# You can also do calculations inside
price = 10
puts "Total: #{price * 2} dollars"  # Total: 20 dollars
```

Single quotes don't support interpolation:

```ruby
name = "Alice"
puts 'Hello, #{name}'  # Prints: Hello, #{name}
puts "Hello, #{name}"  # Prints: Hello, Alice
```

## Key Takeaways

- Variables store data for later use
- Use snake_case for variable names
- Ruby has multiple data types: String, Integer, Float, Boolean, Nil
- Use `.class` to check a variable's type
- Use `.to_i`, `.to_s`, `.to_f` for type conversion
- Use `#{}` for string interpolation (double quotes only)
- Constants use ALL_CAPS

## Practice Time

Ready to practice working with variables and data types? Click below to start the exercise!

### Exercise Goals

1. Create variables of different types
2. Perform type conversions
3. Use string interpolation
4. Work with multiple assignment
5. Practice descriptive naming

> **Tip**: String interpolation only works with double quotes, not single quotes!

## What's Next?

In the next lesson, you'll dive deeper into strings and learn powerful methods for manipulating text. Get ready to become a string master! 🎸

---

**Remember**: Good variable names make your code easier to read and understand. Take time to choose meaningful names!
