# Numbers and Math in Ruby

Ruby provides powerful support for numeric operations with integers, floating-point numbers, and mathematical functions.

## Numeric Types

### Integers
Whole numbers without decimal points:

```ruby
age = 25
big_number = 1_000_000  # Underscores for readability
```

### Floats
Numbers with decimal points:

```ruby
price = 19.99
pi = 3.14159
```

## Basic Arithmetic

```ruby
10 + 5   # => 15 (addition)
10 - 5   # => 5  (subtraction)
10 * 5   # => 50 (multiplication)
10 / 5   # => 2  (division)
10 % 3   # => 1  (modulo/remainder)
2 ** 3   # => 8  (exponentiation)
```

## Division Gotcha

Integer division truncates decimals:

```ruby
10 / 3   # => 3 (integer division)
10.0 / 3 # => 3.333... (float division)
10.to_f / 3  # => 3.333... (convert to float first)
```

## Type Conversion

```ruby
42.to_f        # => 42.0 (integer to float)
3.99.to_i      # => 3 (float to integer)
"42".to_i      # => 42 (string to integer)
"3.14".to_f    # => 3.14 (string to float)
```

## Numeric Methods

```ruby
-5.abs         # => 5 (absolute value)
3.7.round      # => 4 (round to nearest)
4.even?        # => true
5.odd?         # => true
```

## Math Module

```ruby
Math::PI       # => 3.14159...
Math.sqrt(16)  # => 4.0
Math.sqrt(2)   # => 1.414...
```

## Key Takeaways

- Ruby has Integer and Float numeric types
- Integer division truncates (use `.to_f` for decimals)
- Numbers are objects with methods like `.even?`, `.abs`
- Math module provides advanced functions
- Use underscores in large numbers: `1_000_000`
