# E-Commerce Application Constants

Constants in Ruby are special variables that hold values that shouldn't change during program execution. They help make your code more readable and maintainable by giving meaningful names to important values.

## What are Constants?

Constants are declared using uppercase letters and are meant to hold values that remain constant throughout the program execution. Ruby doesn't prevent you from changing constants, but it will warn you when you do.

```ruby
# Good constant names
PI = 3.14159
MAX_USERS = 100
DEFAULT_TIMEOUT = 30
API_VERSION = "v2.1"

# Accessing constants
puts PI          # => 3.14159
puts MAX_USERS   # => 100
```

## Constant Naming Conventions

Ruby constants follow specific naming conventions:

### Uppercase with Underscores

```ruby
# Use ALL_CAPS with underscores for multi-word constants
DATABASE_URL = "postgresql://localhost/myapp"
MAX_FILE_SIZE = 1048576  # 1MB in bytes
DEFAULT_LANGUAGE = "en"
```

### Class and Module Names

```ruby
# Class names are also constants (start with capital letter)
class UserAccount
  # Class content
end

module PaymentProcessor
  # Module content
end
```

## Scope of Constants

Constants have different scoping rules:

### Global Constants

```ruby
# Defined at the top level
GLOBAL_CONFIG = { debug: true }

class MyClass
  def show_config
    puts GLOBAL_CONFIG[:debug]  # Accessible everywhere
  end
end
```

### Class Constants

```ruby
class Car
  WHEELS = 4
  MAX_SPEED = 200

  def self.specifications
    puts "Wheels: #{WHEELS}"
    puts "Max Speed: #{MAX_SPEED}"
  end
end

Car.specifications  # => Wheels: 4, Max Speed: 200
```

### Module Constants

```ruby
module MathConstants
  PI = 3.14159
  E = 2.71828

  def self.circle_area(radius)
    PI * radius * radius
  end
end

puts MathConstants::PI  # => 3.14159
puts MathConstants.circle_area(5)  # => 78.53975
```

## Warning When Changing Constants

Ruby allows you to change constants but issues a warning:

```ruby
PI = 3.14159
puts PI  # => 3.14159

PI = 3.14  # warning: already initialized constant PI
puts PI    # => 3.14 (but you get a warning)
```

## When to Use Constants

Use constants for:

### Configuration Values

```ruby
APP_NAME = "MyApp"
VERSION = "1.0.0"
ENVIRONMENT = "production"
```

### Mathematical Constants

```ruby
PI = 3.14159265359
EULER = 2.71828182846
SPEED_OF_LIGHT = 299792458  # meters per second
```

### Limits and Boundaries

```ruby
MAX_LOGIN_ATTEMPTS = 3
SESSION_TIMEOUT = 3600  # seconds
MAX_FILE_SIZE = 10485760  # 10MB
```

### Status Codes

```ruby
STATUS_OK = 200
STATUS_NOT_FOUND = 404
STATUS_ERROR = 500
```

## Constants vs Variables

Key differences:

| Constants | Variables |
|-----------|-----------|
| ALL_CAPS naming | lowercase_with_underscores |
| Should not change | Can change anytime |
| Global scope by default | Limited scope |
| Warn when reassigned | No warnings |

```ruby
# Variable - can change
user_count = 0
user_count = 10  # OK, no warning

# Constant - should not change
MAX_USERS = 100
MAX_USERS = 200  # Warning issued
```

## Best Practices

### 1. Use Meaningful Names

```ruby
# Good
MAX_CONNECTIONS = 100
DEFAULT_TIMEOUT = 30

# Bad
A = 100
B = 30
```

### 2. Group Related Constants

```ruby
module AppConfig
  DATABASE_HOST = "localhost"
  DATABASE_PORT = 5432
  DATABASE_NAME = "myapp"
end
```

### 3. Use Constants for Magic Numbers

```ruby
# Bad - magic number
if score > 85
  puts "Excellent!"
end

# Good - named constant
PASSING_GRADE = 85
if score > PASSING_GRADE
  puts "Excellent!"
end
```

### 4. Document Constants

```ruby
# Number of seconds in a day
SECONDS_PER_DAY = 86400

# Maximum file upload size in bytes
MAX_UPLOAD_SIZE = 10 * 1024 * 1024  # 10MB
```

## Key Takeaways

- Constants use `ALL_CAPS` naming convention
- They should hold values that don't change during execution
- Ruby warns when you reassign constants (but allows it)
- Use them for configuration, limits, and magic numbers
- Make names descriptive and well-documented
- Group related constants in modules or classes

## Practice Time

Now it's time to practice working with constants. Click the button below to start the exercise.

### Exercise Goals

1. Define constants with proper naming
2. Use constants in calculations and comparisons
3. Understand constant scope and warnings
4. Apply constants to real-world scenarios

> **Tip**: Constants make your code more maintainable. When you need to change a value, you only need to update it in one place!

## What's Next

In the next lesson, you'll learn about working with strings - one of Ruby's most powerful and flexible data types. You'll discover concatenation, interpolation, and many useful string methods.

---

**Remember**: Constants are your friends for configuration and important values. Use them liberally to make your code more readable and maintainable! 📋✨
