# Attribute Methods

## Overview

In Ruby, instance variables (prefixed with `@`) are private to the object and cannot be accessed directly from outside the class. To expose instance variables to the outside world, we need to create accessor methods. Ruby provides three built-in methods to automatically create these accessors: `attr_reader`, `attr_writer`, and `attr_accessor`.

## attr_reader

`attr_reader` creates a "getter" method that allows you to read the value of an instance variable from outside the class.

```ruby
class Person
  attr_reader :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end
end

person = Person.new("Alice", 30)
puts person.name  # => "Alice"
puts person.age   # => 30
```

This is equivalent to manually defining:

```ruby
class Person
  def initialize(name, age)
    @name = name
    @age = age
  end

  def name
    @name
  end

  def age
    @age
  end
end
```

## attr_writer

`attr_writer` creates a "setter" method that allows you to modify the value of an instance variable from outside the class.

```ruby
class Person
  attr_reader :name, :age
  attr_writer :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end
end

person = Person.new("Alice", 30)
person.name = "Bob"  # Changes @name to "Bob"
person.age = 35      # Changes @age to 35

puts person.name     # => "Bob"
puts person.age      # => 35
```

This is equivalent to manually defining:

```ruby
class Person
  def name=(new_name)
    @name = new_name
  end

  def age=(new_age)
    @age = new_age
  end
end
```

## attr_accessor

`attr_accessor` is a combination of `attr_reader` and `attr_writer`. It creates both getter and setter methods for the specified instance variables.

```ruby
class Person
  attr_accessor :name, :age, :email

  def initialize(name, age, email)
    @name = name
    @age = age
    @email = email
  end
end

person = Person.new("Alice", 30, "alice@example.com")

# Reading values
puts person.name   # => "Alice"
puts person.age    # => 30
puts person.email  # => "alice@example.com"

# Modifying values
person.name = "Bob"
person.age = 35
person.email = "bob@example.com"

puts person.name   # => "Bob"
```

This is equivalent to:

```ruby
class Person
  attr_reader :name, :age, :email
  attr_writer :name, :age, :email
end
```

## Multiple Attributes

You can define accessors for multiple attributes in a single call:

```ruby
class Product
  attr_reader :name, :price, :category, :in_stock
  attr_writer :price, :in_stock
  attr_accessor :description

  def initialize(name, price, category)
    @name = name
    @price = price
    @category = category
    @in_stock = true
    @description = ""
  end
end

product = Product.new("Laptop", 999.99, "Electronics")

# Can read all attributes
puts product.name      # => "Laptop"
puts product.price     # => 999.99
puts product.in_stock  # => true

# Can modify price and in_stock (writer methods)
product.price = 899.99
product.in_stock = false

# Can read and modify description (accessor)
product.description = "High-performance laptop"
puts product.description  # => "High-performance laptop"

# Cannot modify name or category (no writer methods)
# product.name = "Desktop"  # => NoMethodError
```

## When to Use Each Type

### Use attr_reader when

- You want to expose a value but prevent external modification
- The value should only be set during initialization or internally by the class
- Examples: ID numbers, creation timestamps, calculated values

```ruby
class Order
  attr_reader :id, :created_at, :total

  def initialize(items)
    @id = generate_unique_id
    @created_at = Time.now
    @items = items
    @total = calculate_total
  end

  private

  def calculate_total
    @items.sum { |item| item[:price] * item[:quantity] }
  end
end
```

### Use attr_writer when

- You want to allow modification but not direct reading
- The value needs validation or processing when set
- Examples: Passwords, internal state that shouldn't be exposed

```ruby
class User
  attr_reader :username, :email
  attr_writer :password

  def initialize(username, email)
    @username = username
    @email = email
    @password_hash = nil
  end

  def password=(new_password)
    @password_hash = hash_password(new_password)
  end

  private

  def hash_password(password)
    # In a real app, use proper password hashing like bcrypt
    password.reverse  # Simplified example
  end
end
```

### Use attr_accessor when

- You need both read and write access
- The attribute is a simple value that can be set directly
- Examples: User profile information, configuration settings

```ruby
class Configuration
  attr_accessor :theme, :language, :notifications_enabled

  def initialize
    @theme = "light"
    @language = "en"
    @notifications_enabled = true
  end
end
```

## Advanced Usage

### Custom Accessors

Sometimes you need custom logic in your accessors. In these cases, define the methods manually:

```ruby
class Temperature
  def initialize(celsius)
    @celsius = celsius
  end

  def celsius
    @celsius
  end

  def celsius=(value)
    @celsius = value.clamp(-273.15, 1000)  # Prevent invalid temperatures
  end

  def fahrenheit
    @celsius * 9.0 / 5.0 + 32.0
  end

  def fahrenheit=(value)
    @celsius = (value - 32.0) * 5.0 / 9.0
  end
end

temp = Temperature.new(20)
puts temp.celsius     # => 20
puts temp.fahrenheit  # => 68.0

temp.fahrenheit = 86
puts temp.celsius     # => 30.0
```

### Boolean Attributes

For boolean attributes, it's common to add a `?` to the reader method name:

```ruby
class Product
  attr_reader :name, :price
  attr_writer :price

  def initialize(name, price)
    @name = name
    @price = price
    @discontinued = false
  end

  def discontinued?
    @discontinued
  end

  def discontinued=(value)
    @discontinued = !!value
  end
end

product = Product.new("Old Product", 29.99)
puts product.discontinued?  # => false
product.discontinued = true
puts product.discontinued?  # => true
```

### Class Variables and Accessors

Attribute accessors work with instance variables, not class variables. For class-level attributes, you need different approaches:

```ruby
class Database
  @connection = nil

  class << self
    attr_accessor :connection
  end

  def self.connect(url)
    self.connection = url
  end
end

Database.connect("postgresql://localhost/mydb")
puts Database.connection  # => "postgresql://localhost/mydb"
```

## Best Practices

1. **Use the most restrictive accessor that meets your needs**: Prefer `attr_reader` over `attr_accessor` when you don't need write access.

2. **Validate input in setters**: When using `attr_writer` or `attr_accessor`, consider adding validation.

3. **Use meaningful names**: Attribute names should clearly describe what they represent.

4. **Consider thread safety**: In multi-threaded applications, be careful about concurrent access to attributes.

5. **Document your attributes**: Use comments or documentation to explain the purpose and valid values of attributes.

Here's an example of input validation:

```ruby
class Person
  attr_reader :age

  def age=(new_age)
    if new_age >= 0 && new_age <= 150
      @age = new_age
    else
      raise ArgumentError, "Age must be between 0 and 150"
    end
  end
end
```

Attribute methods are a fundamental part of Ruby's object-oriented programming, providing a clean and consistent way to expose object state while maintaining encapsulation principles.
