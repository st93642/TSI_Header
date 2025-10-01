# Lesson 8.3: Class vs Instance Methods

## Overview

In Ruby, methods can be defined at two levels: the **class level** and the **instance level**. Understanding the difference between these two types of methods is crucial for effective object-oriented programming.

- **Instance methods** are called on instances of a class (objects)
- **Class methods** are called directly on the class itself

## Instance Methods

Instance methods are the most common type of method. They operate on individual objects and can access instance variables.

```ruby
class Car
  def initialize(make, model)
    @make = make
    @model = model
    @mileage = 0
  end

  # Instance method - called on car objects
  def drive(distance)
    @mileage += distance
    puts "#{@make} #{@model} drove #{distance} miles"
  end

  # Instance method - called on car objects
  def info
    "#{@make} #{@model} has #{@mileage} miles"
  end
end

car1 = Car.new("Toyota", "Camry")
car2 = Car.new("Honda", "Civic")

car1.drive(50)  # => "Toyota Camry drove 50 miles"
car2.drive(30)  # => "Honda Civic drove 30 miles"

puts car1.info  # => "Toyota Camry has 50 miles"
puts car2.info  # => "Honda Civic has 30 miles"
```

## Class Methods

Class methods are defined with `self.` prefix or the class name. They cannot access instance variables but can access class variables.

```ruby
class Car
  @@total_cars = 0

  def initialize(make, model)
    @make = make
    @model = model
    @@total_cars += 1
  end

  # Class method - called on the Car class
  def self.total_cars
    @@total_cars
  end

  # Class method - alternative syntax
  class << self
    def most_popular_make
      "Toyota"  # In a real app, this would be calculated
    end
  end
end

car1 = Car.new("Toyota", "Camry")
car2 = Car.new("Honda", "Civic")
car3 = Car.new("Ford", "Focus")

puts Car.total_cars        # => 3
puts Car.most_popular_make # => "Toyota"
```

## When to Use Class Methods

### Factory Methods

Class methods are often used to create instances with special initialization:

```ruby
class User
  attr_reader :name, :email, :role

  def initialize(name, email, role = :user)
    @name = name
    @email = email
    @role = role
  end

  # Class method factory
  def self.admin(name, email)
    new(name, email, :admin)
  end

  # Another class method factory
  def self.guest
    new("Guest", "guest@example.com", :guest)
  end
end

admin = User.admin("Alice", "alice@company.com")
guest = User.guest

puts admin.role  # => :admin
puts guest.role  # => :guest
```

### Utility Methods

Class methods can provide utility functions that don't need an instance:

```ruby
class MathUtils
  # Class method for utility
  def self.fahrenheit_to_celsius(fahrenheit)
    (fahrenheit - 32) * 5.0 / 9.0
  end

  # Class method for constants
  def self.pi
    3.14159
  end
end

puts MathUtils.fahrenheit_to_celsius(98.6)  # => 37.0
puts MathUtils.pi                           # => 3.14159
```

### Singleton Patterns

Class methods can implement singleton behavior:

```ruby
class Database
  @@instance = nil

  def self.instance
    @@instance ||= new
  end

  def initialize
    @connection = "Connected to database"
  end

  def query(sql)
    puts "Executing: #{sql}"
  end
end

# Always returns the same instance
db1 = Database.instance
db2 = Database.instance

puts db1 == db2  # => true

db1.query("SELECT * FROM users")
```

## Class Variables vs Instance Variables

Understanding the difference between class and instance variables is key:

```ruby
class Counter
  @@class_count = 0

  def initialize
    @instance_count = 0
  end

  # Class method accessing class variable
  def self.total_count
    @@class_count
  end

  # Instance method accessing instance variable
  def increment
    @instance_count += 1
    @@class_count += 1
  end

  def instance_count
    @instance_count
  end
end

counter1 = Counter.new
counter2 = Counter.new

counter1.increment
counter1.increment
counter2.increment

puts counter1.instance_count  # => 2
puts counter2.instance_count  # => 1
puts Counter.total_count      # => 3
```

## Inheritance and Class Methods

Class methods are inherited by subclasses:

```ruby
class Vehicle
  def self.vehicle_type
    "Generic Vehicle"
  end

  def drive
    "Driving a vehicle"
  end
end

class Car < Vehicle
  def self.vehicle_type
    "Car"
  end

  def drive
    "Driving a car"
  end
end

class Truck < Vehicle
  def self.vehicle_type
    "Truck"
  end
end

puts Vehicle.vehicle_type  # => "Generic Vehicle"
puts Car.vehicle_type      # => "Car"
puts Truck.vehicle_type    # => "Truck"

car = Car.new
puts car.drive             # => "Driving a car"

truck = Truck.new
puts truck.drive           # => "Driving a vehicle" (inherited)
```

## Method Visibility

Both class and instance methods can have visibility modifiers:

```ruby
class Example
  # Public class method
  def self.public_method
    "This is public"
  end

  # Private class method
  def self.private_method
    "This is private"
  end
  private_class_method :private_method

  # Public instance method
  def public_instance
    "Public instance"
  end

  # Private instance method
  private

  def private_instance
    "Private instance"
  end
end

puts Example.public_method    # => "This is public"
# puts Example.private_method  # => NoMethodError

obj = Example.new
puts obj.public_instance      # => "Public instance"
# puts obj.private_instance    # => NoMethodError
```

## Common Patterns

### Class Method for Validation

```ruby
class Email
  def self.valid?(email)
    email.match?(/\A[^@\s]+@[^@\s]+\.[^@\s]+\z/)
  end

  def self.domain(email)
    email.split('@').last
  end
end

puts Email.valid?("user@example.com")  # => true
puts Email.domain("user@example.com")  # => "example.com"
```

### Instance Method with Class Method Helper

```ruby
class Password
  MIN_LENGTH = 8

  def self.valid_length?(password)
    password.length >= MIN_LENGTH
  end

  def initialize(password)
    if self.class.valid_length?(password)
      @password = password
    else
      raise ArgumentError, "Password too short"
    end
  end
end

# Using class method directly
puts Password.valid_length?("short")    # => false
puts Password.valid_length?("longpassword") # => true

# Using in instance creation
password = Password.new("mypassword")
```

## Best Practices

1. **Use class methods for operations that don't need instance state**
2. **Use instance methods for operations on specific objects**
3. **Class methods cannot access instance variables**
4. **Instance methods can access class variables (but usually shouldn't)**
5. **Class methods are inherited by subclasses**
6. **Consider using modules for utility methods instead of class methods**

Class and instance methods serve different purposes in Ruby's object-oriented design. Mastering both allows you to write more flexible and maintainable code.
