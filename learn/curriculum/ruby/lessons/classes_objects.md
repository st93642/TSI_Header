# Classes and Objects

Ruby is an object-oriented language. Everything in Ruby is an object, and classes are blueprints for creating objects.

## Defining a Class

```ruby
class Vehicle
  # Class code goes here
end

# Create an instance (object)
car = Vehicle.new
```

## Initialize Method (Constructor)

```ruby
class Vehicle
  def initialize(wheels, tank_type, capacity, max_speed)
    @wheels = wheels
    @tank_type = tank_type
    @capacity = capacity
    @max_speed = max_speed
  end
end

tesla = Vehicle.new(4, 'electric', 5, 250)
```

## Instance Variables

Instance variables (prefixed with `@`) store object state:

```ruby
class Person
  def initialize(name, age)
    @name = name  # Instance variable
    @age = age    # Instance variable
  end
end
```

## Instance Methods

```ruby
class Vehicle
  def initialize(max_speed)
    @max_speed = max_speed
  end

  def make_noise
    "VRRRRUUUUM"
  end

  def accelerate
    puts "Accelerating to #{@max_speed} km/h!"
  end
end

car = Vehicle.new(180)
car.make_noise     # => "VRRRRUUUUM"
car.accelerate     # => Accelerating to 180 km/h!
```

## Getter and Setter Methods

```ruby
class Person
  def initialize(name)
    @name = name
  end

  # Getter method
  def name
    @name
  end

  # Setter method
  def name=(new_name)
    @name = new_name
  end
end

person = Person.new("Alice")
puts person.name      # => Alice
person.name = "Bob"
puts person.name      # => Bob
```

## attr_accessor, attr_reader, attr_writer

Ruby provides shortcuts for getter/setter methods:

```ruby
class Person
  attr_reader :name    # Only getter
  attr_writer :email   # Only setter
  attr_accessor :age   # Both getter and setter

  def initialize(name, age, email)
    @name = name
    @age = age
    @email = email
  end
end

person = Person.new("Alice", 30, "alice@example.com")
person.name           # Works (reader)
# person.name = "Bob" # Error (no writer)

person.age            # Works (accessor)
person.age = 31       # Works (accessor)
```

## Class Methods

```ruby
class MathHelper
  def self.square(number)
    number * number
  end
end

result = MathHelper.square(5)
puts result  # => 25
```

## to_s Method

Customize how objects are printed:

```ruby
class Person
  attr_accessor :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end

  def to_s
    "#{@name}, #{@age} years old"
  end
end

person = Person.new("Alice", 30)
puts person  # => Alice, 30 years old
```

## Key Takeaways

- Classes are blueprints: `class ClassName`
- Create objects with `.new`
- `initialize` method is the constructor
- Instance variables start with `@`
- Instance methods define object behavior
- `attr_reader` - getter only
- `attr_writer` - setter only
- `attr_accessor` - both getter and setter
- Class methods use `self.method_name`
