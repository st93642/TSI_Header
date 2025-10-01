# Mixins and Modules

Modules are Ruby's way of grouping methods, classes, and constants. They enable code reuse across multiple classes through mixins.

## What is a Module?

A module is a collection of methods and constants:

```ruby
module Skill
  def average_speed
    puts "My average speed is 20mph"
  end
end
```

## Including Modules (Mixins)

Use `include` to add module methods to a class:

```ruby
module Skill
  def average_speed
    "20 mph"
  end

  def max_speed
    "25 mph"
  end
end

class Runner
  include Skill

  def initialize(name)
    @name = name
  end
end

runner = Runner.new("Alice")
puts runner.average_speed  # => "20 mph"
puts runner.max_speed      # => "25 mph"
```

## Multiple Mixins

A class can include multiple modules:

```ruby
module Swimming
  def swim
    "Swimming..."
  end
end

module Running
  def run
    "Running..."
  end
end

class Triathlete
  include Swimming
  include Running

  def compete
    "Competing in triathlon"
  end
end

athlete = Triathlete.new
puts athlete.swim       # => "Swimming..."
puts athlete.run        # => "Running..."
puts athlete.compete    # => "Competing in triathlon"
```

## Namespacing with Modules

Modules can group related classes:

```ruby
module Transportation
  class Car
    def drive
      "Driving car"
    end
  end

  class Bike
    def ride
      "Riding bike"
    end
  end
end

# Access with ::
car = Transportation::Car.new
bike = Transportation::Bike.new

puts car.drive   # => "Driving car"
puts bike.ride   # => "Riding bike"
```

## Module Constants

```ruby
module Math
  PI = 3.14159
  E = 2.71828

  def self.circle_area(radius)
    PI * radius ** 2
  end
end

puts Math::PI  # => 3.14159
puts Math.circle_area(5)  # => 78.53975
```

## Differences: Module vs Class

| Module | Class |
|--------|-------|
| Cannot be instantiated | Can create objects with `.new` |
| Can be mixed into classes | Can inherit from one parent |
| Multiple modules per class | Single inheritance only |
| Used for code reuse | Used for object creation |

## Extend vs Include

```ruby
module Greetings
  def hello
    "Hello!"
  end
end

# include - adds instance methods
class Person
  include Greetings
end

person = Person.new
person.hello  # => "Hello!"

# extend - adds class methods
class Company
  extend Greetings
end

Company.hello  # => "Hello!"
```

## Real-World Example

```ruby
module Timestampable
  def created_at
    @created_at ||= Time.now
  end

  def updated_at
    @updated_at ||= Time.now
  end
end

class BlogPost
  include Timestampable
  attr_accessor :title, :content
end

class Comment
  include Timestampable
  attr_accessor :text, :author
end

post = BlogPost.new
puts post.created_at  # => 2025-10-01 ...
```

## The Math Module

Ruby's built-in Math module:

```ruby
Math::PI              # => 3.141592653589793
Math::E               # => 2.718281828459045

Math.sqrt(16)         # => 4.0
Math.sin(0)           # => 0.0
Math.cos(0)           # => 1.0
Math.log(10)          # => 2.302585092994046
```

## Key Takeaways

- Modules group related methods/constants
- Cannot create instances: no `.new`
- Use `include` to add instance methods
- Use `extend` to add class methods
- Classes can include multiple modules
- Modules enable code reuse (mixins)
- Use `::` to access module contents
- Modules provide namespacing
