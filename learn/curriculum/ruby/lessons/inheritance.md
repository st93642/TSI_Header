# Inheritance in Ruby

Inheritance allows classes to inherit characteristics and behavior from parent classes, promoting code reuse.

## Basic Inheritance

```ruby
class Car
  attr_accessor :wheels, :capacity, :max_speed

  def initialize(wheels, capacity, max_speed)
    @wheels = wheels
    @capacity = capacity
    @max_speed = max_speed
  end
end

# ElectricCar inherits from Car
class ElectricCar < Car
end

tesla = ElectricCar.new(4, 5, 250)
puts tesla.wheels      # => 4
puts tesla.max_speed   # => 250
```

## The `<` Operator

The `<` operator indicates inheritance:

```ruby
class Parent
  def greet
    "Hello from parent"
  end
end

class Child < Parent
  # Inherits greet method
end

child = Child.new
puts child.greet  # => "Hello from parent"
```

## Overriding Methods

Child classes can override parent methods:

```ruby
class Animal
  def speak
    "Some sound"
  end
end

class Dog < Animal
  def speak
    "Woof!"
  end
end

class Cat < Animal
  def speak
    "Meow!"
  end
end

dog = Dog.new
cat = Cat.new
puts dog.speak  # => "Woof!"
puts cat.speak  # => "Meow!"
```

## Using `super`

Call the parent class's version of a method:

```ruby
class Vehicle
  def initialize(wheels)
    @wheels = wheels
  end

  def info
    "This vehicle has #{@wheels} wheels"
  end
end

class Car < Vehicle
  def initialize(wheels, brand)
    super(wheels)  # Call parent initialize
    @brand = brand
  end

  def info
    super + " and is a #{@brand}"
  end
end

car = Car.new(4, "Tesla")
puts car.info
# => "This vehicle has 4 wheels and is a Tesla"
```

## Inheritance Chain

```ruby
class GrandParent
  def method1
    "GrandParent method"
  end
end

class Parent < GrandParent
  def method2
    "Parent method"
  end
end

class Child < Parent
  def method3
    "Child method"
  end
end

child = Child.new
child.method1  # From GrandParent
child.method2  # From Parent
child.method3  # From Child
```

## Checking Class Relationships

```ruby
dog = Dog.new

dog.instance_of?(Dog)     # => true
dog.instance_of?(Animal)  # => false

dog.is_a?(Dog)            # => true
dog.is_a?(Animal)         # => true
dog.kind_of?(Animal)      # => true (same as is_a?)
```

## Key Takeaways

- Inheritance uses `<` operator: `class Child < Parent`
- Child classes inherit parent methods and attributes
- Override methods by redefining them in child class
- Use `super` to call parent class methods
- Ruby supports single inheritance only
- Check relationships with `.is_a?` and `.instance_of?`
- Inheritance promotes code reuse and organization
