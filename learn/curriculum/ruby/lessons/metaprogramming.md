# Metaprogramming Basics

Metaprogramming is writing code that writes code. Ruby provides powerful metaprogramming features that allow dynamic method definition and class modification at runtime.

## Dynamic Method Definition

### define_method

```ruby
class Person
  # Dynamically define getter methods
  [:name, :age, :city].each do |attribute|
    define_method(attribute) do
      instance_variable_get("@#{attribute}")
    end

    define_method("#{attribute}=") do |value|
      instance_variable_set("@#{attribute}", value)
    end
  end
end

person = Person.new
person.name = "Alice"
puts person.name  # => Alice
```

## method_missing

Handle calls to undefined methods:

```ruby
class DynamicGreeter
  def method_missing(method_name, *args)
    if method_name.to_s.start_with?('greet_')
      language = method_name.to_s.split('_')[1]
      "Hello in #{language}!"
    else
      super
    end
  end
end

greeter = DynamicGreeter.new
puts greeter.greet_english   # => Hello in english!
puts greeter.greet_spanish   # => Hello in spanish!
```

## send Method

Call methods dynamically by name:

```ruby
class Calculator
  def add(a, b)
    a + b
  end

  def multiply(a, b)
    a * b
  end
end

calc = Calculator.new
operation = "add"

result = calc.send(operation, 5, 3)
puts result  # => 8
```

## class_eval and instance_eval

Execute code in context of a class or instance:

```ruby
# class_eval - modify class
String.class_eval do
  def shout
    self.upcase + "!"
  end
end

puts "hello".shout  # => HELLO!

# instance_eval - modify single instance
str = "hello"
str.instance_eval do
  def whisper
    self.downcase + "..."
  end
end

puts str.whisper  # => hello...
```

## attr_accessor Magic

Understanding how attr_accessor works:

```ruby
class MyClass
  # This:
  attr_accessor :name

  # Is equivalent to:
  def name
    @name
  end

  def name=(value)
    @name = value
  end
end
```

## Practical Example: DSL

Creating a simple Domain-Specific Language:

```ruby
class Task
  attr_accessor :name, :priority

  def initialize(name)
    @name = name
  end

  def high_priority
    @priority = :high
  end
end

class TodoList
  def initialize
    @tasks = []
  end

  def task(name, &block)
    task = Task.new(name)
    task.instance_eval(&block)
    @tasks << task
  end

  def show
    @tasks.each do |task|
      puts "#{task.name} [#{task.priority}]"
    end
  end
end

# Usage - looks like a DSL
list = TodoList.new

list.task "Buy groceries" do
  high_priority
end

list.show  # => Buy groceries [high]
```

## Key Takeaways

- `define_method` - create methods dynamically
- `method_missing` - handle undefined methods
- `send` - call methods by name (string/symbol)
- `class_eval` - execute code in class context
- `instance_eval` - execute code in instance context
- Metaprogramming enables DSLs and frameworks
- Use carefully - can make code hard to understand
- Great for reducing repetitive code
