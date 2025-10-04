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

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Metaprogramming Patterns & Cautions (Appendix)

Short recipes for safe metaprogramming and a table summarizing trade-offs.

```ruby
# Define a simple attribute with history
class AttrHistory
  def self.attr_history(name)
    define_method("#{name}_history") { (instance_variable_get("@#{name}_history") || []) }
    define_method(name) do
      instance_variable_get("@#{name}")
    end
    define_method("#{name}=") do |v|
      (@#{name}_history ||= []) << v
      instance_variable_set("@#{name}", v)
    end
  end
end
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Technique</th><th>Pros</th><th>Cons</th></tr>
  </thead>
  <tbody>
    <tr><td>define_method</td><td>DRY APIs</td><td>Harder to trace</td></tr>
    <tr><td>class_eval</td><td>Dynamic</td><td>Security & readability risks</td></tr>
    <tr><td>method_missing</td><td>DSLs</td><td>Opaqueness</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix)

1. Implement an `attr_history` macro and write tests.
2. Audit a `class_eval` use and refactor into safer, explicit methods.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Metaprogramming Best Practices (Appendix II)

Short checklist for when metaprogramming is justified and a table summarizing safe patterns.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>When</th><th>Why</th><th>Alternative</th></tr>
  </thead>
  <tbody>
    <tr><td>DSL required</td><td>Concise API</td><td>Explicit builder pattern</td></tr>
    <tr><td>Reduce duplication</td><td>Less boilerplate</td><td>Generate code at build time</td></tr>
    <tr><td>Dynamic adapt</td><td>Pluggable extensions</td><td>Explicit extension points</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix II)

1. Review an existing `class_eval` usage and rewrite as explicit methods.
2. Add documentation comments to dynamically defined methods via `define_method`.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Metaprogramming — Resources (Appendix — External Links)

Reading and cautionary notes for runtime code generation and reflection.

- Ruby Object model & reflection: [Object and Module docs](https://ruby-doc.org/core/Object.html)
- Best practices: prefer explicit code over complex metaprogramming when possible

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Aspect</th><th>Doc</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>define_method</td><td><a href="https://ruby-doc.org/core/Module.html#method-i-define_method">define_method</a></td><td>Useful for DRYing repetitive methods</td></tr>
    <tr><td>Method introspection</td><td><a href="https://ruby-doc.org/core/Object.html">Object#methods</a></td><td>Avoid surprising public APIs</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Replace a repetitive accessor definition with a `define_method` implementation and add tests that assert behavior.
2. Add a short note documenting why excessive metaprogramming can harm maintainability.

<!-- markdownlint-enable MD010 -->
