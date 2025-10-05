# Metaprogramming Basics

Metaprogramming is writing code that writes code. Ruby provides powerful
metaprogramming features that allow dynamic method definition and class
modification at runtime.

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

## Practical Example: DSL

Creating a simple Domain-Specific Language:

```ruby
class Task attr_accessor :name, :priority

def initialize(name) @name = name end

def high_priority @priority = :high end end

class TodoList def initialize @tasks = [] end

def task(name, &block) task = Task.new(name) task.instance_eval(&block) @tasks
<< task end

def show @tasks.each do |task| puts "#{task.name} [#{task.priority}]" end end
end

# Usage - looks like a DSL
list = TodoList.new

list.task "Buy groceries" do high_priority end

list.show # => Buy groceries [high]
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

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Metaprogramming — Safe Patterns & Tests (Appendix — metaprogramming-ruby2)

Compact recipes for using `define_method`, `method_missing`, and reflection safely plus test strategies to verify dynamic behaviour.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Technique</th><th>Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>define_method</td><td>Generate methods dynamically</td><td>Avoid defining methods on global classes in libraries</td></tr>
    <tr><td>method_missing</td><td>Flexible dispatch</td><td>Provide respond_to_missing? for compatibility</td></tr>
    <tr><td>Reflection</td><td>Introspection</td><td>Use sparingly; prefer explicit APIs</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: define_method

```ruby
class MyBuilder %i[one two three].each do |name| define_method("do_#{name}") do
|arg| "#{name}-#{arg}" end end end
```

### Testing dynamic methods

- Use `respond_to?` checks and test that generated methods behave as expected.

```ruby
require 'minitest/autorun'

class TestMeta < Minitest::Test def test_dynamic b = MyBuilder.new
assert_respond_to b, :do_one assert_equal 'one-42', b.do_one(42) end end
```

### Exercises (Appendix — metaprogramming-ruby2)

1. Implement a small DSL that defines attribute helpers at runtime and write tests verifying behavior.
2. Replace `method_missing` logic with explicit generated methods to improve performance and testability.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Metaprogramming Patterns & Safety (Appendix — metaprogramming-ruby-appendix-20251005)

Guidance on using `define_method`, `method_missing`, and avoiding maintainability pitfalls.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>Use</th><th>Safety</th></tr>
  </thead>
  <tbody>
    <tr><td>define_method</td><td>Generate methods</td><td>Prefer explicit definitions when possible</td></tr>
    <tr><td>method_missing</td><td>Dynamic dispatch</td><td>Implement `respond_to_missing?`</td></tr>
    <tr><td>send</td><td>Call private methods</td><td>Use sparingly; document intent</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: safe dynamic method

```ruby
class AttrList def initialize(names) names.each { |n| define_singleton_method(n)
{ instance_variable_get("@#{n}") } } end end
```

### Exercises (Appendix — metaprogramming-ruby-appendix-20251005)

1. Implement `method_missing` for a delegator and add tests for expected methods and `respond_to?`.
2. Document when metaprogramming improves vs harms clarity in a short note.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Metaprogramming — Safety & Introspection (Appendix — metaprogramming-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Danger</th><th>Safe alternative</th><th>Note</th></tr>
  </thead>
  <tbody>
    <tr><td>Unscoped class_eval</td><td>Use Module.new + refine or prepend</td><td>Avoid changing core classes globally</td></tr>
    <tr><td>Dynamic method names</td><td>Validate inputs + explicit interface</td><td>Raise early if name not allowed</td></tr>
    <tr><td>Method_missing abuse</td><td>Prefer defined? or explicit delegation</td><td>Provides clearer errors and tooling support</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Patterns

```ruby
# defensive dynamic method
ALLOWED = %i[foo bar baz] ALLOWED.each { |name| define_method(name) { |*a|
handle(name, *a) } }
```

```ruby
# prefer delegation over method_missing
delegate :perform, to: :worker
```

### Exercises

1. Replace a `method_missing` in the codebase (if any) with explicit delegation.
2. Write a small `safe_define` helper that only defines whitelisted methods and tests its behavior.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Metaprogramming — Safety Checklist & Alternatives (Appendix — metaprogramming-appendix-3)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Check</th><th>Alternative</th></tr>
  </thead>
  <tbody>
    <tr><td>Readability</td><td>Is generated code documented?</td><td>Prefer explicit methods when clarity matters</td></tr>
    <tr><td>Security</td><td>Does input influence `eval`/`class_eval`?</td><td>Use `define_method` with blocks, avoid string `eval`</td></tr>
    <tr><td>Testability</td><td>Can you test generated methods individually?</td><td>Generate methods at build-time or provide explicit wrappers</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Appendix — Examples

```ruby
# Prefer define_method over class_eval string
self.class.define_method(:greet) do |name| "Hello, #{name}" end

# Document generated methods for tooling
# :nodoc: generated by Settings.define_boolean
```

### Appendix — Exercises

1. Find a `class_eval` usage and rewrite to `define_method` with a block; run tests to ensure parity.
2. Add a short YARD comment to a dynamically created method so it appears in generated docs.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Metaprogramming — Hidden Tips & Safe Patterns (Appendix — metaprogramming-hidden-20251005)

A few short, practical tips drawn from community best-practices and the Ruby Style Guide.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tip</th><th>Why</th><th>Quick fix</th></tr>
  </thead>
  <tbody>
    <tr><td>Prefer define_method with blocks</td><td>Avoid string eval and keep scope</td><td>use `define_method(:name) { |*a| ... }`</td></tr>
    <tr><td>Implement respond_to_missing?</td><td>Tooling & introspection expect it</td><td>return true for handled methods</td></tr>
    <tr><td>Avoid global class_eval</td><td>Prevents surprising global changes</td><td>define on a module/class or use prepend</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Tiny example — safe dynamic methods

```ruby
# define a whitelisted dynamic API safely
class SafeBuilder ALLOWED = %i[one two]

ALLOWED.each do |n| define_method("do_#{n}") { |arg| "#{n}-#{arg}" } end

def respond_to_missing?(name, include_private = false)
name.to_s.start_with?("do_") || super end end
```

### Exercises

1. Add a test that ensures only methods in ALLOWED are defined and others raise NoMethodError.
2. Replace a small string-eval `class_eval` usage in the repo (if present) with `define_method` and verify behaviour.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
