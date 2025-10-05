# Inheritance in Ruby

Inheritance lets a class reuse and refine the behavior of another class. Ruby keeps inheritance single (one direct superclass), but mixes in modules to share behavior horizontally. Understanding the inheritance chain, method lookup, and `super` semantics helps you structure object hierarchies without surprises.

## Learning goals

- Define subclasses using the `<` operator and trace ancestry with `ancestors`.
- Override methods responsibly, call `super` with/without arguments, and understand default argument forwarding.
- Explore Ruby’s method lookup order, including modules and `prepend`.
- Use hooks (`inherited`, `superclass`, `method_added`) to customize class hierarchies.
- Compare inheritance with composition and know when to favor each.

## Creating subclasses

`class Child < Parent` establishes inheritance. The child automatically receives all instance methods defined on the parent (and its ancestors).

```ruby
class Vehicle
  attr_reader :wheels

  def initialize(wheels)
    @wheels = wheels
  end

  def move
    "Moving on #{wheels} wheels"
  end
end

class ElectricCar < Vehicle
end

car = ElectricCar.new(4)
car.move # => "Moving on 4 wheels"
```

Use `SuperClass` to check the direct parent (`ElectricCar.superclass # => Vehicle`).

## Method overriding and polymorphism

Override by redefining a method in the child class. Ruby uses dynamic dispatch: the method invoked depends on the object’s actual class at runtime.

```ruby
class Vehicle
  def move
    "Vehicle in motion"
  end
end

class Bike < Vehicle
  def move
    "Pedaling"
  end
end

Vehicle.new.move # => "Vehicle in motion"
Bike.new.move    # => "Pedaling"
```

Polymorphism lets clients treat `Bike` and `Vehicle` interchangeably while receiving specialized behavior.

## Calling `super`

`super` invokes the next method up the lookup chain without hardcoding the parent class, so it keeps working even if the hierarchy changes.

### Without arguments

Calling `super` without parentheses forwards the original arguments automatically.

```ruby
class Vehicle
  def initialize(wheels, color: "silver")
    @wheels = wheels
    @color = color
  end

  def move
    "Moving on #{wheels} wheels"
  end
end

class SportsCar < Vehicle
  def initialize(wheels, turbo: false, **kwargs)
    super(wheels, **kwargs)
    @turbo = turbo
  end
end

SportsCar.new(4, turbo: true, color: "red")
```

Passing keyword arguments? Use `**kwargs` to catch and forward extras gracefully.

### With explicit arguments

`super(args)` gives you control when arguments differ.

```ruby
class Document
  def initialize(path)
    @path = path
  end
end

class EncryptedDocument < Document
  def initialize(path, cipher: :aes256)
    @cipher = cipher
    super(path)
  end
end
```

Remember: `super()` with empty parentheses passes zero arguments, even if the caller provided some.

## Method lookup order

When Ruby looks up a method, it walks up the ancestor chain, checking modules included or prepended along the way. Use `ClassName.ancestors` to inspect the order.

```ruby
module Trackable
  def move
    "Tracked"
  end
end

class Vehicle
  def move
    "Rolling"
  end
end

class Tank < Vehicle
  prepend Trackable
end

Tank.ancestors
# => [Trackable, Tank, Vehicle, Object, Kernel, BasicObject]

Tank.new.move # => "Tracked"
```

`prepend` inserts the module before the class in the lookup chain, giving it priority over the class’s own methods. `include` inserts after the class.

## Abstract behavior and template methods

Ruby doesn’t enforce abstract classes, but you can raise errors in base methods to require overrides.

```ruby
class Report
  def generate
    raise NotImplementedError, "#generate must be implemented"
  end
end

class DailyReport < Report
  def generate
    # real implementation
  end
end
```

Template methods define skeleton workflow in the base class and delegate steps to overrides in children.

```ruby
class Exporter
  def export
    validate
    data = fetch
    serialize(data)
  end

  private

  def validate
    # default noop
  end

  def fetch
    raise NotImplementedError
  end

  def serialize(_data)
    raise NotImplementedError
  end
end
```

## Hooks – `inherited`, `method_added`, etc

Use hooks to react when subclasses or methods are defined.

```ruby
class BaseCommand
  def self.inherited(subclass)
    super
    Registry.register(subclass)
  end

  def self.method_added(name)
    super
    puts "Instance method #{name} added to #{self}"
  end
end

class DeployCommand < BaseCommand; end
```

Hook responsibly—excessive meta-programming can make systems difficult to trace.

## `BasicObject` and minimal inheritance

Ruby’s root of the hierarchy is `BasicObject`, a nearly blank class. Deriving from it gives you a minimal object, useful for proxies or delegators.

```ruby
class NullObject < BasicObject
  def method_missing(*_args, &_block)
    self
  end

  def respond_to_missing?(*_args)
    true
  end
end
```

Remember to mix in `Kernel` manually if you need helpers like `puts`.

## Composition vs inheritance

While inheritance is powerful, prefer composition when the relationship isn’t a strict “is-a”. Ask these questions:

1. Do subclasses truly share identity, or just behavior? If it’s only behavior, extract a module.
2. Will the base class need to know about many subclasses? If yes, the hierarchy may be unstable—composition might offer better flexibility.
3. Does the subclass override most methods from the parent? If so, inheritance might not be the right abstraction.

Example using composition:

```ruby
class PdfExporter
  def initialize(renderer: PDF::Renderer.new)
    @renderer = renderer
  end

  def export(data)
    @renderer.render(data)
  end
end
```

Decoupling via composition allows swapping dependencies without altering class hierarchies.

## Checking relationships

`is_a?` (alias `kind_of?`) checks the entire ancestor chain. `instance_of?` checks only the direct class.

```ruby
car = ElectricCar.new(4)
car.is_a?(Vehicle)    # => true
car.instance_of?(Vehicle) # => false
```

Use `respond_to?` to check for capability instead of class when possible—duck typing keeps code flexible.

## Guided practice

1. **Transportation hierarchy**
   - Create `Vehicle`, `Car`, `Boat`, `AmphibiousVehicle` classes.
   - Use modules `Floatable` and `Driveable` with `include`/`prepend` to share behavior. Ensure `AmphibiousVehicle.move` calls both behaviors via `super`.

2. **Template method pattern**
   - Build a `ReportGenerator` base class with steps `prepare`, `generate`, `deliver`.
   - Implement `PdfReport` and `CsvReport` subclasses overriding specific steps. Use `super` to chain base validations.

3. **Hook tracking**
   - Implement a base class that records all subclasses in a registry using `inherited`. Add a `descendants` class method that lists all registered subclasses.

4. **Keyword forwarding**
   - Create a class hierarchy with `BaseJob#initialize(retries:, queue:)`. Override in `EmailJob`, adding `smtp_settings:` while forwarding other keyword arguments properly, including defaults.

5. **Composition refactor**
   - Start with a class method `Report.export_json(data)` that switches on `type` to handle PDF, CSV, etc. Refactor into subclasses + composition (renderers) and compare readability.

## Self-check questions

1. What’s the difference between `super` and `super()` in Ruby?
2. How does `prepend` change the method lookup chain compared to `include`?
3. When would you opt for a module mixin instead of subclassing?
4. Why are class variables (`@@var`) risky in inheritance hierarchies?
5. How can `inherited` hooks help maintain global registries of subclasses, and what are potential pitfalls?

Inheritance should model “is-a” relationships. Use it to share contracts and behavior across related classes, but don’t hesitate to reach for composition or module mixins when the relationship isn’t a clean hierarchy. Understanding Ruby’s lookup chain and `super` semantics ensures subclasses remain predictable and maintainable.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Inheritance — LSP, Composition & Testing (Appendix — inheritance-ruby2)

When to use inheritance vs composition, respecting the Liskov Substitution Principle, and tests to verify substitutability.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Approach</th><th>Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Inheritance</td><td>IS-A relationships</td><td>Prefer for behaviour sharing with clear hierarchies</td></tr>
    <tr><td>Composition</td><td>Has-a relationships</td><td>Better for flexibility and testing</td></tr>
    <tr><td>LSP</td><td>Substitutability</td><td>Derived classes must honour parent contracts</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: composition

```ruby
class Logger
  def initialize(io); @io = io; end
  def log(msg); @io.puts msg; end
end

class Service
  def initialize(logger: Logger.new(STDOUT)); @logger = logger; end
  def call; @logger.log('ok'); end
end
```

### Tests

- Test behaviour through public interfaces; use mocks to assert interactions when needed.

### Exercises (Appendix — inheritance-ruby2)

1. Replace inheritance-based code with composition and add tests proving behaviour preserved.
2. Create a subclass that violates LSP and write a test showing why it's incorrect; then fix and re-test.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
