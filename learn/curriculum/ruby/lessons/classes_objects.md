# Classes and Objects

Ruby leans hard into object orientation: every value you touch is an object, and classes define how those objects behave. Understanding how to model state, expose behavior, and leverage Ruby’s object model unlocks the language’s flexibility.

## Learning goals

- Create classes with expressive constructors, default values, and keyword arguments.
- Manage object state using instance variables, readers, writers, and encapsulation techniques.
- Differentiate between instance methods, class methods, and singleton methods.
- Implement custom string representations, comparisons, and validation hooks.
- Apply visibility (`public`, `private`, `protected`) and composition to design cohesive objects.

## Defining classes and instantiating objects

The simplest class uses the `class` keyword and ends with `end`. Instances are created via `.new`.

```ruby
class Vehicle
  # behavior and state live here
end

car = Vehicle.new
p car.class  # => Vehicle
```

Every class in Ruby inherits from `Object` unless explicitly changed. You can verify the inheritance chain with `Vehicle.ancestors`.

## Constructors with `initialize`

`initialize` runs automatically when you call `.new`. It’s the place to populate instance variables and enforce invariants. Keyword arguments improve clarity and support defaults.

```ruby
class Vehicle
  attr_reader :wheels, :fuel, :capacity, :max_speed

  def initialize(wheels:, fuel:, capacity:, max_speed: 120)
    @wheels = Integer(wheels)
    @fuel = fuel
    @capacity = capacity
    @max_speed = max_speed
  end
end

tesla = Vehicle.new(wheels: 4, fuel: :electric, capacity: 5, max_speed: 250)
```

Use argument validation to fail fast when inputs are invalid. Raising `ArgumentError` inside `initialize` helps catch mistakes early.

## Instance variables and encapsulation

Instance variables (`@name`) belong to each object. They’re private storage—not accessible outside without readers/writers.

```ruby
class Person
  def initialize(name, age)
    @name = name
    @age = age
  end

  def age
    @age
  end

  def age=(value)
    raise ArgumentError, "Age must be positive" unless value.positive?
    @age = value
  end
end

person = Person.new("Ava", 29)
person.age = 30
```

Prefer exposing intention-revealing methods (`retire!`, `birthday!`) instead of raw setters when domain rules exist.

### Attribute helpers

Ruby provides macros to generate readers/writers:

```ruby
class Account
  attr_reader :id
  attr_accessor :balance

  def initialize(id, balance = 0.0)
    @id = id
    @balance = balance
  end
end

acct = Account.new("A-100", 50.00)
acct.balance += 10
```

Variants:

- `attr_reader`: getter only.
- `attr_writer`: setter only.
- `attr_accessor`: getter + setter.
- `attr :name, true`: legacy alias for `attr_reader` (avoid).

## Instance methods define behavior

Define behavior by adding methods. Ruby implicitly makes methods public unless you change visibility.

```ruby
class Vehicle
  def initialize(max_speed)
    @max_speed = max_speed
  end

  def accelerate
    "Accelerating to #{@max_speed} km/h"
  end

  def honk!
    "VRRRM!"
  end
end

puts Vehicle.new(180).accelerate
```

Side-effect-free methods (returning strings) are easier to test than ones that print. Reserve `puts` for user interfaces; otherwise return values.

## Class-level behavior

Sometimes logic belongs to the class itself, not individual instances. There are two ways to define class methods:

```ruby
class MathHelper
  def self.square(x)
    x * x
  end

  class << self
    def cube(x)
      x**3
    end
  end
end

MathHelper.square(3)  # => 9
MathHelper.cube(3)    # => 27
```

Use class methods for factories, configuration, or cached data. If you find yourself storing lots of class-level state, consider extracting a dedicated object instead to avoid tight coupling.

### Class instance variables vs class variables

- `@counter` defined inside `class << self` or on the class object is a class instance variable (per subclass).
- `@@counter` is shared across the inheritance chain—be cautious: subclasses can accidentally change parent state. Favor class instance variables.

```ruby
class ApiClient
  class << self
    attr_accessor :timeout
  end
end

ApiClient.timeout = 5
```

## Stringifying and inspection

Override `to_s` for user-friendly output and `inspect` for debugging.

```ruby
class Ticket
  attr_reader :id, :status

  def initialize(id, status: :open)
    @id = id
    @status = status
  end

  def to_s
    "Ticket ##{@id} (#{@status})"
  end

  def inspect
    "#<Ticket id=#{@id} status=#{@status}>"
  end
end

puts Ticket.new(42)
p Ticket.new(42)
```

`puts` uses `to_s`; `p` uses `inspect`.

## Equality and comparison

Controlling equality is crucial for value objects.

```ruby
class Money
  include Comparable

  attr_reader :amount, :currency

  def initialize(amount, currency)
    @amount = amount
    @currency = currency
  end

  def <=>(other)
    return unless currency == other.currency
    amount <=> other.amount
  end

  def eql?(other)
    amount.eql?(other.amount) && currency.eql?(other.currency)
  end

  def hash
    [amount, currency].hash
  end
end

prices = [Money.new(5, :USD), Money.new(10, :USD)]
prices.sort # works because of Comparable
```

Implementing `<=>`, `eql?`, and `hash` lets your objects play nicely with `sort`, `uniq`, and hash-based collections.

## Visibility: public, private, protected

- **Public**: default; callable from anywhere.
- **Private**: callable only within the defining object (no explicit receiver).
- **Protected**: callable within the defining class and subclasses, even with explicit receivers.

```ruby
class BankAccount
  attr_reader :balance

  def initialize(balance = 0)
    @balance = balance
  end

  def transfer_to(other_account, amount)
    raise "Insufficient funds" if balance < amount

    withdraw(amount)
    other_account.deposit(amount)
  end

  protected

  def deposit(amount)
    @balance += amount
  end

  private

  def withdraw(amount)
    @balance -= amount
  end
end
```

`withdraw` is private (no explicit receiver), while `deposit` is protected so other accounts can call it during transfers.

## Freezing and immutability

Use `freeze` to prevent further mutation when objects should be immutable.

```ruby
class Config
  attr_reader :options

  def initialize(options)
    @options = options.freeze
    freeze
  end
end

settings = Config.new(log_level: :info)
# settings.options[:log_level] = :debug  # raises FrozenError
```

Immutable objects are easier to reason about, thread-safe, and can act as reliable cache keys.

## Composition and delegation

Prefer composition over deep inheritance hierarchies. Delegate behavior using `Forwardable` or simple forwarding methods.

```ruby
require "forwardable"

class ShoppingCart
  extend Forwardable

  def_delegators :@items, :each, :size

  def initialize
    @items = []
  end

  def add(product)
    @items << product
  end
end
```

`Forwardable` lets your object expose collection-like behavior while retaining control.

## Singleton methods and eigenclasses

Objects can have per-instance methods (singleton methods). Ruby stores them in an eigenclass.

```ruby
order = Object.new

def order.status
  "pending"
end

order.status  # => "pending"

class << order
  def cancel!
    @cancelled = true
  end
end

order.cancel!
```

Use singleton methods for special cases like mock objects or DSLs, but avoid polluting production objects with ad-hoc methods at runtime.

## Lifecycle hooks

- `initialize`: runs on instantiation.
- `initialize_copy`: runs on `dup`/`clone`.
- `method_missing`: fallback when a method isn’t found (pair with `respond_to_missing?`).
- `included`, `extended`, `inherited`: module/class callbacks.

```ruby
class BaseWidget
  def self.inherited(subclass)
    super
    Registry.register(subclass)
  end
end
```

Hooks enable meta-programming, but use judiciously—they can make control flow harder to follow.

## Guided practice

1. **Value object**
   - Implement a `Coordinate` class with `x`, `y`, `z` values.
   - Make it immutable, define `==`, `eql?`, `hash`, and `to_s`.
   - Add a `distance_to(other)` instance method using Euclidean distance.

2. **Factory methods**
   - Create a `User` class with a `self.from_csv(row)` class method that builds instances from CSV data.
   - Ensure email validation occurs in `initialize`.

3. **Visibility exercise**
   - Model a `BankAccount` that keeps `deposit` public, `withdraw` private, and includes a protected `balance_for(other)` method used during transfers.

4. **Delegation**
   - Build a `Playlist` class that internally stores an array of `Track` objects.
   - Delegate `each`, `map`, and `size` to the internal array while adding a custom `total_duration` method.

5. **Configuration builder**
   - Design a `Config` class that accepts keyword arguments, merges them with defaults, and freezes resulting hashes.
   - Provide `Config.load(path)` to parse YAML into instances.

## Self-check questions

1. What’s the difference between an instance variable and a class instance variable? How do they relate to inheritance?
2. Why is returning an immutable object (via `freeze`) beneficial for configuration classes?
3. When would you choose to override `inspect` in addition to `to_s`?
4. How does including `Comparable` simplify implementing ordering for value objects?
5. Why should custom `method_missing` implementations also define `respond_to_missing?`?

Well-designed classes make code bases flexible and maintainable. Model real-world concepts with clear state and focused behavior, keep your public interfaces small, and leverage Ruby’s object model to express intent cleanly.
