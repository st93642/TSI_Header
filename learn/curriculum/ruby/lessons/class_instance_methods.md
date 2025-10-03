# Lesson 8.3: Class vs Instance Methods

Classes in Ruby can expose behavior at two levels: per-instance and per-class. Choosing the right level keeps your APIs intuitive and your encapsulation tight. Let’s explore how the object model handles both, when to prefer each, and how to design patterns around them.

## Learning goals

- Distinguish between method definitions on instances, classes, and singleton objects.
- Implement class methods using the `self.` prefix, the eigenclass (`class << self`), and module helpers.
- Manage shared state safely using class instance variables (and avoid `@@` pitfalls).
- Decide when to use class methods vs. instance methods for factories, configuration, caching, and utilities.
- Control visibility (`public`, `private`, `protected`, `private_class_method`) across both levels.

## Instance methods recap

Instance methods operate on a specific object and have access to that object’s instance variables. They’re defined inside the class body without any receiver prefix.

```ruby
class Car
  def initialize(make, model)
    @make = make
    @model = model
    @mileage = 0
  end

  def drive(distance)
    @mileage += distance
    "#{@make} #{@model} drove #{distance} km"
  end

  def mileage
    @mileage
  end
end

car = Car.new("Toyota", "Corolla")
car.drive(50)
car.mileage # => 50
```

The method’s implicit receiver (`self`) is the current instance.

## Class methods: defining behavior on the class object

Classes themselves are objects—instances of `Class`. Defining a method with `self.` inside the class body attaches it to the class object.

```ruby
class Car
  def self.fleet
    @fleet ||= []
  end

  def self.register(car)
    fleet << car
  end
end

Car.register(Car.new("Ford", "Focus"))
Car.fleet.size # => 1
```

Alternative syntax uses the eigenclass (`class << self`). Both approaches are equivalent; choose the one that maximizes clarity.

```ruby
class Car
  class << self
    def policy_number
      @policy_number ||= SecureRandom.hex(4)
    end
  end
end
```

### Class instance variables vs class variables

- **Class instance variables** (`@fleet`) belong to the class object. Each subclass gets its own copy.
- **Class variables** (`@@fleet`) are shared across the inheritance chain and can create subtle coupling. Prefer class instance variables unless you explicitly want global sharing.

```ruby
class Vehicle
  class << self
    attr_accessor :registry
  end
end

class Car < Vehicle; end

Vehicle.registry = []
Car.registry # => nil, because class instance variables are not inherited automatically
```

To share defaults safely, override in subclasses or use class macros that copy values.

## Factories, configuration, and singletons

Class methods often serve as entry points when object construction needs extra logic.

```ruby
class User
  attr_reader :name, :role

  def initialize(name, role: :member)
    @name = name
    @role = role
  end

  def self.admin(name)
    new(name, role: :admin)
  end

  def self.from_json(payload)
    data = JSON.parse(payload, symbolize_names: true)
    new(data[:name], role: data[:role]&.to_sym)
  end
end

User.admin("Ava").role # => :admin
```

For singletons, memoize an instance in a class method using a class instance variable.

```ruby
class AppConfig
  class << self
    def instance
      @instance ||= new
    end
  end

  private_class_method :new
end

AppConfig.instance # => singleton
```

Making `.new` private ensures callers go through `.instance`.

## Utility modules and `extend self`

If you only need stateless helpers, modules offer a cleaner pattern than classes with only class methods.

```ruby
module MathUtils
  module_function

  def fahrenheit_to_celsius(f)
    (f - 32) * 5.0 / 9.0
  end

  def hypotenuse(a, b)
    Math.sqrt(a**2 + b**2)
  end
end

MathUtils.hypotenuse(3, 4) # => 5.0
```

`module_function` makes methods both module-level (callable on the module) and private when mixed in; `extend self` exposes module methods as module-level without affecting inclusion.

## Accessing shared state safely

Class methods often manage shared caches or configuration.

```ruby
class ApiClient
  class << self
    attr_accessor :timeout
  end

  self.timeout = 5 # seconds

  def initialize(endpoint)
    @endpoint = endpoint
  end

  def get(path)
    HTTP.with(timeout: self.class.timeout).get(File.join(@endpoint, path))
  end
end
```

Instances can access class-level settings via `self.class`. Keep mutable shared state thread-safe if accessed concurrently.

## Method visibility across levels

- `public`, `protected`, and `private` affect instance methods.
- `private_class_method` hides class methods.

```ruby
class Secret
  def self.expose
    new.secret_code
  end

  def secret_code
    generate
  end

  private

  def generate
    SecureRandom.hex(3)
  end

  class << self
    private

    def audit!
      puts "audited"
    end
  end
end

# Secret.audit! # => NoMethodError
```

`private_class_method :audit!` is equivalent to opening `class << self` and marking methods private.

## Singleton methods on individual instances

You can attach methods to a single object (not the class) using `def object.method` or `define_singleton_method`.

```ruby
user = User.new("Ava")

def user.admin?
  true
end

user.singleton_methods # => [:admin?]
```

Internally, Ruby creates an eigenclass for that object. Use this for quick test doubles or DSLs, but avoid in core domain objects unless you have a compelling reason—it can confuse other developers.

## Inheritance of class methods

Class methods are inherited, but remember that class instance variables aren’t shared by default.

```ruby
class Vehicle
  class << self
    def type
      "vehicle"
    end
  end
end

class Truck < Vehicle
  class << self
    def type
      "truck"
    end
  end
end

Vehicle.type # => "vehicle"
Truck.type   # => "truck"
```

Need per-subclass defaults? Use hooks like `inherited` to set them up.

```ruby
class Vehicle
  class << self
    attr_accessor :category

    def inherited(subclass)
      super
      subclass.category = :land
    end
  end
end

class Boat < Vehicle; end
Boat.category # => :land (maybe override to :water)
```

## Avoiding global state with class methods

While class methods are convenient, overusing them can lead to hidden dependencies and hard-to-test code. Prefer instance methods when behavior depends on instance-specific state or when you might need multiple instances (e.g., different configurations) simultaneously.

### Refactoring hint

If a class method starts accumulating lots of conditional logic or state, consider extracting a service object. Example: `Report.generate(params)` might become `ReportGenerator.new(params).call` to allow dependency injection and testing.

## Practical comparisons

| Scenario | Prefer Instance Method | Prefer Class Method |
|----------|-----------------------|---------------------|
| Operating on object state | ✅ | |
| Calculating shared configuration | | ✅ |
| Creating specialized instances (factories) | | ✅ |
| Running a one-off utility | | ✅ (or module function) |
| Needs polymorphism with different subclasses | ✅ (override per subclass) | ✅ (if class-level behavior differs) |

## Guided practice

1. **Webhook registry**
   - Build a `Webhook` class with class methods `register(event, klass)` and `handlers_for(event)`.
   - Ensure class instance variables are per-subclass so services can isolate handlers.

2. **Multi-tenant clients**
   - Refactor a class method `.client` that returns a global HTTP client into an instance-based approach so each tenant gets a distinct configuration.

3. **Metrics aggregator**
   - Implement `Metrics.record(name, value)` as a class method storing totals in a class instance variable.
   - Expose `Metrics.reset!` as a `private_class_method` so only tests can invoke it.

4. **Custom constructors**
   - Add `self.from_env` and `self.from_yaml(path)` class methods to build `AppConfig` instances.
   - Ensure `.new` remains available but mark it private if you want factory-only instantiation.

5. **Per-instance overrides**
   - Create a `FeatureFlag` object whose individual instances can have singleton methods toggled at runtime (e.g., `def flag.enabled?; false; end`).
   - Inspect the singleton class to confirm where the method lives.

## Self-check questions

1. What are the trade-offs between using class variables (`@@`) and class instance variables when sharing state?
2. How does defining a method with `def self.method` differ from `class << self; def method; end; end`?
3. When might you choose to replace a class method with a module function?
4. Why is memoizing data (`@cache ||= ...`) inside a class method safer than using a global constant?
5. How does `private_class_method` improve encapsulation for factory patterns?

Mastering the distinction between class and instance methods helps you keep responsibilities clear. Reach for class methods when the behavior belongs to the class as a whole—configuration, caching, factories—and lean on instance methods when the logic depends on an object’s unique state.
