# Attribute Methods

Accessors connect object state to the outside world. In Ruby, instance variables are private by default; you expose them through reader and writer methods. This lesson goes deeper than “use `attr_accessor`” by covering visibility, custom setters, defaults, boolean predicates, class-level accessors, and meta-programming techniques.

## Learning goals

- Choose the most restrictive accessor (`attr_reader`, `attr_writer`, `attr_accessor`) for each attribute.
- Add validation, coercion, and memoization inside accessor methods.
- Build boolean, predicate-style readers and write-only setters for sensitive data.
- Create class/module-level accessors with `class << self` or `mattr_accessor`-style patterns.
- Generate accessors dynamically when working with repetitive structures, while keeping code maintainable.

## Built-in attribute macros

Ruby provides three macros for instance variables:

```ruby
class Person
  attr_reader :id, :created_at      # getter only
  attr_writer :password             # setter only
  attr_accessor :email, :timezone   # getter + setter

  def initialize(id:, email:, timezone: "UTC")
    @id = id
    @created_at = Time.now
    @password = nil
    @email = email
    @timezone = timezone
  end
end
```

- `attr_reader` prevents outside mutation, great for identifiers or derived values.
- `attr_writer` hides sensitive fields while allowing updates (e.g., passwords).
- `attr_accessor` is convenient but use it only when both directions truly belong in the public API.

## Customizing accessors

Need validation or side effects? Define methods manually.

```ruby
class Temperature
  attr_reader :celsius

  def celsius=(value)
    @celsius = value.to_f.clamp(-273.15, 5_000)
  end

  def fahrenheit
    (@celsius * 9.0 / 5.0) + 32
  end

  def fahrenheit=(value)
    self.celsius = (value.to_f - 32) * 5.0 / 9.0
  end
end

temp = Temperature.new
temp.celsius = 25
temp.fahrenheit # => 77.0
```

Manual setters let you clamp values, coerce types, or trigger callbacks.

## Boolean predicates

Expose boolean state with a `?` suffix. You can still provide a writer using a regular name.

```ruby
class FeatureFlag
  def enabled?
    @enabled
  end

  def enabled=(value)
    @enabled = !!value
  end
end
```

Ruby doesn’t generate predicate readers automatically; define them explicitly for clarity. Consider `attr_predicate :enabled, true` if you use ActiveSupport.

## Lazy readers and memoization

Use `attr_reader` in combination with helper methods to memoize expensive computations.

```ruby
class Report
  attr_reader :data_source

  def totals
    @totals ||= data_source.fetch_totals
  end
end
```

Memoizing inside the reader keeps repeated calls fast while avoiding premature work.

## Visibility modifiers for accessors

You can make accessors private to restrict usage.

```ruby
class ApiClient
  attr_accessor :token
  private :token, :token=

  def initialize(token:)
    self.token = token
  end

  def call(endpoint)
    HTTP.auth(token).get(endpoint)
  end
end
```

Private readers/writers are useful when values should only be touched internally yet still benefit from accessor syntax.

## Working with keyword accessors

Ruby’s `attr_*` macros operate on instance variables. When you want to support keyword initialization, combine them with keyword arguments in `initialize`.

```ruby
class Config
  attr_reader :timeout, :retries

  def initialize(timeout: 5, retries: 3)
    @timeout = timeout
    @retries = retries
  end
end
```

## Write-only attributes

Sometimes you only need to set a value (e.g., storing hashed passwords) without exposing it.

```ruby
class User
  attr_reader :password_digest

  def password=(value)
    @password_digest = BCrypt::Password.create(value)
  end
end
```

Avoid using `attr_writer` for passwords if the setter requires custom logic—define it manually so the digest is stored securely.

## Class and module accessors

`attr_*` macros affect instances. For class-level state, open the singleton class.

```ruby
class Settings
  class << self
    attr_accessor :redis_url, :feature_flags
  end
end

Settings.redis_url = ENV.fetch("REDIS_URL", "redis://localhost:6379")
```

Frameworks like Rails provide `mattr_accessor` and friends for class/module attributes. Implement your own if needed:

```ruby
module ClassAttribute
  def cattr_accessor(name)
    singleton_class.class_eval do
      attr_accessor name
    end
  end
end

class Config
  extend ClassAttribute
  cattr_accessor :app_name
end
```

## Dynamic accessor generation

When dealing with repetitive attributes (e.g., JSON columns), you can define accessors programmatically.

```ruby
class Preferences
  SETTINGS = %i[email_notifications sms_notifications dark_mode].freeze

  SETTINGS.each do |setting|
    define_method(setting) { @store.fetch(setting, false) }
    define_method("#{setting}=") { |value| @store[setting] = !!value }
  end

  def initialize(store = {})
    @store = store
  end
end
```

Document generated methods to keep your API discoverable.

## Protecting invariants in setters

When an attribute must obey rules, the setter enforces them.

```ruby
class Order
  attr_reader :status

  VALID_STATUSES = %w[pending paid shipped cancelled].freeze

  def status=(value)
    value = value.to_s
    raise ArgumentError, "Invalid status" unless VALID_STATUSES.include?(value)

    @status = value
  end
end
```

Setters are a natural place for validation, coercion, or callbacks.

## Thread safety considerations

Attribute writers aren’t atomic. For multi-threaded code, guard mutations with mutexes or use thread-safe data structures.

```ruby
class Counter
  attr_reader :value

  def initialize
    @value = 0
    @mutex = Mutex.new
  end

  def value=(new_value)
    @mutex.synchronize { @value = new_value }
  end
end
```

## Guided practice

1. **Immutable ID**
   - Create a `User` class where `id` is read-only (`attr_reader`), `email` is read/write, and `password` is write-only with hashing.

2. **Settings with defaults**
   - Implement `AppSettings` that exposes `attr_reader` for defaults, `attr_writer` for overrides, and memoizes combined values.

3. **Boolean helpers**
   - Add predicate-style readers for `published?` and corresponding setters that coerce truthy/falsey values.

4. **Class-level configuration**
   - Build a module with `class << self; attr_accessor :logger; end` to share a logger across service classes. Demonstrate usage from multiple classes.

5. **Dynamic accessors**
   - Generate accessors for a list of CSV column names using `define_method`. Ensure setters trim whitespace before storing values.

## Self-check questions

1. Why is `attr_reader` often safer than `attr_accessor` when exposing internal state?
2. How can you enforce validation or transformation logic inside a setter created with `attr_writer`?
3. What’s the difference between instance-level accessors (`attr_accessor`) and class-level accessors defined inside `class << self`?
4. How do predicate-style readers (`active?`) improve API readability, and how would you implement their setters?
5. When does it make sense to generate accessors dynamically with `define_method`, and what documentation considerations come with that choice?

Attribute methods help you maintain encapsulation while giving callers the data they need. Reach for the least-permissive accessor, inject validation where it matters, and prefer explicit, well-documented readers and writers over indiscriminate `attr_accessor` usage.
