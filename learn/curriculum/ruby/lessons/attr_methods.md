# Attribute Methods

Accessors connect object state to the outside world. In Ruby, instance variables are private by default; you expose them through reader and writer methods. This lesson goes deeper than “use `attr_accessor`” by covering visibility, custom setters, defaults, boolean predicates, class-level accessors, and meta-programming techniques.

## Learning goals

- Choose the most restrictive accessor (`attr_reader`, `attr_writer`,
  `attr_accessor`) for each attribute.
- Add validation, coercion, and memoization inside accessor methods.
- Build boolean, predicate-style readers and write-only setters for sensitive
  data.
- Create class/module-level accessors with `class << self` or
  `mattr_accessor`-style patterns.
- Generate accessors dynamically when working with repetitive structures, while
  keeping code maintainable.

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

- `attr_reader` prevents outside mutation, great for identifiers or derived
  values.
- `attr_writer` hides sensitive fields while allowing updates (e.g., passwords).
- `attr_accessor` is convenient but use it only when both directions truly
  belong in the public API.

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

Memoizing inside the reader keeps repeated calls fast while avoiding premature
work.

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

Private readers/writers are useful when values should only be touched internally
yet still benefit from accessor syntax.

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

Sometimes you only need to set a value (e.g., storing hashed passwords) without
exposing it.

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

When dealing with repetitive attributes (e.g., JSON columns), you can define
accessors programmatically.

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

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->
<!-- markdownlint-disable MD022 MD032 MD024 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Attribute Methods — Patterns & Tests (Appendix — attr_methods-playbook) (Appendix)

Quick patterns for safe accessors, predicate readers, and class-level attributes.

<!-- markdownlint-disable MD033 MD013 -->
<table>
  <thead>
    <tr><th>Problem</th><th>Pattern</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr><td>Expose state</td><td>Use `attr_reader`</td><td>Prefer immutability where possible</td></tr>
    <tr><td>Sensitive setters</td><td>Custom writer</td><td>Hashing, coercion, and validations belong here</td></tr>
    <tr><td>Class state</td><td>Singleton accessors</td><td>Use `class << self` or `mattr_accessor` patterns</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD013 -->

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix)

1. Implement a boolean predicate and a write-only setter for a `password` field;
   write tests that assert the digest is stored and the raw value isn't exposed.
2. Add a class-level setting with `cattr_accessor` and test isolation between
   classes.

<!-- markdownlint-enable MD013 -->
### Self-check (Appendix)

1. When should you prefer `attr_reader` over `attr_accessor`?
2. How do you safely expose derived values without leaking mutable state?

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD013 -->
## Practical Appendix: Accessors — Security & Patterns (Appendix — attr_methods-ruby-sec) (Appendix)

Notes for secure attribute design and patterns to avoid leaking internal state.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Risk</th><th>Pattern</th><th>Mitigation</th></tr>
  </thead>
  <tbody>
    <tr><td>Exposing secrets</td><td>attr_accessor :api_key</td><td>Use write-only setter and store hashed/encrypted</td></tr>
    <tr><td>Mutable defaults</td><td>attr_accessor :options</td><td>Use frozen defaults or dup in initializer</td></tr>
    <tr><td>Class state leaks</td><td>class-level attrs</td><td>Prefer dependency injection for testability</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Quick patterns (Appendix)

- Use `dup` for default arrays/hashes in constructors.
- Make sensitive readers private and expose behavior via public methods.
- Avoid class variables (`@@`) for per-subclass state; prefer class instance
  variables.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-enable MD013 -->
## Protecting invariants in setters (Appendix)

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

<!-- markdownlint-disable MD013 -->
## Thread safety considerations (Appendix)

Attribute writers aren’t atomic. For multi-threaded code, guard mutations with
mutexes or use thread-safe data structures.

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

<!-- markdownlint-enable MD013 -->
## Guided practice (Appendix)

1. **Immutable ID**
   - Create a `User` class where `id` is read-only (`attr_reader`), `email` is
     read/write, and `password` is write-only with hashing.

2. **Settings with defaults**
   - Implement `AppSettings` that exposes `attr_reader` for defaults,
     `attr_writer` for overrides, and memoizes combined values.

3. **Boolean helpers**
   - Add predicate-style readers for `published?` and corresponding setters that
     coerce truthy/falsey values.

4. **Class-level configuration**
   - Build a module with `class << self; attr_accessor :logger; end` to share a
     logger across service classes. Demonstrate usage from multiple classes.

5. **Dynamic accessors**
   - Generate accessors for a list of CSV column names using `define_method`.
     Ensure setters trim whitespace before storing values.

<!-- markdownlint-disable MD013 -->
## Self-check questions (Appendix)

1. Why is `attr_reader` often safer than `attr_accessor` when exposing internal
   state?
2. How can you enforce validation or transformation logic inside a setter
   created with `attr_writer`?
3. What’s the difference between instance-level accessors (`attr_accessor`) and
   class-level accessors defined inside `class << self`?
4. How do predicate-style readers (`active?`) improve API readability, and how
   would you implement their setters?
5. When does it make sense to generate accessors dynamically with
`define_method`, and what documentation considerations come with that choice?

Attribute methods help you maintain encapsulation while giving callers the data they need. Reach for the least-permissive accessor, inject validation where it matters, and prefer explicit, well-documented readers and writers over indiscriminate `attr_accessor` usage.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Attribute Methods  Security, Testing & Threading (Appendix  attr_methods-hidden-20251005)

Practical recommendations for using accessors safely in real projects.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Exposing secrets</td><td>Write-only setter</td><td>Store hashes/digests, keep readers private</td></tr>
    <tr><td>Mutable defaults</td><td>dup or freeze in initializer</td><td>Prevents cross-instance leakage</td></tr>
    <tr><td>Thread safety</td><td>Mutex around writers</td><td>Protect concurrent mutations</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: thread-safe writer

```ruby
class Counter
  attr_reader :value
  def initialize
    @value = 0
    @mutex = Mutex.new
  end

  def increment
    @mutex.synchronize { @value += 1 }
  end
end
```

### Exercises

1. Convert a class with `attr_accessor :options` so that the initializer
   duplicates the default hash to avoid shared mutation; add tests to prove
   instances are isolated.
2. Implement a write-only `password=` setter that stores a bcrypt digest and
   write a test that ensures the raw password is not exposed.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Advanced Attribute Methods Techniques (Appendix — attr_methods-advanced-20251005)

Explore lesser-known aspects of attr methods, including deprecations, multiple
arguments, and performance considerations.

<!-- markdownlint-disable MD033 -->
<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Method</th><th>Purpose</th><th>Insider Tip</th></tr>
  </thead>
  <tbody>
    <tr><td><code>attr_reader</code></td><td>Getter methods</td><td>Use for immutable attributes</td></tr>
    <tr><td><code>attr_writer</code></td><td>Setter methods</td><td>For sensitive data</td></tr>
    <tr><td><code>attr_accessor</code></td><td>Getter + setter</td><td>Convenient but selective</td></tr>
    <tr><td><code>attr</code></td><td>Deprecated alias</td><td>Avoid; use <code>attr_reader</code></td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Advanced Examples

```ruby
class Example
  # Multiple attributes in one call
  attr_reader :name, :age
  attr_accessor :email

  # Old deprecated form (warns)
  # attr :deprecated_attr, true  # equivalent to attr_accessor

  def initialize(name, age, email)
    @name = name
    @age = age
    @email = email
  end
end

# Usage
ex = Example.new("Alice", 30, "alice@example.com")
ex.name  # => "Alice"
ex.age   # => 30
ex.email = "new@example.com"
```

### Exercises — Advanced Techniques

1. Define a class using `attr_reader` for multiple attributes and verify they
   are read-only.
2. Implement a custom setter alongside `attr_writer` for validation.
3. Compare `attr_accessor` vs manual getter/setter for performance in a
   benchmark.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
