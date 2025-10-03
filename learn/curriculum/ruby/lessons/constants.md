# Constants and Configuration Values

Constants capture values that should not change while your Ruby program runs—API endpoints, numeric limits, version strings, configuration toggles. They also serve as anchors for class and module names. This lesson goes beyond `MAX_USERS = 100` and digs into constant lookup rules, namespacing, immutability, and dynamic constant management.

## Learning goals

- Declare and organize constants using idiomatic naming conventions.
- Understand Ruby’s constant lookup path and how namespacing affects reference resolution.
- Control mutability: freeze constant data structures and know when reassignment is acceptable.
- Work with dynamic constants via `const_set`, `const_defined?`, and `autoload` while maintaining clarity.
- Apply constants to real-world configuration, environment settings, and domain limits.

## Declaring constants

Constants are identifiers beginning with an uppercase letter. Ruby warns (but does not forbid) reassignment, so choose names carefully and avoid mutating them in place.

```ruby
PI = 3.1415926535
MAX_CONNECTIONS = 100
API_BASE_URL = "https://api.example.com"
```

When constants reference mutable objects (arrays, hashes), freeze them to protect against accidental modification.

```ruby
ROLES = %w[admin editor viewer].freeze
CONFIG = {
  timeout: 30,
  retries: 3
}.freeze
```

## Classes and modules are constants

`User`, `OrderProcessor`, and modules like `Billing` are constants under the hood. Their namespacing follows the constant lookup rules described below.

```ruby
module Payment
  class Invoice
  end
end

Payment::Invoice # => constant lookup via namespace
```

## Constant lookup rules

Ruby resolves constants by walking the lexical scope first, then the ancestors of the surrounding classes/modules, and finally `Object`.

```ruby
PI = 3.1

module Geometry
  PI = 3.14159

  class Circle
    def circumference(radius)
      2 * PI * radius
    end
  end
end

Geometry::Circle.new.circumference(1)
# => 6.28318 (uses Geometry::PI, not top-level PI)
```

Use absolute paths (`::PI`) to access top-level constants explicitly.

```ruby
::PI # => 3.1 (top-level constant)
```

When in doubt, check `Module.nesting` inside your scope to see the lookup order.

## Namespacing constants

Group related constants within modules or classes to avoid collisions and clarify intent.

```ruby
module Currency
  USD = "USD"
  EUR = "EUR"
  SUPPORTED = [USD, EUR].freeze
end

module Limits
  MAX_LOGIN_ATTEMPTS = 5
  SESSION_TIMEOUT = 30.minutes
end

Currency::SUPPORTED
```

You can even nest modules: `Billing::Taxes::RATE`.

## Reassignment and warnings

Ruby warns on reassigning constants. Instead of reassignment, prefer updating configuration via other mechanisms or replace the object entirely (emitting one warning intentionally during boot if necessary).

```ruby
TIMEOUT = 10
TIMEOUT = 20
# warning: already initialized constant TIMEOUT
```

To avoid warnings during configuration reloads, define constants once and mutate a copy carefully—or better, encapsulate config inside mutable objects.

```ruby
module AppConfig
  class << self
    attr_accessor :settings
  end

  self.settings = {
    timeout: 10,
    retries: 5
  }
end

AppConfig.settings[:timeout] = 15
```

## `freeze` and immutability

Freezing constant values prevents accidental mutation.

```ruby
DEFAULT_HEADERS = {
  "Content-Type" => "application/json"
}.freeze

# DEFAULT_HEADERS["Content-Type"] = "text/plain" # => FrozenError
```

For nested structures, consider `deep_freeze` (implement yourself or use ActiveSupport’s `deep_dup`/`deep_freeze`).

## Dynamic constants

Define constants dynamically with `const_set`. Use sparingly—too much dynamism harms readability.

```ruby
module Features
  def self.enable(name)
    const_set(name.upcase, true)
  end
end

Features.enable(:beta_mode)
Features::BETA_MODE # => true
```

Check for existing definitions with `const_defined?` to avoid collisions.

```ruby
module Features
  def self.enabled?(name)
    const_defined?(name.upcase)
  end
end
```

## Autoloading constants

`Module#autoload` defers loading until the constant is first referenced. Useful for large applications where loading everything upfront is expensive.

```ruby
module Services
  autoload :EmailSender, "services/email_sender"
end

Services::EmailSender.new # loads file on demand
```

With frameworks like Rails, Zeitwerk manages autoloading automatically. When coding by hand, ensure autoload paths are correct.

## Environment-specific constants

Encapsulate environment logic instead of scattering `if ENV[...]` throughout the code.

```ruby
module Environments
  PRODUCTION = "production"
  STAGING = "staging"
  DEVELOPMENT = "development"
end

CURRENT_ENV = (ENV["APP_ENV"] || Environments::DEVELOPMENT).freeze

if CURRENT_ENV == Environments::PRODUCTION
  # production-only setup
end
```

## Well-known Ruby constants

Ruby ships with global constants like `RUBY_VERSION`, `RUBY_PLATFORM`, and `$LOAD_PATH`. Avoid reusing these names. Explore `Object.constants` to inspect what’s already defined.

## Constant visibility and privacy

You can control constant visibility with `private_constant` (Ruby 2.1+).

```ruby
module Secrets
  API_KEY = "secret".freeze
  private_constant :API_KEY

  def self.fetch
    API_KEY
  end
end

Secrets::API_KEY # => NameError
```

This helps prevent leaking sensitive values outside their intended scope.

## Using constants to avoid magic numbers

Replace raw literals with descriptive names.

```ruby
MAX_RETRIES = 5
PAUSE_BETWEEN_RETRIES = 0.5 # seconds

MAX_RETRIES.times do |attempt|
  break if perform_request
  sleep PAUSE_BETWEEN_RETRIES
end
```

This approach clarifies intent and centralizes configuration adjustments.

## Guided practice

1. **Feature flag registry**
   - Create a `Features` module with constants representing active features.
   - Add helper methods `enabled?(key)` and `enable!(key)` using `const_defined?` and `const_set`.
   - Hide raw constants from external callers via `private_constant`.

2. **Namespaced limits**
   - Build a `Billing::Limits` module grouping plan-specific constants (`FREE_TIER_REQUEST_LIMIT`, `PRO_TIER_REQUEST_LIMIT`).
   - Provide `Billing::Limits.for(plan)` that returns the correct limit.

3. **Autoload practice**
   - Set up a small project where referencing `Services::PdfExporter` triggers an autoload. Confirm that the file isn’t loaded until the constant is accessed.

4. **Immutable configuration**
   - Define a nested configuration hash for database settings and implement a helper `deep_freeze!` to protect the structure.
   - Demonstrate that attempts to mutate raises `FrozenError`.

5. **Constant lookup exploration**
   - Create nested modules and classes with constants of the same name.
   - Inspect `Module.nesting` and `ancestors` inside methods to predict which constant Ruby resolves.

## Self-check questions

1. How does Ruby resolve constants when the same name exists in multiple scopes?
2. Why is freezing constant hashes or arrays beneficial? When might you skip freezing?
3. What’s the difference between `const_set` and `const_defined?`, and when would you use them?
4. How does `autoload` help manage large codebases? What caveats should you keep in mind?
5. When would `private_constant` be a better choice than documenting “do not use this constant” in comments?

Constants bring structure to configuration and reduce ambiguity in your code. Respect their immutability, organize them within clear namespaces, and leverage Ruby’s lookup rules to keep your codebase tidy and intention-revealing.
