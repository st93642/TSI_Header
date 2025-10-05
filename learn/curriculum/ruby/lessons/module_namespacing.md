# Lesson 8.5: Module Namespacing

Modules are the primary tool for namespacing in Ruby. By grouping related constants, classes, and methods under a module, you avoid collisions, communicate ownership, and make large codebases navigable. This lesson focuses on structuring namespaces effectively, loading namespaced code, and balancing nesting depth with clarity.

## Learning goals

- Define modules to group related code and prevent constant collisions.
- Understand how constant lookup works inside nested modules.
- Organize directory structures that align with namespace hierarchies.
- Leverage autoloading (`require`, `require_relative`, `autoload`) alongside namespaced modules.
- Adopt naming conventions and patterns that scale in large applications.

## Core namespacing pattern

```ruby
module Graphics
  class Circle
    def draw
      "Drawing a circle"
    end
  end

  class Square
    def draw
      "Drawing a square"
    end
  end
end

Graphics::Circle.new.draw
```

Here, `Graphics::Circle` and `Graphics::Square` live under the `Graphics` namespace. Ruby stores both classes as constants `Circle` and `Square` on the `Graphics` module object.

## Constant lookup in nested modules

Ruby resolves constants by scanning the lexical scope first, then the ancestors of the current class/module, and finally `Object`. Nested modules narrow the lookup to the namespace you intend.

```ruby
module Math
  PI = 3.14159

  module Trig
    def self.sin(x)
      # use namespaced constant
      x - (x**3) / 6.0
    end
  end
end

Math::Trig.sin(0.5)
```

Access nested constants with `::`: `Math::Trig` or `Math::PI`. Use `::PI` for top-level constants.

`Module.nesting` shows the current lexical namespace stack—helpful when debugging constant resolution.

## Deep namespaces and directory structure

Mirroring namespaces in the filesystem keeps autoloaders and future maintainers happy.

```text
app/
  services/
    payments/
      charge.rb   # defines Services::Payments::Charge
```

File `charge.rb`:

```ruby
module Services
  module Payments
    class Charge
      def call
        # implementation
      end
    end
  end
end
```

Tools like Zeitwerk (used by Rails) expect `Services::Payments::Charge` to live at `services/payments/charge.rb`. Following this convention avoids load errors.

## Avoiding name collisions

Namespacing lets identical class names coexist without interfering.

```ruby
module Blog
  class Post; end
end

module Social
  class Post; end
end

Blog::Post.new
Social::Post.new
```

Each `Post` is distinct because it lives under a different module.

## Module functions and singleton methods

Modules can expose functionality via module (singleton) methods without becoming mixins.

```ruby
module StringUtils
  module_function

  def slugify(text)
    text.downcase.gsub(/[^a-z0-9]+/, "-").gsub(/^-|-$/, "")
  end
end

StringUtils.slugify("Hello, World!")
```

`module_function` marks methods as both module-level and private instance methods when mixed in.

Alternatively, define methods as `self.method_name` to expose utilities within the namespace: `Services::Clock.now`.

## Combining mixins with namespaces

Modules can encapsulate shared behavior and namespaced classes side by side.

```ruby
module Database
  module Connection
    def connect(url)
      @connection = "Connected to #{url}"
    end

    def connected?
      !@connection.nil?
    end
  end

  class PostgreSQL
    include Connection
  end
end

Database::PostgreSQL.new.connect("postgres://localhost")
```

Keep mixins near the classes that rely on them for quick discovery.

## Autoloading namespaced code

Explicit requires:

```ruby
require_relative "services/payments/charge"

Services::Payments::Charge.new.call
```

Lazy loading via `Module#autoload`:

```ruby
module Services
  autoload :Invoice, "services/invoice"
end

Services::Invoice.new # loads file when first accessed
```

In frameworks with autoloading (Rails, Hanami), follow naming conventions and let the loader manage `require` statements.

## Versioning and API boundaries

Namespaces excel at isolating versions or feature slices.

```ruby
module API
  module V1
    class UsersController; end
  end

  module V2
    class UsersController; end
  end
end
```

Controllers remain independent yet share a logical grouping. This approach simplifies gradual upgrades.

## Namespacing and gems

Gem authors wrap public classes inside a root module to prevent collisions.

```ruby
module MyGem
  class Client; end
end

MyGem::Client.new
```

The top-level file typically sets up the namespace and requires additional files: `require "my_gem/client"` etc.

## Guarding privacy with `private_constant`

Hide implementation details by keeping helper classes private to a namespace.

```ruby
module Payments
  class TokenSigner; end
  private_constant :TokenSigner

  def self.sign(payload)
    TokenSigner.new.call(payload)
  end
end

# Payments::TokenSigner # => NameError
```

This keeps your external API intentional and malleable.

## Guidelines for healthy namespaces

- **Choose descriptive module names.** Favor `Billing::Invoices` over `BI`.
- **Keep nesting shallow.** Two to three levels is usually enough; deeper trees impede discovery.
- **Align directories with namespaces.** Autoloaders and human readers expect this convention.
- **Avoid polluting the global namespace.** Wrap top-level behavior in a root module (`MyApp`).
- **Document module responsibilities.** A short YARD docstring or comment prevents guesswork.

## Guided practice

1. **Service grouping**
   - Create `Ecommerce::Services::PaymentProcessor` and `Ecommerce::Services::ShippingCalculator` in separate files.
   - Ensure `require_relative` or autoload wires them correctly from a small runner script.

2. **Versioned API**
   - Implement `API::V1::OrdersController` and `API::V2::OrdersController` with different response shapes.
   - Build a router that dispatches based on version parameter using `const_get`.

3. **Feature isolation**
   - Create `Features::Checkout` and `Features::Profile` modules. Each should expose a `.enabled?` method reading from environment variables.
   - Hide helper constants behind `private_constant`.

4. **Autoload experiment**
   - Define a namespace `Analytics` with `autoload :Reporter, "analytics/reporter"`.
   - Verify the file is only loaded when `Analytics::Reporter` is referenced.

5. **Constant lookup exploration**
   - Inside a nested namespace, define constants with duplicate names (`Status`) at different levels.
   - Use `Module.nesting` and `ancestors` prints to show which `Status` Ruby resolves.

## Self-check questions

1. How does Ruby determine which constant to use when names are duplicated across modules?
2. Why should directory structure mirror namespace structure, especially in autoloaded projects?
3. When might you favor modules with singleton methods (`module_function`) over classes?
4. How can `private_constant` help maintain a clean public API inside a namespace?
5. What are the pros and cons of deep namespace hierarchies, and how do you keep them manageable?

Thoughtful namespacing keeps Ruby codebases intuitive as they grow. Group related components, expose clear APIs, and lean on modules to prevent naming conflicts while maintaining a discoverable structure.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Module Namespacing — Autoload, Constants & Testing (Appendix — module_namespacing-ruby2)

Guidance for organizing code with modules, using `autoload`, managing constants, and testing module boundaries.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Namespace modules</td><td>module MyApp; end</td><td>Group related classes and avoid top-level leakage</td></tr>
    <tr><td>autoload</td><td>autoload :Lib, 'my_app/lib'</td><td>Lazy-load constants to reduce startup time</td></tr>
    <tr><td>Constants</td><td>Freeze values</td><td>Signal immutability and avoid accidental mutation</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example

```ruby
module MyApp
  autoload :Utils, 'my_app/utils'
end
```

### Tests

- Assert that constants are present and that autoload triggers load when referenced.

### Exercises (Appendix — module_namespacing-ruby2)

1. Convert a flat set of classes into a namespaced module and adjust requires; add tests to ensure constant lookup works.
2. Use `autoload` for a large helper and write a test asserting it loads on first use.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Module Namespacing — Gem Layout & Require Order (Appendix — module_namespacing-ruby3)

Practical notes for project layout: `lib/` structure, require order, and avoiding circular requires in namespaced projects.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>lib layout</td><td>lib/mygem.rb + lib/mygem/**</td><td>Keep top-level file small and require internals lazily</td></tr>
    <tr><td>require order</td><td>Explicit requires</td><td>Avoid circular requires by deferring requires inside methods when needed</td></tr>
    <tr><td>autoload pitfalls</td><td>Be careful with constants</td><td>Autoload can surprise during test setup</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix — module_namespacing-ruby3)

1. Scaffold a minimal gem layout with namespaced modules and verify require order by running tests.
2. Introduce a circular require and then refactor to defer loading to break the cycle; add tests confirming load completes.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Modules & Namespacing — Mixins, Names, Autoload (Appendix — module_namespacing-ruby-appendix-20251005)

Practical guidance on using modules for namespacing, mixins, and `autoload` patterns to keep code organized.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Use</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Namespacing</td><td>module Foo; end</td><td>Avoid global constants</td></tr>
    <tr><td>Mixins</td><td>include/extend</td><td>Prefer composition when possible</td></tr>
    <tr><td>Autoload</td><td>autoload :Thing, 'thing'</td><td>Lazy-loading constants</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example (Appendix — module_namespacing-ruby-appendix-20251005-01

```ruby
module Utils
  def self.format(x) x.to_s end
end

class C
  include Utils
end
```

### Exercises (Appendix — module_namespacing-ruby-appendix-20251005-01)

1. Refactor a small set of classes into a module namespace and add tests referencing the new names.
2. Replace a mixin with explicit delegation and benchmark clarity/maintainability.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
