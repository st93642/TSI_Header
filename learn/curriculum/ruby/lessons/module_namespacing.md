# Lesson 8.5: Module Namespacing

Modules are the primary tool for namespacing in Ruby. By grouping related
constants, classes, and methods under a module, you avoid collisions,
communicate ownership, and make large codebases navigable. This lesson focuses
on structuring namespaces effectively, loading namespaced code, and balancing
nesting depth with clarity.

## Learning goals

- Define modules to group related code and prevent constant collisions.
- Understand how constant lookup works inside nested modules.
- Organize directory structures that align with namespace hierarchies.
- Leverage autoloading (`require`, `require_relative`, `autoload`) alongside
  namespaced modules.
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

Mirroring namespaces in the filesystem keeps autoloaders and future maintainers
happy.

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

Modules can expose functionality via module (singleton) methods without becoming
mixins.

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

Controllers remain independent yet share a logical grouping. This approach
simplifies gradual upgrades.

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
- **Keep nesting shallow.** Two to three levels is usually enough; deeper trees
  impede discovery.
- **Align directories with namespaces.** Autoloaders and human readers expect
  this convention.
- **Avoid polluting the global namespace.** Wrap top-level behavior in a root
  module (`MyApp`).
- **Document module responsibilities.** A short YARD docstring or comment
  prevents guesswork.

## Guided practice

1. **Service grouping**
   - Create `Ecommerce::Services::PaymentProcessor` and
     `Ecommerce::Services::ShippingCalculator` in separate files.
   - Ensure `require_relative` or autoload wires them correctly from a small
     runner script.

2. **Versioned API**
   - Implement `API::V1::OrdersController` and `API::V2::OrdersController` with
     different response shapes.
   - Build a router that dispatches based on version parameter using
     `const_get`.

3. **Feature isolation**
   - Create `Features::Checkout` and `Features::Profile` modules. Each should
     expose a `.enabled?` method reading from environment variables.
   - Hide helper constants behind `private_constant`.

4. **Autoload experiment**
   - Define a namespace `Analytics` with `autoload :Reporter,
     "analytics/reporter"`.
   - Verify the file is only loaded when `Analytics::Reporter` is referenced.

5. **Constant lookup exploration**
   - Inside a nested namespace, define constants with duplicate names (`Status`)
     at different levels.
   - Use `Module.nesting` and `ancestors` prints to show which `Status` Ruby
     resolves.

## Self-check questions

1. How does Ruby determine which constant to use when names are duplicated
   across modules?
2. Why should directory structure mirror namespace structure, especially in
   autoloaded projects?
3. When might you favor modules with singleton methods (`module_function`) over
   classes?
4. How can `private_constant` help maintain a clean public API inside a
   namespace?
5. What are the pros and cons of deep namespace hierarchies, and how do you keep
   them manageable?

Thoughtful namespacing keeps Ruby codebases intuitive as they grow. Group
related components, expose clear APIs, and lean on modules to prevent naming
conflicts while maintaining a discoverable structure.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Module Namespacing — Autoload, private_constant & Layout (Appendix  module_namespacing-hidden-20251005)

Practical rules-of-thumb for keeping namespaces clean and autoload-friendly.

<!-- markdownlint-disable MD033 MD013 -->
<table>
  <thead>
    <tr><th>Rule</th><th>Why</th><th>Quick action</th></tr>
  </thead>
  <tbody>
    <tr>
      <td>Mirror filesystem to namespaces</td>
      <td>Autoloaders rely on it</td>
      <td>services/payments/charge.rb -> Services::Payments::Charge</td>
    </tr>
    <tr>
      <td>Use private_constant</td>
      <td>Hide internals from public API</td>
      <td>`private_constant :HelperClass`</td>
    </tr>
    <tr>
      <td>Avoid deep nesting</td>
      <td>Too many levels impede discovery</td>
      <td>Prefer submodules of depth 2-3</td>
    </tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD013 -->

### Tiny example — autoload and private

```ruby
module MyApp
  autoload :Reporter, 'my_app/reporter'

  class Internal
  end

  private_constant :Internal
end
```

### Exercises

1. Create a small namespaced class `Tools::Formatter` in
`lib/tools/formatter.rb` and require it from a runner script; ensure constant lookup works.
2. Add a test asserting that `MyApp::Internal` is not accessible (raises
   NameError).

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-disable MD013 -->
## Practical Appendix: Module Introspection & Refinements (Appendix — module_namespacing-refinements-20251005b)

Use ancestors, const_source_location, and refinements for advanced module
management.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Purpose</th><th>Hidden Benefit</th></tr>
  </thead>
  <tbody>
    <tr>
      <td>`ancestors`</td>
      <td>Inspect inheritance chain</td>
      <td>Debug mixin order and resolution</td>
    </tr>
    <tr>
      <td>`const_source_location`</td>
      <td>Find constant definitions</td>
      <td>Locate source files for debugging</td>
    </tr>
    <tr>
      <td>`refinements`</td>
      <td>Scoped monkey-patching</td>
      <td>Avoid global pollution with `using`</td>
    </tr>
    <tr>
      <td>`set_temporary_name`</td>
      <td>Anonymous module naming</td>
      <td>Distinguish dynamic modules without constants</td>
    </tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples — Refinements

```ruby
module MyMod
  refine String do
    def shout
      upcase + "!"
    end
  end
end

class Test
  using MyMod
  def speak
    "hello".shout  # => "HELLO!"
  end
end

# Introspection
MyMod.ancestors  # => [MyMod]
String.const_source_location('upcase')  # => ["(eval)", 1] or file path
```

### Exercises — Module Introspection

1. Define a refinement for Array that adds a `sum_squares` method, and use it in
   a class with `using`.
2. Use `const_source_location` to find where a standard library constant is
   defined.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Advanced Module Namespacing Techniques (Appendix — module-namespacing-advanced-20251005)

Explore lesser-known features for dynamic constants, lazy loading, and module
introspection to enhance namespacing.

<!-- markdownlint-disable MD033 MD013 -->
<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Description</th><th>Insider Tip</th></tr>
  </thead>
  <tbody>
    <tr><td><code>const_missing</code></td><td>Hook for undefined constants</td><td>Dynamic loading hook</td></tr>
    <tr><td><code>autoload</code></td><td>Lazy-load files on first access</td><td>Reduce startup time</td></tr>
    <tr><td><code>private_constant</code></td><td>Hide constants from external access</td><td>Encapsulate internals</td></tr>
    <tr><td><code>const_source_location</code></td><td>Find where a constant is defined</td><td>Debug resolution</td></tr>
    <tr><td><code>ancestors</code></td><td>List module hierarchy</td><td>Understand precedence</td></tr>
    <tr><td><code>refinements</code></td><td>Scoped monkey-patching</td><td>Avoid global effects</td></tr>
    <tr><td><code>set_temporary_name</code></td><td>Name anonymous modules</td><td>Improve debugging</td></tr>
    <tr><td><code>module_function</code></td><td>Expose methods as utilities</td><td>Clean APIs</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->
<!-- markdownlint-enable MD033 MD013 -->

### Advanced Examples

```ruby
module MyLib
  def self.const_missing(name)
    # Dynamic constant creation
    const_set(name, "Generated: #{name}")
  end

  autoload :Helper, 'my_lib/helper'

  class Internal; end
  private_constant :Internal

  refine Array do
    def custom_sum
      inject(0, :+)
    end
  end
end

# const_missing in action
MyLib::DynamicConstant  # => "Generated: DynamicConstant"

# Refinement usage
class Processor
  using MyLib
  def process(arr)
    arr.custom_sum
  end
end

# Introspection
MyLib.ancestors  # => [MyLib]
Array.const_source_location('size')  # => file path or nil
```

### Exercises — Advanced Techniques

1. Implement `const_missing` in a module to autoload constants from a hash.
2. Use `set_temporary_name` on an anonymous module and verify with `name`.
3. Create a refinement for String and test it in a scoped
`using` block.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
