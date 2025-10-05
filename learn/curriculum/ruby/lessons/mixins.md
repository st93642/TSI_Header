# Mixins and Modules

Modules let you bundle behavior and share it across classes without inheriting. By mixing in modules, you can reuse code, compose features, and shape method lookup order. This lesson explores mixin mechanics, `include` vs `extend` vs `prepend`, hooks, and best practices for building maintainable modules.

## Learning goals

- Understand how `include`, `extend`, and `prepend` alter method lookup.
- Encapsulate shared behavior in modules and mix it into classes cleanly.
- Use module callbacks (`included`, `extended`, `prepended`) to configure consumers.
- Combine modules with namespacing and constants.
- Recognize when to use mixins versus inheritance or composition.

## Defining a module

Modules are collections of methods, constants, and classes. They can’t be instantiated.

```ruby
module Skill
  def average_speed
    "20 mph"
  end

  def max_speed
    "25 mph"
  end
end
```

## `include`: add instance methods

`include` mixes module methods into the instance method table of a class.

```ruby
class Runner
  include Skill

  def initialize(name)
    @name = name
  end
end

Runner.new("Ava").average_speed
```

Method lookup order after inclusion: the module sits between the class and its superclass (`Runner.ancestors` shows `[Runner, Skill, Object, Kernel, BasicObject]`). Methods in `Skill` override those up the chain but not methods already defined on `Runner` itself.

## Multiple mixins

You can compose behavior from several modules. The most recently included module has higher precedence.

```ruby
module Swimming
  def move
    "Swimming"
  end
end

module Running
  def move
    "Running"
  end
end

class Triathlete
  include Swimming
  include Running
end

Triathlete.new.move # => "Running" (last included wins)
```

Order matters: include modules in the sequence you want for method lookup.

## `extend`: add class-level behavior

`extend` mixes module methods into a specific object (often the class itself), creating class methods.

```ruby
module Identifiable
  def next_id
    @next_id ||= 0
    @next_id += 1
  end
end

class User
  extend Identifiable
end

User.next_id # => 1
```

Each class that extends the module gets its own copy of module instance variables.

## `prepend`: put module ahead of the class

`prepend` inserts the module before the class in the ancestor chain, letting it wrap or override class methods (useful for decorators).

```ruby
module Instrumentation
  def perform(*args)
    start = Time.now
    result = super
    puts "#{self.class} took #{Time.now - start}s"
    result
  end
end

class Job
  prepend Instrumentation

  def perform
    # work
  end
end
```

`Job.ancestors # => [Instrumentation, Job, Object, ...]`. Use `super` in the module to call the original method.

## Mixins vs inheritance

Ruby supports single inheritance, so modules fill in for multiple inheritance. Use inheritance (`class Sub < Base`) when there’s an “is-a” relationship; use modules when many classes need shared behavior but differ otherwise.

## Module callbacks

Hook into lifecycle events:

- `self.included(base)` — runs when `include` is used.
- `self.extended(base)` — fires when `extend` is called on an object.
- `self.prepended(base)` — triggered on `prepend`.

```ruby
module Cacheable
  def self.included(base)
    base.extend ClassMethods
  end

  module ClassMethods
    def cache_store(store = nil)
      @cache_store = store if store
      @cache_store
    end
  end

  def fetch(key)
    self.class.cache_store.fetch(key)
  end
end

class Article
  include Cacheable
end

Article.cache_store(MemoryStore.new)
Article.new.fetch("foo")
```

This pattern attaches class-level helpers automatically when a module is included. Frameworks like Rails wrap this inside `ActiveSupport::Concern` for cleaner syntax.

## Using `ActiveSupport::Concern`

When available, `ActiveSupport::Concern` simplifies module setup.

```ruby
module SoftDeletable
  extend ActiveSupport::Concern

  included do
    scope :active, -> { where(deleted_at: nil) }
  end

  def soft_delete
    update!(deleted_at: Time.current)
  end
end
```

Concerns manage dependency ordering and provide the `included` block without manual hook wiring.

## Sharing state across mixins

Because modules can’t store instance variables directly accessible to each consumer, use accessor methods or initialize state in a hook.

```ruby
module Trackable
  def self.included(base)
    base.extend ClassMethods
  end

  module ClassMethods
    def tracked_events
      @tracked_events ||= []
    end
  end

  def track(event)
    self.class.tracked_events << event
  end
end
```

Each class gets its own `@tracked_events`; subclasses inherit it unless redefined.

## Namespacing with modules

Modules also group related classes, preventing constant name conflicts and signaling ownership.

```ruby
module Messaging
  class Email; end
  class SMS; end
end

Messaging::Email.new
```

You can mix namespacing and mixins in the same module tree.

## Refinements (advanced)

Refinements provide scoped monkey-patching by wrapping modifications in a module.

```ruby
module StringExtensions
  refine String do
    def shout
      upcase + "!"
    end
  end
end

using StringExtensions
"ruby".shout # => "RUBY!"
```

Refinements are opt-in within the file or scope where `using` is called. They’re useful when you need targeted overrides without global effects.

## Mixins and super

Modules can call `super` to delegate up the chain. Embrace this to create stackable behavior: loggers, instrumentation, caching layers, etc. Ensure methods call `super` only if the next link is expected to respond; otherwise guard with `defined?(super)`.

```ruby
module Retryable
  def perform(*args)
    attempts = 0
    begin
      attempts += 1
      super
    rescue => e
      retry if attempts < 3
      raise e
    end
  end
end
```

## When to avoid mixins

- Module becomes a grab bag of unrelated methods.
- Mixins require too much shared state, leading to fragile coupling.
- Overriding behavior via multiple modules makes method resolution hard to reason about.

Consider composition (objects containing other objects) or service objects when mixins become unwieldy.

## Guided practice

1. **Auditable concern**
   - Build a `Auditable` module that adds `created_at`/`updated_at` timestamps and a class-level `before_save` hook via `included`.

2. **Instrumented jobs**
   - Implement `Instrumentation` module using `prepend` to time job execution. Ensure it calls `super` and logs duration.

3. **Dual interface module**
   - Write a module that, when included, adds instance methods and when extended, adds class methods (`self.included` and `self.extended`). Demonstrate both behaviors.

4. **Refinement sandbox**
   - Create a refinement that adds `String#camelize`. Use it inside a specific class without affecting global `String` behavior.

5. **Mixin order exploration**
   - Include multiple modules defining the same method. Use `ancestors` to inspect resolution order and adjust inclusion order to change behavior.

## Self-check questions

1. How does `include` change a class’s ancestor chain compared to `prepend`?
2. When would you prefer a mixin over subclassing?
3. What role do module callbacks like `included` and `extended` play when building reusable concerns?
4. How does `extend` differ when called on an object versus within `class << self`?
5. What problems can arise from mixing in too many modules, and how can you mitigate them?

Mixins provide Ruby’s version of multiple inheritance. Use them to compose behavior across unrelated classes, but keep modules focused, explicit, and well-documented so your ancestor chains remain understandable.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Mixins — Include vs Extend, Conflicts & Tests (Appendix — mixins-ruby2)

Patterns for using modules as mixins: when to `include` vs `extend`, how to resolve method conflicts, and testing strategies.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Use</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>include</td><td>Instance methods</td><td>Mix behaviour into instances</td></tr>
    <tr><td>extend</td><td>Class methods</td><td>Add methods to the singleton class</td></tr>
    <tr><td>Conflicts</td><td>Aliasing</td><td>Use `alias_method` or explicit module prepending</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example

```ruby
module Logging
  def log(msg); puts msg; end
end

class Service
  include Logging
end
```

### Exercises (Appendix — mixins-ruby2)

1. Create a module that provides logging and include it into a class; test that instances respond to `log`.
2. Demonstrate method conflicts when including multiple modules and resolve using `Module#prepend` or aliasing; add tests.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Mixins — `prepend`, Refinements & Namespacing (Appendix — mixins-ruby3)

Advanced mixin patterns: use `prepend` to change method lookup, consider refinements for scoped monkey-patching, and keep module namespacing clean.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>prepend</td><td>Override instance methods</td><td>Prepend modules to intercept calls</td></tr>
    <tr><td>refinements</td><td>Scoped monkey-patch</td><td>Use sparingly; limited to specific files/scopes</td></tr>
    <tr><td>namespacing</td><td>Avoid top-level leakage</td><td>Prefer `MyGem::Utils` over global modules</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Appendix — mixins-ruby3)

1. Demonstrate `prepend` to add logging around an existing instance method and test that the original method is still called.
2. Use a refinement to add a helper method only in a test file and assert it doesn't leak globally.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
