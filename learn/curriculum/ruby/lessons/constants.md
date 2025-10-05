# Constants and Configuration Values

Constants capture values that should not change while your Ruby program runs — API endpoints, numeric limits, version strings, configuration toggles. They also serve as anchors for class and module names. This lesson goes beyond `MAX_USERS = 100` and digs into constant lookup rules, namespacing, immutability, and dynamic constant management.

## Learning goals

- Declare and organize constants using idiomatic naming conventions.
- Understand Ruby’s constant lookup path and how namespacing affects reference
  resolution.
- Control mutability: freeze constant data structures and know when reassignment
  is acceptable.

<!-- markdownlint-disable MD013 -->
## Practical Appendix: Constants — Patterns & Tips

(Appendix — constants-appendix-20251005)

Short, practical guidance and small helpers for working with constants safely.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr>
      <th>Concern</th>
      <th>Pattern</th>
      <th>Notes</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Mutable constants</td>
      <td>Deep-freeze at boot</td>
      <td>Prevents runtime mutation and test surprises</td>
    </tr>
    <tr>
      <td>Test overrides</td>
      <td>Temporarily replace then restore</td>
      <td>Always restore in `ensure`</td>
    </tr>
    <tr>
      <td>Namespacing</td>
      <td>Module-level grouping</td>
      <td>Avoid top-level pollution</td>
    </tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Tiny helper: safe_override (for tests)

```ruby
module TestHelpers
  def with_constant(mod, name, value)
    original_defined = mod.const_defined?(name)
    original = mod.const_get(name) if original_defined

    if original_defined
      mod.send(:remove_const, name)
    end

    mod.const_set(name, value)
    yield
  ensure
    if mod.const_defined?(name)
      mod.send(:remove_const, name)
    end
    mod.const_set(name, original) if original_defined
  end
end
```

### Appendix Exercises — constants-appendix-20251005

1. Add a frozen `DEFAULTS` constant and a helper that returns merged runtime
   config; write tests for default and overridden behavior.
2. Implement `with_constant` (or use the provided `safe_override`) to
   temporarily replace a constant in a test and ensure it is restored
   afterwards.

### Tips & Tricks: Constants — Quick Rules

- Use UPPER_SNAKE_CASE and group related values under modules.
- Freeze mutable constant values and use `deep_freeze` for nested structures.
- Avoid embedding secrets in constants; load them securely at boot.
- Prefer configuration objects or environment-based settings over constant
  reassignment.
- Use `private_constant` to hide internals when appropriate.

Constants add clarity when used intentionally. Prefer immutable patterns and
safe test helpers to prevent surprises.

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Constants — Hidden Tips

(Appendix — constants-hidden-20251005b)

A few lesser-known APIs and small helpers that make working with constants safer
and easier.

### Handy APIs

- Module#const_source_location(name) -> [file, lineno] or nil
  - Use this to locate where a constant was defined. It's helpful when debugging
    constant redefinitions or autoload-related surprises.
- Module#autoload(name, filename) and Module#autoload?(name)
  - Defers loading until a constant is first referenced. Prefer explicit loader
    code for clarity. Autoload can reduce startup cost in large projects, but be
    mindful of load-order surprises.
- Module#const_missing(name)
  - Use sparingly. Prefer explicit factories or registries. If used at all, keep
    implementations small and well-tested.
- Module#private_constant(:CONST)
  - Hide implementation constants from the public API surface.

### Tiny helper: deep_freeze

```ruby
def deep_freeze(obj)
  case obj
  when Hash
    obj.each do |k, v|
      deep_freeze(k)
      deep_freeze(v)
    end
  when Array
    obj.each do |v|
      deep_freeze(v)
    end
  end
  obj.freeze
end

# Usage:
DEFAULTS = {
  timeout: 5,
  modes: %i[fast safe]
}
deep_freeze(DEFAULTS)
```

This helper protects nested structures used as constants from accidental
mutation in runtime or tests.

### Pattern: safe test override (reminder)

When tests need to override a constant, temporarily replace it and always
restore the original in an ensure block. Example (shown earlier in this lesson):

```ruby
def with_constant(mod, name, value)
  original_defined = mod.const_defined?(name)
  original = mod.const_get(name) if original_defined
  mod.send(:remove_const, name) if original_defined
  mod.const_set(name, value)
  yield
ensure
  mod.send(:remove_const, name) if mod.const_defined?(name)
  mod.const_set(name, original) if original_defined
end
```

### Quick patterns & cautions

- Prefer `Module#const_get` / `const_set` for dynamic constant access instead of
  evaluating strings into code.
- Avoid defining constants inside blocks (task/loop blocks); their scope can be
  surprising and may leak to the top level.
- Use `private_constant` to reduce accidental API exposure.
- For configuration, prefer a frozen DEFAULTS constant and merge at runtime:
`DEFAULTS.merge(runtime_overrides)`.

<!-- markdownlint-disable MD013 -->
### Appendix Exercises — constants-hidden-20251005b

1. Add a frozen `DEFAULTS` constant that contains nested arrays/hashes. Then
   write a test that attempts to mutate it and asserts a RuntimeError is raised.
2. Write a small script that prints `Module.const_source_location` for a few
   constants in this project (pick one defined in Ruby and one from the core
   library) and observe the difference.
3. Convert a top-level mutable constant into a `private_constant` under a module
   and update callers to use the module's public API instead.

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Constants Advanced Features (Appendix — constants-advanced-20251005)

Explore Ruby's constant introspection and dynamic management for flexible code.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Tip</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr>
      <td><code>const_defined?</code></td>
      <td>Check existence</td>
      <td><code>Module.const_defined?(:PI)</code></td>
    </tr>
    <tr>
      <td><code>const_get</code></td>
      <td>Dynamic access</td>
      <td><code>Math.const_get(:PI)</code></td>
    </tr>
    <tr>
      <td><code>const_set</code></td>
      <td>Dynamic definition</td>
      <td><code>Module.const_set(:MY_CONST, 42)</code></td>
    </tr>
    <tr>
      <td><code>const_missing</code></td>
      <td>Fallback hook</td>
      <td><code>def const_missing(name); end</code></td>
    </tr>
    <tr>
      <td><code>autoload</code></td>
      <td>Lazy loading</td>
      <td><code>autoload :MyClass, 'my_class.rb'</code></td>
    </tr>
    <tr>
      <td><code>const_source_location</code></td>
      <td>Definition location</td>
      <td><code>Math.const_source_location(:PI)</code></td>
    </tr>
    <tr>
      <td><code>constants</code></td>
      <td>List accessible constants</td>
      <td><code>Module.constants.first(5)</code></td>
    </tr>
    <tr>
      <td><code>private_constant</code></td>
      <td>Hide from public</td>
      <td><code>private_constant :INTERNAL</code></td>
    </tr>
    <tr>
      <td><code>remove_const</code></td>
      <td>Remove definition</td>
      <td><code>send(:remove_const, :TEMP)</code></td>
    </tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-enable MD013 -->
### Insider Notes

- Constants are looked up in ancestors; use `inherit` param to control scope.
- `const_missing` is called when a constant isn't found; implement carefully.
- Autoload defers loading; useful for large codebases but can cause load-order
  issues.
- `const_source_location` returns nil for C-defined constants.

### Examples — Constant Introspection

```ruby
# Check and get constants
Math.const_defined?(:PI)  # => true
Math.const_get(:PI)       # => 3.141592653589793

# Set dynamically
module Config
  const_set(:VERSION, "1.0.0")
end
Config::VERSION  # => "1.0.0"

# const_missing hook
class DynamicLoader
  def self.const_missing(name)
    require name.to_s.downcase
    const_get(name)
  end
end

# Autoload example
class LazyLoader
  autoload :HeavyClass, 'heavy.rb'
end
# HeavyClass loaded on first access

# Source location
Math.const_source_location(:PI)  # => nil (C code)
Object.const_source_location(:String)  # => nil

# List constants
Math.constants.include?(:PI)  # => true

# Private constant
module Internal
  SECRET = "hidden"
  private_constant :SECRET
end
Internal::SECRET  # => NameError

# Remove constant
module Temp
  TEMP_VAL = 123
  send(:remove_const, :TEMP_VAL)
end
Temp.const_defined?(:TEMP_VAL)  # => false
```

### Exercises — Advanced Constants

1. Use `const_missing` to implement a simple registry for loading classes
   dynamically.
2. Write a script that lists all constants in a module and their source
   locations.
3. Implement a configuration system using `const_set` and `const_get` for
   dynamic settings.


<!-- Practical Appendix: Reference and further reading -->

### Practical Appendix
This appendix contains brief practical notes and quick references to complement the lesson content. It is intentionally short and safe: no code execution or large data dumps.

- Reference: Official documentation and language core references are excellent further reading sources. Follow the standard docs for authoritative examples.
- Quick tips:
  - Re-run the examples in a REPL to experiment with small changes.
  - Use small, focused test cases when validating behavior.
  - Prefer idiomatic standard-library helpers for clarity and maintainability.

Further reading and sources:
- Official language documentation (search for "official <LANG> docs" where <LANG> is the lesson's language).
- Standard library reference and API pages.
- For curriculum authors: keep examples minimal and include runnable snippets in fenced code blocks.

*End of Practical Appendix.*
