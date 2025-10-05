# # Symbols

Symbols are immutable identifiers—lightweight labels Ruby uses for method names, hash keys, state machines, and internal bookkeeping. Because each symbol is unique and stored once, they’re faster to compare and cheaper to reuse than strings. Knowing when to reach for symbols keeps APIs consistent and memory usage predictable.

Symbols are immutable identifiers—lightweight labels Ruby uses for method names, hash keys, state machines, and internal bookkeeping. Because each symbol is unique and stored once, they’re faster to compare and cheaper to reuse than strings. Knowing when to reach for symbols keeps APIs consistent and memory usage predictable.

## Learning goals

- Recognize how symbols differ from strings in identity, mutability, and garbage collection.
- Choose symbols for stable identifiers (hash keys, enum-like states) while keeping mutable text as strings.
- Convert safely between strings and symbols without leaking memory or exposing security risks.
- Use symbols in metaprogramming (dynamic method dispatch, reflection) and configuration objects.
- Audit symbol-heavy code for performance pitfalls, especially when dealing with untrusted input.

## Creating and inspecting symbols

```ruby
:name
:"symbol with spaces"
:'quoted'
:"interpolated_#{id}" # evaluated like double-quoted strings

:name.class        # => Symbol
:name.object_id    # same every time in a process
```

Each distinct symbol exists once per Ruby process. Calling `:hello.object_id` repeatedly returns the same identifier; strings don’t behave that way.

## Symbols vs. strings

<!-- markdownlint-disable MD033 MD013 -->
<table>
  <thead>
    <tr><th>Aspect</th><th>Symbol</th><th>String</th></tr>
  </thead>
  <tbody>
    <tr><td>Mutability</td><td>Immutable, frozen</td><td>Mutable by default</td></tr>
    <tr><td>Identity</td><td>Unique per name (<code>:foo</code> is single)</td><td>New object per literal unless frozen</td></tr>
    <tr><td>Garbage collection</td><td>Ruby 2.2+ collects dynamic symbols</td><td>Always GC'd</td></tr>
    <tr><td>Typical usage</td><td>Keys, identifiers</td><td>User-visible text, mutable content</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD013 -->

Comparing symbols is pointer equality (`:foo == :foo` is instant). Comparing strings walks characters unless both are frozen and reused.

## Hash keys and configuration

Symbols shine as hash keys when the set of keys is fixed.

```ruby
config = {
  host: "localhost",
  port: 3000,
  features: { dark_mode: true }
}

config[:host]         # => "localhost"
config.fetch(:port)   # => 3000
```

Symbols keep APIs consistent and avoid typos; Ruby will raise on unknown keys with `fetch`.

## Method references and metaprogramming

```ruby
object.send(:save)
user.respond_to?(:update?)
public_send(:authenticate!, token)

class Person
  attr_accessor :name
end
```

Many core APIs accept symbols to reference methods (`send`, `public_send`, `method`, `define_method`). Because the interpreter already tracks method names as symbols, you get efficient lookup.

## Enumerations and state machines

```ruby
class Order
  STATES = %i[pending approved shipped cancelled].freeze

  def initialize
    @state = :pending
  end

  def transition_to(new_state)
    raise ArgumentError unless STATES.include?(new_state)
    @state = new_state
  end

  def approved? = @state == :approved
end
```

Symbols serve as lightweight enum values; compare them directly without needing
extra objects.

## Conversions

```ruby
:status.to_s          # => "status"
"status".to_sym       # => :status
"status".intern       # => :status (alias)

"status".to_sym.frozen? # => true
```

Converting user input to symbols can leak memory if the set of inputs is unbounded. Ruby 2.7+ garbage collects symbols created with `to_sym`, but older versions do not. Even with GC, avoid symbolizing attacker-controlled strings unless you whitelist them first.

```ruby
ALLOWED = %i[name email phone].freeze
key = params[:sort].to_sym
raise "Invalid" unless ALLOWED.include?(key)
```

## Symbol literals from strings

Double-quoted symbols support interpolation, but they’re created only once per
file load.

```ruby
suffix = "name"
field = :"user_#{suffix}"   # => :user_name
```

Dynamic combinations at runtime still produce interned symbols—mind the memory
cost.

## Symbol methods

Symbols offer a subset of string-like behavior, returning new symbols where
possible or strings when necessary.

```ruby
:hello.upcase         # => :HELLO
:hello.capitalize     # => :Hello
:hello.length         # => 5
:hello.to_s.reverse   # => "olleh"
```

In Ruby 3+, many of these return symbols; earlier versions may return strings.
Check your target Ruby version when relying on return types.

## Reflection helpers

```ruby
Symbol.all_symbols.size     # total symbols currently interned
Symbol.all_symbols.grep(/^set_/)
```

`Symbol.all_symbols` aids debugging but should stay out of hot code paths (it creates a new array each call).

## Performance considerations

- Reusing symbols avoids allocations and speeds comparisons.
- Symbols in large hashes reduce memory overhead versus strings for repeated
  keys.
- Creating symbols dynamically inside tight loops can still be costly—cache the
  symbol or use strings when the identifier set is huge or user-driven.

## Safety tips

- **Don’t symbolize arbitrary input.** Validate or whitelist first.
- **Freeze constant hashes.** Prevent accidental mutation: `SETTINGS =
  {...}.freeze`.
- **Prefer strings for mutable text.** Error messages, UI copy, translations,
  user content—all should stay as strings.
- **Log responsibly.** Converting sensitive data to symbols hides content but
  doesn’t protect memory access. Use strings for secrets and clear them if
  needed.

## Guided practice

1. **Enum guard**
   - Implement `status_from(string)` that maps user input to one of `:pending`,
     `:approved`, `:rejected`.
   - Reject unexpected values without creating new symbols.

2. **Dispatch table**
   - Build a hash mapping symbols to lambdas (`:add`, `:subtract`, etc.).
   - Write `calculate(operation, a, b)` that looks up the handler safely and
     raises for unsupported operations.

3. **Symbol cache**
   - Benchmark converting the same string to a symbol 100,000 times versus
     reusing a memoized symbol.
   - Print timing differences using `Benchmark.realtime`.

4. **Metaprogrammed accessors**
   - Given an array of field names, dynamically define reader methods using
     `define_method` and symbols.
   - Ensure method names are sanitized before defining them.

5. **Configuration loader**
   - Parse a YAML file into a hash.
   - Recursively symbolize keys only if they exist in a whitelist per nesting
     level.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Symbols — Safe Usage and Patterns

This appendix gives safe usage patterns for symbols, conversion helpers, and a brief HTML table showing common pitfalls.

```ruby
ALLOWED_KEYS = %i[name email phone].freeze

def safe_to_sym(s)
  k = s.to_sym
  raise ArgumentError unless ALLOWED_KEYS.include?(k)
  k
end
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Operation</th><th>Symbol</th><th>String</th></tr>
  </thead>
  <tbody>
    <tr><td>Key reuse</td><td>Efficient</td><td>Allocating</td></tr>
    <tr><td>Mutable data</td><td>Not suitable</td><td>Preferred</td></tr>
  </tbody>
</table>
```

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Symbols — Conversion Utilities & Whitelisting (Appendix — symbols-ruby-utils)

Small helpers for safely converting user input to symbols and for whitelisting candidate keys.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Utility</th><th>Purpose</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr><td>safe_to_sym</td><td>Whitelist inputs</td><td>See below</td></tr>
    <tr><td>symbolize_hash</td><td>Convert known keys</td><td>Recursive safe mapping</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

```ruby
ALLOWED = %i[name email phone].freeze

def safe_to_sym(s) k = s.to_sym raise ArgumentError, 'disallowed key' unless
ALLOWED.include?(k) k end

def symbolize_known_keys(hash) hash.each_with_object({}) do |(k, v), out| sk =
k.to_s out[sk.to_sym] = v if ALLOWED.include?(sk.to_sym) end end
```

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
<!-- markdownlint-enable MD033 -->

### Exercises

1. Implement `safe_to_sym` and test it rejects unexpected keys.
2. Benchmark symbolization of repeated keys vs reused symbols.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tip</th><th>Why</th><th>Quick fix</th></tr>
  </thead>
  <tbody>
    <tr><td>Avoid symbolizing raw params</td><td>Can increase symbol table churn</td><td>Whitelist keys or map known headers</td></tr>
    <tr><td>Cache common conversions</td><td>Reduces repeated to_sym cost</td><td>Memoize mapping in a constant</td></tr>
    <tr><td>Prefer strings for user text</td><td>Symbols are not for translatable content</td><td>Use strings and freeze when needed</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Tiny helper: memoized_symbol

```ruby
SYMBOL_CACHE = {} def memoized_symbol(s) SYMBOL_CACHE[s] ||= s.to_sym end
```

### Exercises — symbols-hidden-20251005b

1. Replace scattered `to_sym` calls in a tiny parser with `memoized_symbol` and benchmark the change.
2. Audit a small controller to remove any `to_sym` on user-supplied input; add tests that ensure unexpected keys raise.

<!-- Additional Practical Appendix: Symbols — Hidden Tips (Appendix — symbols-hidden-20251005b) -->

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Tip</th><th>Why</th><th>Quick fix</th></tr>
  </thead>
  <tbody>
    <tr><td>Avoid symbolizing raw params</td><td>Can increase symbol table churn</td><td>Whitelist keys or map known headers</td></tr>
    <tr><td>Cache common conversions</td><td>Reduces repeated to_sym cost</td><td>Memoize mapping in a constant</td></tr>
    <tr><td>Prefer strings for user text</td><td>Symbols are not for translatable content</td><td>Use strings and freeze when needed</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Tiny helper: memoized_symbol_hidden

```ruby
SYMBOL_CACHE = {} def memoized_symbol_hidden(s) SYMBOL_CACHE[s] ||= s.to_sym end
```

### Exercises — symbols-hidden-20251005b (continued)

1. Replace scattered `to_sym` calls in a tiny parser with `memoized_symbol` and benchmark the change.
2. Audit a small controller to remove any `to_sym` on user-supplied input; add tests that ensure unexpected keys raise.

## Self-check questions

1. Why are symbols faster to compare than strings, and how does Ruby ensure their uniqueness?
2. When is it appropriate to convert strings to symbols, and what safeguards should you implement beforehand?
3. How do symbols enable metaprogramming features like `send`, `respond_to?`, and `define_method`?
4. What changed about symbol garbage collection in Ruby 2.2 and later, and why does it matter for long-running processes?
5. In what scenarios would strings still be the better choice over symbols, even for keys or identifiers?

Symbols are Ruby’s efficient labels—perfect for stable identifiers and internal wiring. Use them to make intent clear and performance brisk, but keep user-driven or mutable text as strings so your programs remain safe and flexible.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: External Tools & Examples (Appendix — External Tools — symbols-ruby)

Notes on Ruby symbols, performance trade-offs vs strings, and quick examples. Links point to Ruby docs.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concept</th><th>Why</th><th>Reference</th></tr>
  </thead>
  <tbody>
    <tr><td>Symbols</td><td>Immutable identifiers with lower allocation overhead</td><td><a href="https://ruby-doc.org/core/Symbol.html">Symbol docs</a></td></tr>
    <tr><td>Strings vs Symbols</td><td>Choose based on mutability & memory use</td><td>Prefer strings for user data</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (symbols-ruby)

1. Replace a hash keyed by strings with one keyed by symbols and benchmark access time for a large dataset.
2. Document why interned symbols can be a memory leak risk in long-running processes when generated from untrusted input.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Symbols — Safe Usage (Appendix — symbols-ruby2)

Guidelines for safe symbol use, conversion helpers, and testing strategies to avoid symbol-related memory issues.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Risk</th><th>Mitigation</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr><td>Symbolizing user input</td><td>Whitelist keys</td><td>Use ALLOWED_KEYS.include?(key.to_sym)</td></tr>
    <tr><td>Dynamic symbols</td><td>Cache or avoid creating repeatedly</td><td>Memoize symbol conversions</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Safe converter

```ruby
ALLOWED = %i[name email phone].freeze

def safe_to_sym(s) k = s.to_sym raise ArgumentError unless ALLOWED.include?(k) k
end
```

### Exercises (Appendix — symbols-ruby2)

1. Implement `safe_to_sym` and test it rejects unexpected keys.
2. Benchmark repeated `to_sym` on the same string vs memoized symbol usage.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Symbols — Interning, Memory & Use Cases (Appendix — symbols-ruby2)

Notes on when to use symbols vs strings, memory implications, and safe patterns for dynamic symbol creation.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Use</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Identifiers</td><td>Method or key names</td><td>Good as hash keys for fixed sets</td></tr>
    <tr><td>Dynamic symbols</td><td>Rare</td><td>Avoid creating from user input to prevent memory growth</td></tr>
    <tr><td>String vs Symbol</td><td>Text data</td><td>Prefer String for user-visible text</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example

```ruby
h = { foo: 1, bar: 2 } h[:foo] # => 1

# avoid:
user_input = 'danger' sym = user_input.to_sym # may bloat symbol table
```

### Testing symbol behavior

- Test that API accepts either string or symbol keys if you support both (`h.fetch('k') || h.fetch(:k)` patterns or normalize keys).

```ruby
require 'minitest/autorun'

class TestSymbols < Minitest::Test def test_hash_access h = { 'a' => 1 } assert_equal 1, h['a'] end end
```

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

### Exercises (Appendix — symbols-ruby2-unique)

1. Write a helper that normalizes hash keys to symbols safely (without converting unknown large user inputs), and test with mixed key types.
2. Benchmark lookups on large hashes keyed by strings vs symbols and report any performance differences.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Symbols — Internals & Safe Usage (Appendix — symbols-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Aspect</th><th>Behavior</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr><td>Immutability</td><td>Symbols are immutable and reused</td><td>Good keys, avoid dynamic creation from untrusted input</td></tr>
    <tr><td>Conversion</td><td>`to_sym` / `to_s`</td><td>Prefer strings from input, symbolize after validation</td></tr>
    <tr><td>Memory</td><td>Symbols persist</td><td>Don't `to_sym` user input repeatedly</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```ruby
h = { name: 'Alice' } key = :name h[key]

# safe conversion
sym = str.to_sym if %w[allowed keys].include?(str)
```

### Exercises

1. Audit code to find places that call `to_sym` on user input and add validation.
2. Write tests that show symbol vs string lookup differences and conversion safely.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
