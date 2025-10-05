# Variables and Data Types

Output is exciting, but programs become powerful when they remember information. This lesson unpacks how Ruby handles values, how dynamic typing works, and how to pick the right representation for the job. By the time you finish, variables, constants, and core data types will feel like second nature.

## Learning goals

- Understand how Ruby binds names to objects using assignment, and how scope affects visibility.
- Explore the main Ruby value classes—strings, numbers, symbols, booleans, arrays, hashes, ranges, and `nil`.
- Convert between types safely, watching for edge cases with user input or external data.
- Apply naming conventions that make code self-documenting and maintainable.
- Guard against mutation surprises when multiple variables reference the same object.

## Variables in Ruby: labels pointing to objects

In Ruby, variables are references to objects. When you assign `name = "Alice"`, the variable `name` points to a String object stored on the heap. Reassigning the variable simply points it to a new object; the old object sticks around until no references remain and the garbage collector reclaims it.

```ruby
name = "Alice"
puts name.object_id

name = "Bob"
puts name.object_id # different object; reassignment doesn't mutate the original
```

Ruby's dynamic typing means variables do not have fixed types. Each object carries its own class information, so the same variable can hold integers, strings, or arrays throughout a program. While flexibility is great, descriptive naming and predictable transformations keep code understandable.

## Naming conventions and style

- Use snake_case for local variables and methods: `total_points`, `user_name`.
- Avoid single-letter names unless inside a tight loop (e.g., `i` in `each_with_index`).
- Leading underscores signal "intentionally unused" variables (`_unused`), often in pattern matching or destructuring assignments.
- Ruby is case-sensitive, so `count` and `Count` refer to different identifiers.

```ruby
first_name = "Ada"
total_hours_worked = 37.5
_unused_placeholder = "ignored"
```

## Local scope essentials

Variables defined inside a method or block are local to that scope. If you assign to a variable inside a block without prior assignment outside, Ruby treats it as a new, block-local variable.

```ruby
value = "outside"

3.times do |i|
  value = "iteration #{i}" # reuses the existing local variable
end

puts value # => "iteration 2"
```

Inside methods, local variables vanish once the method returns. Use instance variables (`@name`), class variables (`@@total`), or constants when you need broader access—later lessons dive deeper into those.

## A tour of core value types

### Strings

- Created with single or double quotes; double quotes enable interpolation and escape sequences.
- Use `%Q{...}` for double-quoted strings without escaping quotes, `%q{...}` for single-quoted behavior.
- Immutable by default when `# frozen_string_literal: true` is used, otherwise mutable.

```ruby
title = "Ruby 101"
subtitle = %Q{Beginner's Guide}
escaped = "Line 1\nLine 2"
```

### Symbols

Symbols (e.g., `:status`) are immutable identifiers often used as hash keys or method names. They are lightweight and unique—Ruby creates only one copy of a given symbol.

```ruby
status = :pending
action = :save!
```

### Numbers: Integers, Floats, Rational, Complex, BigDecimal

- Integers (`Integer`) scale automatically to arbitrary precision—no overflow surprises.
- Floats (`Float`) follow IEEE 754 double-precision; expect rounding errors.
- Use `Rational(1, 3)` for exact fractions and `Complex(1, 2)` for complex numbers.
- `BigDecimal` (from the standard library) helps with precise financial calculations.

```ruby
count = 42
price = 19.99
exact_third = Rational(1, 3)
precise_total = BigDecimal("12.345") * 3
```

Require `bigdecimal` for the last example.

### Booleans and truthiness

Ruby has two singleton booleans: `true` (`TrueClass`) and `false` (`FalseClass`). In conditionals, everything except `nil` and `false` evaluates as truthy. Even `0` and empty strings count as true!

```ruby
puts "truthy" if "" # prints despite empty string
puts "falsey" if nil  # never prints
```

### Nil: the "no value" object

`nil` is Ruby's null object. Use it to signal missing data or optional values. Methods like `nil?`, `&.` (safe navigation), and `||=` help you work with nil gracefully.

```ruby
nickname = nil
puts nickname || "Guest"       # => "Guest"

person = nil
puts person&.name               # safe navigation prevents NoMethodError
```

### Arrays and hashes (quick preview)

Even though dedicated lessons cover collections, they're worth mentioning here because variables frequently hold them.

```ruby
numbers = [1, 2, 3]
profile = { name: "Ada", role: "Engineer" }
```

### Ranges

Ranges express sequences and are commonly used for iteration or validation.

```ruby
inclusive = 1..5  # includes 5
exclusive = 1...5 # excludes 5

puts inclusive.include?(5) # true
```

## Inspecting types and object ancestry

Use `.class` to inspect the runtime class. Chain `.ancestors` on the class to see inherited modules.

```ruby
value = [1, 2, 3]
puts value.class            # => Array
puts value.class.ancestors  # => [Array, Enumerable, Object, Kernel, BasicObject]
```

`obj.is_a?(ClassName)` checks whether an object descends from a particular class or module.

## Type conversion and coercion

Ruby provides `to_s`, `to_i`, `to_f`, `to_sym`, `to_a`, and more for explicit conversions. Watch out for edge cases: converting invalid strings yields `0` with `to_i` and `0.0` with `to_f`.

```ruby
"15".to_i       # 15
"hello".to_i    # 0 (careful!)
"19.99".to_f    # 19.99
42.to_s         # "42"
"total".to_sym  # :total
```

For safer integer parsing, use `Integer(string, exception: false)` which returns `nil` on failure.

```ruby
Integer("42")                 # 42
Integer("not-a-number", exception: false) # nil
```

## Mutation vs. reassignment

Reassignment points a variable to a new object. Mutation changes the object in place. Two variables referencing the same object will both observe mutations.

```ruby
greeting = "Hello"
alias_ref = greeting

greeting << ", world" # mutates the original string
puts alias_ref         # => "Hello, world"

greeting = greeting + "!" # creates a new string
puts alias_ref             # still "Hello, world"
```

Freeze an object with `freeze` (or use the magic comment `# frozen_string_literal: true`) to guard against accidental mutation.

## Multiple assignment and parallel destructuring

Ruby destructures arrays on the left-hand side, enabling concise swaps and unpacking.

```ruby
first, second = ["Ada", "Grace"]
x, y = y, x # classic swap

name, age, city = "Alice", 25, "Riga"
```

Combine with the splat operator to capture remaining values.

```ruby
head, *tail = [1, 2, 3, 4]
# head => 1, tail => [2, 3, 4]
```

Hash destructuring (Ruby 2.7+) lets you extract values by key:

```ruby
user = { name: "Ada", country: "LV", verified: true }
{ name:, country: } = user
```

## Constants and the object model

Constants start with an uppercase letter. Ruby allows reassignment but prints a warning. Keeping constants immutable (freeze them or use immutable data structures) prevents bugs.

```ruby
APP_NAME = "TSI Toolkit".freeze
MAX_RETRIES = 3

module Config
  TIMEOUT = 5
end
```

Constants live within modules/classes. Access nested constants with `::`, e.g., `Config::TIMEOUT`.

### Global, instance, and class variables (preview)

- Global variables start with `$` (`$stdout`)—use sparingly.
- Instance variables belong to objects (`@balance`).
- Class variables (`@@count`) are shared across a class hierarchy. Later lessons explore them fully.

## String interpolation power tips

- Interpolation works inside double-quoted strings and heredocs.
- Wrap complex expressions in `{}`.
- Use `format` or `sprintf` for complex numeric alignment.

```ruby
name = "Ada"
age = 28
puts "#{name} will be #{age + 5} in five years."
```

`format("%.2f", value)` rounds floats without mutating them.

## Practical patterns and best practices

- Initialize variables close to first use to clarify intent.
- Avoid implicit `nil` by providing defaults (`count ||= 0`).
- Use `fetch` with hashes to supply fallback values or raise helpful errors.
- Lean on symbols for identifiers, but use strings when data needs user-friendly formatting.

## Guided practice

1. **Profile builder**
   - Create variables for name, age, city, languages (array), and primary_skill (symbol).
   - Interpolate them into a multi-line introduction using a heredoc.
   - Print the `.class` of each variable afterward.

2. **Type conversion clinic**
   - Prompt for an item price as text (e.g., `"19.95"`).
   - Convert it to `BigDecimal` for precise arithmetic.
   - Multiply by a quantity and format the result with two decimals.

3. **Mutation detective**
   - Assign the same array to two variables.
   - Mutate through one reference (`<<`, `push`, or `map!`).
   - Print both variables to observe shared state, then repeat using `dup` to create a copy before mutating.

4. **Constant catalog**
   - Define a module `Limits` with constants representing rate caps, bonus multipliers, and timeouts.
   - Freeze any arrays or hashes stored in constants to prevent accidental changes.

5. **Destructuring challenge**
   - Given `response = { status: 200, body: "ok", headers: { content_type: "text/plain" } }`, extract `status` and `content_type` into local variables via destructuring.

## Self-check questions

1. How does Ruby's object model handle assignment, and why does that matter for mutable objects?
2. Which values evaluate to false in conditionals, and how does that differ from other languages you may know?
3. When should you reach for `BigDecimal` or `Rational` instead of floats?
4. What dangers arise from using `to_i` on user input, and how can `Integer(input, exception: false)` help?
5. How can you prevent constants that reference collections from being modified elsewhere in your code?

## Next steps

Experiment liberally—create variables, mutate them, inspect object IDs, and practice destructuring. Mastery of variables and types makes the rest of Ruby's features click, whether you are parsing API payloads or modeling business logic. Next up, we zoom in on strings and text manipulation, expanding your toolkit for working with human-readable data.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Variables & Types — Numeric Promotion & Safe Defaults (Appendix — variables_types-ruby2)

Notes on how Ruby treats numeric operations, common pitfalls, and patterns for safe default values.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Numeric promotion</td><td>Integers and Floats</td><td>Watch division: `1/2` => 0 in older Ruby; use `1.0/2` or `to_f`</td></tr>
    <tr><td>Default values</td><td>nil sentinel</td><td>Use `opts.fetch(:k, default)` to avoid accidental mutation</td></tr>
    <tr><td>Immutability</td><td>freeze constants</td><td>Freeze arrays/hashes to signal intent</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```ruby
# division
puts 1.fdiv(2) # => 0.5

# safe defaults
def configure(opts = {})
  opts = { retry: 3 }.merge(opts)
end
```

### Exercises (Appendix — variables_types-ruby2)

1. Demonstrate integer division vs float division and write tests covering both.
2. Implement a config loader that uses frozen default objects and write tests ensuring defaults are not mutated.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Variables & Types — Scope, Duck Typing & Conventions (Appendix — variables_types-ruby-appendix-20251005)

Practical notes on choosing variable scope, understanding Ruby's type system, and when to document expected types.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Var type</th><th>Prefix</th><th>When to use</th></tr>
  </thead>
  <tbody>
    <tr><td>Local</td><td>none</td><td>Method-scope locals</td></tr>
    <tr><td>Instance</td><td>@</td><td>Per-object state</td></tr>
    <tr><td>Class</td><td>@@</td><td>Shared across class hierarchy (use sparingly)</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: duck typing

```ruby
def process(collection)
  collection.each { |x| puts x }
end
```

### Exercises (Appendix — variables_types-ruby-appendix-20251005)

1. Convert a class using class variables to one using class instance variables and add tests.
2. Document expected types for public methods and add simple runtime assertions.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
