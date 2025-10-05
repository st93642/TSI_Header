# Boolean Logic and Control Flow Foundations

Boolean logic is the decision-making engine of every Ruby program. Mastering truthiness, comparisons, and logical operators lets you build clear conditionals, guard against edge cases, and express complex rules succinctly. This lesson extends far beyond `true` and `false`, exploring how Ruby evaluates expressions and how you can harness boolean logic to guide control flow.

## Learning goals

- Understand how Ruby represents booleans and truthy/falsy values.
- Build predictable conditionals using comparison operators and logical
  composition.
- Apply guard clauses, safe navigation, and pattern matching to simplify
  branching.
- Distinguish between `&&/||` and `and/or`, recognizing precedence differences.
- Practice real-world boolean patterns used in authentication, validation, and
  configuration.

## Boolean values and truthiness

Ruby provides two singleton objects, `true` (`TrueClass`) and `false` (`FalseClass`). In conditional contexts, only `false` and `nil` are falsy—everything else counts as truthy, including `0`, empty strings, and empty arrays.

```ruby
!!false # => false
!!nil   # => false
!!0     # => true
!!""    # => true
!![]    # => true
```

Double negation (`!!`) is a common trick to coerce any value into a boolean explicitly.

## Comparison operators

Comparisons return booleans and form the backbone of branching logic.

```ruby
5 == 5    # equality => true
5 != 6    # inequality => true
10 > 5    # greater than => true
10 >= 10  # greater or equal => true
3 < 4     # less than => true
3 <= 2    # => false

"TSI" == "tsi"   # => false (case sensitive)
"10" == 10       # => false (different types)
```

`<=>` (spaceship operator) returns -1, 0, or 1 and powers sorting; it’s not boolean but often used inside boolean expressions.

## Logical operators and short-circuiting

### `&&` (logical AND)

Evaluates the left operand first. If it’s falsy, Ruby skips the right operand
entirely (short-circuiting) and returns the falsy value. Otherwise it returns
the right operand.

```ruby
user = find_user(id)
if user && user.active?
  puts "Welcome back!"
end
```

Short-circuiting prevents `NoMethodError` by avoiding `user.active?` when `user` is `nil`.

### `||` (logical OR)

Returns the first truthy operand; short-circuits if the left side is truthy.

```ruby
display_name = user.nickname || user.full_name || "Guest"
```

### `!` (logical NOT)

Flips truthiness.

```ruby
puts !true   # false
puts !false  # true

def maintenance_mode?
  !ENV["MAINTENANCE"].nil?
end
```

### `and` / `or`

Ruby also includes `and`/`or`, but they have lower precedence than `&&/||`. Use them sparingly—mainly for control flow, not expressions.

```ruby
logged_in = authenticate(user) and authorize(user)
# Equivalent to: (logged_in = authenticate(user)) and authorize(user)
```

Because of precedence quirks, prefer `&&/||` for most boolean expression work.

## Parentheses and readability

Use parentheses to clarify complex expressions.

```ruby
eligible = (age >= 18 && country == "LV") || guardian_signed?
```

Breaking logic across lines with descriptive helper methods often beats writing
giant expressions.

## Guard clauses and early returns

Guard clauses validate assumptions up front and return early when conditions
fail, keeping the happy path clear.

```ruby
def ship_order(order)
  return "Missing items" if order.items.empty?
  return "Account suspended" unless order.account.active?

  process_shipment(order)
end
```

## Safe navigation (`&.`) and presence checks

Avoid `NoMethodError` by using `&.` when accessing methods on potentially nil objects.

```ruby
country = user&.address&.country
if country&.upcase == "LV"
  puts "Local perks unlocked!"
end
```

Pair with `||` or `||=` to supply defaults.

```ruby
session[:token] ||= SecureRandom.hex(16)
```

## Pattern matching and boolean expressions (Ruby 2.7+)

Pattern matching uses `case` with `in` to destructure and test data. Combined with guards (`if/unless`), it expresses complex logic readably.

```ruby
case payload
in { status: 200, body:, error: nil }
  puts "Success: #{body}"
in { status:, error: }
  puts "Error (#{status}): #{error}"
else
  puts "Unexpected response"
end
```

Guards can further filter patterns (`in { status: 200 } if body&.any?`).

## Boolean-friendly method naming

Ruby conventions name predicate methods with a trailing question mark: `empty?`, `valid?`, `admin?`. Implementing these on your classes makes boolean checks self-explanatory.

```ruby
class FeatureFlag
  def initialize(enabled)
    @enabled = enabled
  end

  def enabled?
    !!@enabled
  end
end
```

## Combining conditions with `case` and `if` modifiers

`case` is great for matching against multiple conditions:

```ruby
case
when score >= 90
  grade = "A"
when score >= 80
  grade = "B"
else
  grade = "C"
end
```

Use suffix modifiers for concise single-line conditions:

```ruby
puts "Check your email" if notifications_enabled?
warn "Low disk space" unless free_space_mb > 500
```

## `&&=` and `||=` shorthands

Shorthands combine boolean logic with assignment. `||=` assigns only if the variable is falsy; `&&=` assigns only if the variable is truthy.

```ruby
config[:timezone] ||= "UTC"
token &&= refresh_token(token)
```

## Truthy/falsy in control flow

Because everything except `false` and `nil` is truthy, you can rely on arrays, hashes, and numbers directly.

```ruby
items = fetch_items
if items
  puts "Loaded #{items.size} items"
else
  puts "No items found"
end
```

To treat empty collections as falsy, combine with `empty?` or `any?`.

```ruby
if items&.any?
  puts "We have #{items.size} items"
else
  puts "Empty inventory"
end
```

## Common pitfalls

- **Assignment vs equality**: `=` assigns; `==` compares. Write conditionals as
  `if (value = compute)` intentionally, with parentheses to signal assignment.
- **Truthy strings**: `"false"` is truthy because it’s a non-empty string—coerce
  user input explicitly when parsing booleans.
- **Precedence surprises**: `a && b || c` groups as `(a && b) || c`. Add
  parentheses for clarity.
- **Skipping explicit nil checks**: Methods like `empty?` will raise on `nil`.
  Use safe navigation or guard against `nil` first.

## Guided practice

1. **Eligibility rules**
   - Write a method `eligible_for_discount?(user)` that returns true when the
     user has at least one of these: loyalty tier "gold" or "platinum", or an
     active coupon that hasn’t expired.
   - Use guard clauses for nil checks (`user.coupon&.expired?`).

2. **Feature rollout**
   - Combine environment variables and runtime checks to compute a boolean flag
     `feature_enabled`.
   - Enable when `ENV["FEATURE"] == "on"` or the current user is in a beta
     testers list.

3. **Safe dig**
   - Given a nested hash `settings`, write `timezone_for(settings)` returning
     the timezone or "UTC" if any intermediate key is missing.
   - Use safe navigation (`&.`) or `dig` with boolean fallbacks.

4. **Input sanitization**
   - Parse user input strings like "yes", "no", "true", "0" into booleans using
     a method `truthy?(value)`.
   - Treat unknown inputs as `false` and document the conversion table.

5. **Alarm system**
   - Implement logic that triggers an alarm when either a door sensor or window
     sensor is open and the system is armed.
   - Add a bypass flag that, when true, suppresses the alarm even if sensors are
     tripped.

## Self-check questions

1. Why do `&&`/`||` return the original operands instead of strict booleans, and
   how can that behavior be useful?
2. How does short-circuit evaluation prevent runtime errors when chaining method
   calls?
3. When should you use `and`/`or` instead of `&&/||`?
4. How do you treat empty collections as falsy without breaking on `nil` values?
5. What naming conventions help signal that a method returns a boolean?

Strong boolean logic unlocks robust conditionals, readable code, and fewer
surprises. Keep experimenting with combinations, guard clauses, and expressive
predicate methods—you’ll quickly find your Ruby code becoming clearer and more
reliable.

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Boolean Logic & Truthiness (Appendix — boolean_logic-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concept</th><th>What</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr><td>Truthiness</td><td>Only `false` and `nil` are falsy</td><td>Prefer explicit checks when `nil` and `false` have different meanings</td></tr>
    <tr><td>Short-circuit</td><td>`&&` / `||`</td><td>Use to guard dangerous calls: `obj && obj.method`</td></tr>
    <tr><td>Safe navigation</td><td>`&.`</td><td>Use `&.` to avoid `nil` checks but avoid overuse in core logic</td></tr>
    <tr><td>Double-negation</td><td>`!!`</td><td>Coerce to boolean when returning predicates from methods</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```ruby
# Only nil and false are falsy
puts !!0    # => true
puts !!""  # => true

# Guarding
user && user.profile&.email

# Predicate methods should return true/false
def admin?
  !!@role && @role == :admin
end
```

<!-- markdownlint-disable MD013 -->
### Appendix — Exercises

1. Write a predicate `present?` for a small wrapper that distinguishes `nil` vs
   empty strings and returns strict booleans.
2. Replace a chain of `if` nil-guards with safe navigation in a small snippet
   and write tests ensuring behavior unchanged.

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Boolean Algebra Laws — Apply to Ruby Conditionals (Appendix — boolean_logic-laws-20251005)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Law</th><th>Rule</th><th>Applied Tip</th></tr>
  </thead>
  <tbody>
    <tr><td>Double negation</td><td>!!A == A</td><td>Use `!!` to normalize truthy values when implementing predicates</td></tr>
    <tr><td>De Morgan</td><td>!(A && B) == (!A || !B)</td><td>Use to rewrite negative conditions into positive checks or simplify `unless` chains</td></tr>
    <tr><td>Commutative</td><td>A && B == B && A</td><td>Order can be changed for readability when safe (watch side effects)</td></tr>
    <tr><td>Associative</td><td>(A && B) && C == A && (B && C)</td><td>Parentheses optional for same-operator groups; use them for mixed operators</td></tr>
    <tr><td>Distributive</td><td>A && (B || C) == (A && B) || (A && C)</td><td>Use to push guards inside or factor them out for early returns</td></tr>
    <tr><td>Idempotent</td><td>A || A == A</td><td>Avoid duplicating checks that add noise; factor predicates into helpers</td></tr>
    <tr><td>Absorption</td><td>A || (A && B) == A</td><td>Recognize redundant guards and remove them</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-enable MD013 -->
### Ruby examples (equivalence assertions)

```ruby
require 'minitest/assertions'
include Minitest::Assertions
self.assertions = 0

# Double negation
x = nil
assert_equal(!!x, x)

# De Morgan
a = true
b = false
assert_equal(!(a && b), (!a || !b))

# Distributive law
a = true
b = false
c = true
left = a && (b || c)
right = (a && b) || (a && c)
assert_equal(left, right)

# Absorption (example: redundant guard)
flag = true
cond = flag || (flag && expensive_check())
# simplified:
cond_simple = flag
assert_equal(cond, cond_simple)
```

> Note: In Ruby `&&`/`||` return operands (not strict booleans). The `assert_equal` checks above rely on truthy/falsey equivalence; wrap with `!!` if you require strict boolean equality.

### Practical refactor tips

- Use De Morgan to replace `unless (a && b)` with `if !a || !b` or to rewrite
  `unless` forms into clearer `if` forms.
- Factor repeated predicates into named predicate methods (e.g., `eligible?`) to
  make idempotent and absorption simplifications obvious.
- When reordering operands (commutative law), avoid moving expressions with side
  effects (method calls that mutate or rely on state).
- For performance, push cheap guards first (short-circuit) so expensive checks
  are skipped early.

<!-- markdownlint-disable MD013 -->
### Appendix — Exercises

1. Rewrite this method using De Morgan and guard clauses to make the happy path
   more obvious:

```ruby
def allow_access?(user)
  return false unless user
  return false unless user.active?
  return false if user.banned? || user.suspended?
  true
end
```

1. Given `a && (b || c)`, produce two refactorings: one that distributes `a`
   (explicit `||`) and one that factors out a guard with an early return; write
   tests that assert behavior equality.
1. Find an example in the codebase (or write a small snippet) where `A || (A &&
   B)` appears; simplify it using absorption and add a unit test demonstrating
   equivalence.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Boolean Operators & Nil Conversions (Appendix — boolean_logic-ops-20251005)

Leverage non-short-circuit operators and nil's conversion methods for concise
defaults.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Use Case</th><th>Insider Tip</th></tr>
  </thead>
  <tbody>
    <tr>
      <td>`&` / `|` / `^`</td>
      <td>Always evaluate both sides</td>
      <td>Use when side effects are needed regardless of first operand</td>
    </tr>
    <tr>
      <td>`nil.to_a`</td>
      <td>Default to empty array</td>
      <td>`(data || []).map` vs `data&.map || []`</td>
    </tr>
    <tr>
      <td>`nil.to_h`</td>
      <td>Default to empty hash</td>
      <td>Perfect for merging configs: `opts.to_h.merge(defaults)`</td>
    </tr>
    <tr>
      <td>`nil.to_s`</td>
      <td>Default to empty string</td>
      <td>Use in interpolations to avoid "nil" strings</td>
    </tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples — Operators & Conversions

```ruby
# & evaluates both, useful for logging
def log_and_check(user)
  log_access(user) & user.active?
end

# Nil conversions for defaults
def process_items(items)
  (items || []).each { |item| puts item }
end

# Or with to_h
def merge_opts(opts)
  opts.to_h.merge(theme: 'dark')
end

# String interpolation
name = nil
puts "Hello #{name.to_s}!"  # => "Hello !"
```

### Exercises — Operators & Conversions

1. Write a method that uses `&` to log an operation and return a boolean result.
2. Refactor a method that checks `if data.nil? then [] else data end` using
`to_a`.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
