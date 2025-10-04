# Boolean Logic and Control Flow Foundations

Boolean logic is the decision-making engine of every Ruby program. Mastering truthiness, comparisons, and logical operators lets you build clear conditionals, guard against edge cases, and express complex rules succinctly. This lesson extends far beyond `true` and `false`, exploring how Ruby evaluates expressions and how you can harness boolean logic to guide control flow.

## Learning goals

- Understand how Ruby represents booleans and truthy/falsy values.
- Build predictable conditionals using comparison operators and logical composition.
- Apply guard clauses, safe navigation, and pattern matching to simplify branching.
- Distinguish between `&&/||` and `and/or`, recognizing precedence differences.
- Practice real-world boolean patterns used in authentication, validation, and configuration.

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

Evaluates the left operand first. If it’s falsy, Ruby skips the right operand entirely (short-circuiting) and returns the falsy value. Otherwise it returns the right operand.

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

Breaking logic across lines with descriptive helper methods often beats writing giant expressions.

## Guard clauses and early returns

Guard clauses validate assumptions up front and return early when conditions fail, keeping the happy path clear.

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

- **Assignment vs equality**: `=` assigns; `==` compares. Write conditionals as `if (value = compute)` intentionally, with parentheses to signal assignment.
- **Truthy strings**: `"false"` is truthy because it’s a non-empty string—coerce user input explicitly when parsing booleans.
- **Precedence surprises**: `a && b || c` groups as `(a && b) || c`. Add parentheses for clarity.
- **Skipping explicit nil checks**: Methods like `empty?` will raise on `nil`. Use safe navigation or guard against `nil` first.

## Guided practice

1. **Eligibility rules**
   - Write a method `eligible_for_discount?(user)` that returns true when the user has at least one of these: loyalty tier "gold" or "platinum", or an active coupon that hasn’t expired.
   - Use guard clauses for nil checks (`user.coupon&.expired?`).

2. **Feature rollout**
   - Combine environment variables and runtime checks to compute a boolean flag `feature_enabled`.
   - Enable when `ENV["FEATURE"] == "on"` or the current user is in a beta testers list.

3. **Safe dig**
   - Given a nested hash `settings`, write `timezone_for(settings)` returning the timezone or "UTC" if any intermediate key is missing.
   - Use safe navigation (`&.`) or `dig` with boolean fallbacks.

4. **Input sanitization**
   - Parse user input strings like "yes", "no", "true", "0" into booleans using a method `truthy?(value)`.
   - Treat unknown inputs as `false` and document the conversion table.

5. **Alarm system**
   - Implement logic that triggers an alarm when either a door sensor or window sensor is open and the system is armed.
   - Add a bypass flag that, when true, suppresses the alarm even if sensors are tripped.

## Self-check questions

1. Why do `&&`/`||` return the original operands instead of strict booleans, and how can that behavior be useful?
2. How does short-circuit evaluation prevent runtime errors when chaining method calls?
3. When should you use `and`/`or` instead of `&&/||`?
4. How do you treat empty collections as falsy without breaking on `nil` values?
5. What naming conventions help signal that a method returns a boolean?

Strong boolean logic unlocks robust conditionals, readable code, and fewer surprises. Keep experimenting with combinations, guard clauses, and expressive predicate methods—you’ll quickly find your Ruby code becoming clearer and more reliable.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Operator Precedence & Patterns

This appendix shows precedence examples, safe boolean helpers, and an HTML table summarising operator precedence and common idioms.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Operator</th><th>Precedence</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>!</td><td>High</td><td>Negation</td></tr>
    <tr><td>&&</td><td>Higher</td><td>Logical AND</td></tr>
    <tr><td>||</td><td>Lower</td><td>Logical OR</td></tr>
    <tr><td>and/or</td><td>Very low</td><td>Use for control flow, not expressions</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises

1. Build a `truthy_value?(v)` helper that normalizes common boolean-like strings to booleans.
2. Rewrite a complex conditional using intermediate variables for clarity and add unit tests.

<!-- markdownlint-enable MD010 -->
