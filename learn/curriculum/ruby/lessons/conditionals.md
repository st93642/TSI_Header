# Conditionals and Control Flow

Conditionals give your Ruby code the power to react, branch, and adapt at runtime. Ruby’s syntax makes branching expressive and concise, whether you’re validating user input, routing HTTP requests, or orchestrating background jobs. This lesson explores the full spectrum of conditional tools—from `if` chains to pattern matching—while emphasizing clarity and maintainability.

## Learning goals

- Compose conditionals using `if`, `elsif`, `else`, and statement modifiers.
- Decide when to use `unless`, `case`, or pattern matching for more readable
  logic.
- Apply guard clauses and early returns to keep happy paths clear.
- Understand truthiness, short-circuit evaluation, and ternary expressions.
- Practice structuring nested logic into well-named helper methods.

## Basic `if` usage

The classic `if` expression executes its block when its condition evaluates to truthy (anything except `false` or `nil`).

```ruby
age = 18

if age >= 18
  puts "You can vote."
end
```

Ruby’s `if` returns the last evaluated expression, allowing assignment:

```ruby
message = if age >= 18
  "Eligible"
else
  "Not yet"
end
```

Avoid trailing `return` inside the branches when the `if` expression itself is the return value of a method.

## `if`/`elsif`/`else` chains

Use chained branches to handle mutually exclusive scenarios.

```ruby
def grade_for(score)
  if score >= 90
    "A"
  elsif score >= 80
    "B"
  elsif score >= 70
    "C"
  else
    "F"
  end
end
```

Order matters: Ruby evaluates conditions top to bottom and stops at the first truthy branch. When conditions become complex, consider extracting them into predicate methods (`excellent?`, `passing?`) for readability.

## Guard clauses and early returns

Guard clauses exit a method early if prerequisites fail. They keep the main
logic close to the left margin and reduce nested indentation.

```ruby
def process_order(order)
  return "No items" if order.items.empty?
  return "Account locked" unless order.account.active?

  ship(order)
end
```

`return` exits the method immediately. Inside blocks (like `each`), use `next` to skip to the following iteration.

## Statement modifiers: concise conditionals

Ruby allows postfix conditionals for single expressions.

```ruby
puts "High score!" if score > 100
warn "Disk space low" unless free_space_gb > 5
```

Use them for brief, readable conditions. If the logic grows or requires an `else`, revert to multi-line `if` blocks.

## `unless` for negative checks

`unless` executes its block unless the condition is truthy. Treat it as the opposite of `if`.

```ruby
unless user&.verified?
  puts "Please verify your email."
end
```

Avoid combining `unless` with `else`; it inverts twice and harms readability. Opt for `if` in those cases.

## Ternary operator

`condition ? true_branch : false_branch` returns one of two values succinctly.

```ruby
age = 20
status = age >= 18 ? "Adult" : "Minor"
```

Keep ternaries simple. When branches require multiple statements, use a regular `if` expression instead.

## Combining logical operators

Use `&&` (AND), `||` (OR), and `!` (NOT) to compose complex conditions. Remember that `&&` and `||` short-circuit.

```ruby
if user.logged_in? && user.admin?
  puts "Welcome, admin."
end

if weekend? || holiday?
  puts "Enjoy your time off!"
end

puts "Bring umbrella" if raining? && !indoors?
```

Group with parentheses for clarity.

## `case` expressions

`case` shines when matching one object against many possibilities. It uses `===` internally, enabling ranges, classes, and regex matches.

```ruby
grade = case score
        when 90..100 then "A"
        when 80...90 then "B"
        when 70...80 then "C"
        else "Needs improvement"
        end
```

`case` can also omit the target to evaluate arbitrary conditions:

```ruby
message = case
          when temp > 30 then "Too hot"
          when temp < 10 then "Too cold"
          else "Just right"
          end
```

## Pattern matching (Ruby 2.7+)

Enhanced `case` lets you destructure hashes and arrays while matching.

```ruby
case response
in { status: 200, body: }
  puts "OK: #{body}"
in { status: 404 }
  puts "Not found"
else
  puts "Unexpected response"
end
```

Add guards (`if condition`) to refine matches.

## Safe navigation with conditionals

Use `&.` to call methods on objects that might be `nil`, avoiding `NoMethodError`.

```ruby
if user&.profile&.completed?
  puts "Thanks for completing your profile!"
else
  puts "Please complete your profile."
end
```

Combine with `||` to supply defaults: `user&.name || "Guest"`.

## Building expressive predicates

Encapsulate logic in predicate methods (`deadline_passed?`, `eligible?`) to keep conditionals readable.

```ruby
def overdue?(invoice)
  Time.current > invoice.due_at && !invoice.paid?
end

puts "Overdue" if overdue?(invoice)
```

## Nested conditionals vs. refactoring

Deeply nested `if` statements hinder readability. Strategies to flatten them include:

- Guard clauses to exit early.
- Extracting helper methods for inner logic.
- Using hashes to map keys to actions (dispatch tables).

```ruby
HANDLERS = {
  "paid" => ->(order) { ship(order) },
  "pending" => ->(order) { enqueue_verification(order) }
}

<!-- markdownlint-disable MD033 MD034 MD040 MD010 MD022 MD032 MD024 -->

## Practical Appendix: Conditionals — Guard Clauses & Tests (Appendix — conditionals-quick)

Short patterns for keeping conditionals clear and testable.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>When</th><th>Test tip</th></tr>
  </thead>
  <tbody>
    <tr><td>Guard clauses</td><td>Early exits</td><td>Test each guard path separately</td></tr>
    <tr><td>Predicate methods</td><td>Readable conditions</td><td>Unit-test predicate logic</td></tr>
    <tr><td>Dispatch tables</td><td>Replace long if/elsif chains</td><td>Test mapping completeness and default case</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

Exercises:

1. Refactor a nested conditional into guard clauses with small predicate methods and add tests.
2. Replace an `if/elsif` cascade with a dispatch hash and test default handling.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
}.freeze

handler = HANDLERS[order.status] || ->(_) { warn "Unknown status" }
handler.call(order)
```

## Practice: boolean expressions recap

Remember that only `false` and `nil` are falsy. Use trait methods and checkers (`empty?`, `any?`, `zero?`) to convey intent.

```ruby
puts "Inventory empty" unless items&.any?
```

## Guided practice

1. **Shipping router**
   - Write `shipping_method(order)` that returns "express" when the destination
     is domestic and the order total exceeds 100, "standard" for domestic orders
     otherwise, and "international" for everything else.
   - Use guard clauses for missing address information.

2. **On-call schedule**
   - Implement logic that prints who is on call based on day of week and whether
     the person is already covering another shift.
   - Use `case` with `Date.today.wday` and a fallback branch.

3. **Feature toggle evaluator**
   - Combine environment variables, user roles, and percentage rollouts
     (`rollout_percentage > rand(100)`) into a single predicate
     `feature_enabled?(user)`.
   - Keep the expression readable with intermediate variables or helper methods.

4. **Pattern match API responses**
   - Given hashes with keys `:status`, `:body`, and optional `:error`, use
     pattern matching to log success, retry, or escalate.
   - Add guards to check `error[:code]` when present.

5. **Input validator**
   - Prompt for a username and classify it as "valid", "missing", or "invalid"
     using an `if`/`elsif` chain.
   - Requirements: present, 3–16 characters, only lowercase letters or
     underscores.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Conditionals — Guard Clauses & Idioms (Appendix — conditionals-ruby2)
<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>When</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr><td>Guard clauses</td><td>Preconditions</td><td>`return unless valid?`</td></tr>
    <tr><td>Ternary</td><td>Simple branches</td><td>`a > b ? a : b`</td></tr>
    <tr><td>Dispatch table</td><td>Many cases</td><td>Hash of lambdas</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Quick pattern

```ruby
def safe_process(data)
  return unless data && data[:enabled]
  process(data)
end
```

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
Guidance for choosing conditional styles, guard clauses for early returns, and
test patterns for branching logic.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Style</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Guard clause</td><td>Early returns</td><td>Makes code flatter and clearer</td></tr>
    <tr><td>case/when</td><td>Multiple branches</td><td>Use `when` with ranges or classes</td></tr>
    <tr><td>ternary</td><td>Simple inline choices</td><td>Keep concise; avoid nested ternaries</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```ruby
def process(user)
  return unless user&.active?
  # proceed
end

case value
when 0..9
  :small
when 10..99
  :medium
else
  :large
end
```

### Testing branches

- Write separate tests for each branch, include edge cases (nil, boundary
  values).

```ruby
require 'minitest/autorun'

class TestConditionals < Minitest::Test
  def test_guard
    assert_nil process(nil)
  end
end
```

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — conditionals-ruby2)

1. Refactor a method that has nested `if` statements into guard clauses and case
   statements where appropriate; add tests proving behaviour unchanged.
2. Implement a parser that returns different symbols for numeric ranges and
   thoroughly test boundaries (e.g., 9, 10, 99, 100).

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Conditionals — Refactoring & Property Tests (Appendix — conditionals-ruby3)

Refactor patterns for complex branching, guard-clause idioms, and an
introduction to property-based testing approaches for conditional logic.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Refactor</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Guard clauses</td><td>Reduce nesting</td><td>Return early for invalid inputs</td></tr>
    <tr><td>Case expressions</td><td>Multiple branches</td><td>Use `when` with ranges for clarity</td></tr>
    <tr><td>Property tests</td><td>Complex invariants</td><td>Randomized inputs can reveal edge cases</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: guard clauses

```ruby
def process(item)
  return :invalid unless item
  return :empty if item.empty?
  # normal processing
end
```

### Property-like checks

- Use small randomized inputs to assert invariants, e.g., that a function
  preserves length or ordering.

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — conditionals-ruby3)

1. Refactor a nested-if example into guard clauses and provide tests
   demonstrating behavior preserved.
2. Write a property-style test that checks a conditional transformation keeps
   ordering for random inputs.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-enable MD013 -->
## Self-check questions

1. When would you prefer `case` over multiple `elsif` branches?
2. How does the ternary operator differ from `if` in terms of return value and
   readability?
3. Why should you avoid `unless` with `else` clauses?
4. How do guard clauses improve the structure of methods with multiple failure
   conditions?
5. What advantages does pattern matching provide for handling structured data
   compared to nested conditionals?

Dependable control flow stems from clear boolean logic and a toolbox of
conditional constructs. Experiment with each technique, refactor nested branches
into descriptive helper methods, and you’ll write Ruby code that communicates
its intent with confidence.

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Conditionals — Guard Clauses & Readability (Appendix — conditionals-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>When</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr><td>Guard clause</td><td>Early return on invalid input</td><td>Keeps happy path unindented</td></tr>
    <tr><td>Case/when</td><td>Dispatch on values</td><td>Prefer `case` over long `if/elsif` chains when possible</td></tr>
    <tr><td>Predicate methods</td><td>`valid?` / `present?`</td><td>Encapsulate checks to improve readability</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```ruby
def process(obj)
  return unless obj.valid?
  # happy path here
end
```

<!-- markdownlint-disable MD013 -->
### Appendix exercises

1. Refactor a nested conditional to use guard clauses and add tests that cover
   the early returns.
2. Convert a long `if/elsif` chain to `case` and verify behavior remains
   identical.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
