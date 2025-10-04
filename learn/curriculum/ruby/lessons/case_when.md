# Case statements and pattern matching

`case` expressions let you compare a value against multiple conditions without stacking `if`/`elsif` branches. Modern Ruby extends `case` with structural pattern matching, guards, and smart matching rules, giving you a powerful control-flow tool for both simple switches and complex data destructuring.

## Learning goals

- Understand how `case` uses the `===` (case equality) operator to evaluate `when` clauses.
- Choose between `case <value>` and `case` with no target for custom boolean logic.
- Combine literals, ranges, regular expressions, and classes in `when` branches.
- Leverage Ruby 2.7+ pattern matching (`in`, guards, destructuring) for nested hashes and arrays.
- Apply best practices to keep `case` expressions readable, exhaustive, and side-effect free.

## Case equality basics

When you write `case target`, each `when` clause calls `===` on the object in the clause, passing the target. That means different types behave uniquely.

```ruby
def weekday_type(day)
  case day
  when "Monday"
    "Start of work week"
  when "Tuesday", "Wednesday", "Thursday"
    "Midweek"
  when "Friday"
    "Almost weekend!"
  when "Saturday", "Sunday"
    "Weekend!"
  else
    "Unknown day"
  end
end
```

`String#===` is essentially `==`, but `Range#===` checks inclusion and `Regexp#===` tests matches.

```ruby
case temperature
when -Float::INFINITY...0
  :freezing
when 0..18
  :cool
when 19..28
  :pleasant
when /storm/i
  :severe
else
  :hot
end
```

## Multi-value when clauses

Separate values with commas to share the same block. Ruby stops at the first matching `when` (no fallthrough).

```ruby
case response.downcase
when "y", "yes", "sure"
  :accepted
when "n", "no"
  :declined
else
  :unknown
end
```

## Using classes and modules

Classes implement `===` to check ancestry, enabling type-based dispatch.

```ruby
def render(value)
  case value
  when String
    "plain text: #{value}"
  when Numeric
    format("number: %.2f", value)
  when Enumerable
    "items: #{value.join(", ")}"
  else
    value.inspect
  end
end
```

This is a lightweight alternative to polymorphism when you can’t modify source classes.

## Case without an argument

Omit the target to evaluate arbitrary boolean expressions—each `when` acts like an `if` condition.

```ruby
case
when score >= 90 && attendance > 0.95
  :honors
when score >= 70
  :pass
when makeup_available?
  :eligible_for_makeup
else
  :fail
end
```

This pattern keeps related conditions grouped while still returning a value.

## Returning values

`case` is an expression: the last evaluated value in the matching branch becomes the result.

```ruby
label = case severity
        when 0 then "DEBUG"
        when 1 then "INFO"
        when 2 then "WARN"
        when 3 then "ERROR"
        else "FATAL"
        end
```

Because `case` returns a value, avoid mixing it with output (`puts`) inside branches; instead, return data and handle side effects outside.

## Guards with `then` or modifiers

Keep short branches terse with `then` or modifiers, but default to multiline blocks for complex logic.

```ruby
case status
when :open then "Ticket is open"
when :closed then "Ticket is closed"
else "Unknown status"
end
```

If each branch spans multiple lines, use `when` on its own line followed by an indented block for readability.

## Pattern matching (`case` / `in`)

Ruby 2.7 introduced `case` pattern matching with the `in` keyword, enabling structural checks and destructuring.

```ruby
case payload
in { status: 200, body: }
  "Success: #{body}"
in { status: 404, path: }
  "Not found: #{path}"
in { status: 500..599, error: }
  "Server error: #{error}"
else
  "Unhandled response"
end
```

- Hash keys in the pattern must exist; missing keys skip the branch unless you use a double splat (`**rest`).
- Variables on the right-hand side (like `body`) capture values from the payload.

### Array destructuring

```ruby
case event
in [:login, { user: }]
  "Login for #{user}"
in [:logout, { user: }]
  "Logout for #{user}"
else
  "Unknown event"
end
```

Use the splat (`*rest`) to capture remaining elements: `in [:metrics, *values]`.

### Guards in pattern matching

Add `if` or `unless` after a pattern to refine matches.

```ruby
case response
in { status: 200, body: } if body.is_a?(Hash)
  handle_success(body)
in { status: 200, body: }
  parse_json(body)
else
  raise "Unexpected response"
end
```

### Exhaustiveness checks

Pattern matching does not enforce exhaustiveness at compile time, so use an `else` branch to raise or log unexpected structures.

## Regex patterns and captures

`Regexp#===` returns true when the pattern matches. Combine with capture variables for further processing.

```ruby
case input
when /^#(?<hex>[0-9a-f]{6})$/i
  hex = Regexp.last_match[:hex]
  "Color code: ##{hex.upcase}"
when /^@(?<handle>[a-z0-9_]{1,15})$/i
  "Twitter handle: #{Regexp.last_match[:handle]}"
else
  "Plain text"
end
```

Pattern matching also supports regex: `in /@(?<handle>\w+)/` assigns captures automatically.

## Best practices

- **Keep branches symmetric.** Use similar formatting and return types to aid readability.
- **Avoid deeply nested case expressions.** Extract helper methods or objects when branches grow large.
- **Prefer immutable return values.** Avoid mutating shared state inside branches unless necessary.
- **Handle else explicitly.** Even if you think you’ve covered every case, add `else` to catch future changes.
- **Document fallbacks.** If an `else` raises, mention why in the exception message.

## Common pitfalls

- **Forgetting `break`.** Ruby’s `case` doesn’t fall through, so you don’t need `break`. If you expect fallthrough, restructure with arrays or successive conditions.
- **Misusing `and` / `or`.** Because of precedence, stick to `&&`/`||` inside `when` clauses unless you intentionally want low-precedence control flow.
- **Side effects in `when`.** Comparing the target should be pure; avoid assignments that change program state.
- **Ambiguous patterns.** When a value matches multiple branches (e.g., regex and string), order matters—higher priority patterns go first.

## Guided practice

1. **HTTP response router**
   - Accept a hash `{ status:, body:, headers: }`.
   - Use structural pattern matching to return symbols like `:ok`, `:redirect`, `:client_error`, `:server_error`.
   - Add a guard to detect JSON responses only when `headers[:content_type]` includes `"json"`.

2. **Command parser**
   - Parse user input strings like `"deploy staging"` or `"rollback production --force"`.
   - Use regular-expression `when` clauses to capture environment names and flags.
   - Return structured hashes describing the command.

3. **Type-based renderer**
   - Write `render_value(value)` that uses `case value` with classes to format strings, numbers, arrays, and hashes differently.
   - Include a branch for objects responding to `to_h`, converting them before rendering.

4. **Event matcher**
   - Given arrays shaped like `[:create, { model: "User", attrs: {...} }]` or `[:destroy, { model: "Order", id: 42 }]`, use pattern matching to dispatch to handler methods.
   - Capture nested hash values using keyword-style matches.

5. **Fallback auditing**
   - Add logging to the `else` branch of an existing `case` expression to capture unexpected inputs, and write a test verifying the log triggers when an unknown value arrives.

## Self-check questions

1. How does the `===` operator differ across `String`, `Range`, `Regexp`, and `Class`, and how does that impact case matching?
2. When would you choose `case` without an argument instead of a `case target` expression?
3. How do pattern matching guards (`if`/`unless`) change the behavior of a branch?
4. What strategies can you use to ensure a `case` expression remains maintainable as the number of branches grows?
5. Why is it important to include an `else` branch, and what should it typically do in production code?

Well-structured `case` expressions keep control flow readable and adaptable. As your data models evolve, revisit your branches, tighten guards, and lean on pattern matching to express intent with less code.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Pattern Matching Recipes

This appendix shows common patterns for matching API responses, arrays, and nested hashes. Includes an HTML table and exercises.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>Example</th><th>Use case</th></tr>
  </thead>
  <tbody>
    <tr><td>Range</td><td>when 0..9</td><td>Grades, buckets</td></tr>
    <tr><td>Regexp</td><td>when /email/</td><td>Input parsing</td></tr>
    <tr><td>Hash match</td><td>in {status: 200, body:}</td><td>API responses</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises

1. Build a response router using `case/in` that handles success, redirect, and error cases.
2. Add guards to match only JSON responses when `headers[:content_type]` includes `json`.

<!-- markdownlint-enable MD010 -->
