# Debugging with `p`

Ruby’s `p` method is a fast, expressive way to inspect values while you code. It combines printing with inspection, making it perfect for tracing state, verifying assumptions, and understanding unfamiliar objects. Learning when—and how—to use `p` can dramatically speed up your debugging sessions.

## Learning goals

- Compare `p`, `puts`, `print`, and `pp` to choose the right inspection tool.
- Understand how `p` leverages `Object#inspect` and how to customize it.
- Use `tap`, `then`, and block constructs to log intermediate values without changing control flow.
- Capture inspected output for logs or tests, and redirect it safely.
- Apply `p` in real-world debugging scenarios, from API responses to complex enumerables.

## What `p` does

`p` prints an object using `inspect` and returns the object itself. It writes to `STDOUT` by default.

```ruby
value = "Hello"
returned = p(value)
# STDOUT: "Hello"
# returned == "Hello"
```

Because `p` returns the original object, you can chain it or sprinkle it in expression pipelines without breaking the result.

## `p` vs. `puts` vs. `pp`

```ruby
puts "Hello"   # => Hello (no quotes)
p "Hello"      # => "Hello" (uses inspect)
require "pp"
pp "Hello"     # => "Hello" (pretty-print; useful for nested structures)
```

- **`puts`**: great for user-facing output. Converts to string and adds a newline per argument.
- **`p`**: inspection output with quotes, escape characters, and nested objects on one line.
- **`pp`**: pretty-print nested arrays/hashes; ideal when a single line wraps awkwardly.

## Inspecting common structures

```ruby
arr = [1, [2, 3], { ok: true }]
puts arr
# 1
# [2, 3]
# {:ok=>true}

p arr
# [1, [2, 3], {:ok=>true}]

require "pp"
pp arr
# [1,
#  [2, 3],
#  {:ok=>true}]
```

Choose the style that keeps data legible and context clear.

## Customizing inspection

`p` calls `inspect`. Override `inspect` (and ideally `pretty_print`) in your classes for clearer diagnostics.

```ruby
class Token
  attr_reader :value, :expires_at

  def initialize(value:, expires_at:)
    @value = value
    @expires_at = expires_at
  end

  def inspect
    "#<Token value=**** expires_at=#{expires_at.iso8601}>"
  end
end

p Token.new(value: "secret", expires_at: Time.now + 3600)
# #<Token value=**** expires_at=2025-10-03T14:00:00+00:00>
```

Custom inspection keeps sensitive data safe and highlights what matters.

## Inline debugging with `tap`

`Kernel#tap` yields the object to a block and returns the object. Combine it with `p` to log intermediate values without altering flow.

```ruby
result = users
  .select { |u| u.active? }
  .tap { |list| p active_users: list.size }
  .map(&:email)
```

`ActiveSupport#debug` and `Object#yield_self`/`then` offer similar patterns.

## Capturing output

Redirect `STDOUT` temporarily or use `StringIO` when you need to capture `p` output in tests.

```ruby
require "stringio"

io = StringIO.new
$stdout = io
p({ status: :ok })
$stdout = STDOUT

io.string # => "{:status=>:ok}\n"
```

Always restore `$stdout` inside an `ensure` block to avoid affecting later code.

## Logging contexts

Use `p` liberally during development, but swap to structured logging before shipping. One pattern: guard debug output with an environment check.

```ruby
p response if ENV["DEBUG"]
```

For production-grade logs, prefer `logger.debug` or `Rails.logger.debug` with JSON payloads.

## Beyond the basics

- `Kernel#pp` (pretty print) formats nested data across multiple lines.
- Gems like `awesome_print` or `pry` offer colored, annotated dumps.
- `Object#pretty_inspect` returns the pretty-printed string without printing—handy for building log messages.
- `Kernel#caller` combined with `p` reveals stack traces when needed.

## Guided practice

1. **API response trace**
   - Fetch JSON from an API (or mock the response).
   - Use `p` to inspect the parsed hash, focusing on nested structures.
   - Override `inspect` on a wrapper class to hide large payload fields.

2. **Enumerator pipeline**
   - Chain `map`, `select`, and `reduce` over an array.
   - Insert `tap { |value| p stage: "after map", value: }` to observe each phase.

3. **Custom inspect implementation**
   - Build a `User` struct whose `inspect` masks `password_digest` and includes role/count metadata.
   - Demonstrate before/after output using `p`.

4. **Capture in tests**
   - Write a mini helper that captures output from `p` calls.
   - Assert that debug statements include key information without leaking sensitive data.

5. **Conditional debug flag**
   - Implement a helper `debug_dump(label, value)` that calls `p` only when `ENV["DEBUG"]` is truthy.
   - Ensure it still returns `value` so callers can chain.

## Self-check questions

1. How does `p` differ from `puts` in terms of return value and visual output?
2. Why is overriding `inspect` useful, and what should you watch out for when doing so?
3. How can `tap` help you view intermediate states without altering the final result of an expression?
4. When should you upgrade from `p` to structured logging or dedicated debugging tools?
5. What techniques let you capture or suppress `p` output during automated tests?

`p` is the Rubyist’s quick flashlight—turn it on to illuminate confusing code paths, then switch to more targeted instrumentation once you’ve found the issue. With thoughtful use, it becomes an indispensable part of your debugging toolkit.
