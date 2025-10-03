# Defining Methods in Ruby

Methods encapsulate behavior behind a name. They reduce duplication, clarify intent, and form the public interface of classes and modules. Ruby makes method definitions flexible: you can accept positional, keyword, and block arguments, return multiple values, and even define methods dynamically.

## Learning goals

- Define methods with different argument styles (positional, keyword, optional, splat, double splat, block).
- Control return values, guard clauses, and early exits.
- Follow Ruby naming conventions (`snake_case`, `?`, `!`).
- Pass and yield blocks, convert between blocks and `Proc` objects, and forward arguments.
- Use visibility modifiers (`public`, `private`, `protected`) and dynamically define methods at runtime when needed.

## Method definition basics

Use the `def` keyword, optionally with parameters. Methods implicitly return the last evaluated expression.

```ruby
def greet
  "Hello, world!"
end

greet # => "Hello, world!"
```

Explicit `return` is rarely required but helpful for guard clauses or exiting early.

```ruby
def divide(a, b)
  return Float::INFINITY if b.zero?
  a.to_f / b
end
```

## Positional arguments and defaults

Defaults provide optional parameters.

```ruby
def send_email(to, subject = "(no subject)")
  "Sending '#{subject}' to #{to}"
end

send_email("team@company.com")
```

Place optional arguments at the end to avoid ambiguity.

## Keyword arguments and defaults

Keyword arguments make call sites self-documenting.

```ruby
def create_user(name:, email:, role: :member)
  { name: name, email: email, role: role }
end

create_user(name: "Ava", email: "ava@example.com")
```

Use double splats (`**options`) to capture arbitrary keyword options, invaluable for wrapper methods that forward extra options.

```ruby
def log_event(event, **metadata)
  { event: event, timestamp: Time.now, **metadata }
end
```

## Variable arguments (splat)

The splat operator `*` gathers positional arguments into an array.

```ruby
def sum(*numbers)
  numbers.sum
end

sum(1, 2, 3, 4) # => 10
```

You can mix regular parameters and splats.

```ruby
def tag(name, *classes)
  "<#{name} class='#{classes.join(' ')}'>"
end

tag(:div, "card", "highlight")
```

## Argument forwarding (`...`)

Ruby 2.7+ introduces the forwarding argument (`...`) to capture all arguments (positional, keyword, block) and forward them.

```ruby
def instrument(name, ...)
  start = Time.now
  result = send(name, ...)
  elapsed = Time.now - start
  puts "#{name} took #{elapsed}s"
  result
end

def heavy_work(x:, y:)
  # expensive computation
end

instrument(:heavy_work, x: 1, y: 2)
```

## Multiple return values and parallel assignment

Methods can return arrays; Ruby supports destructuring.

```ruby
def min_max(numbers)
  [numbers.min, numbers.max]
end

min, max = min_max([1, 9, 4])
```

Returning hashes makes more explicit contracts when many values are involved.

## Naming conventions

- Use `snake_case` for method names.
- Methods ending in `?` return boolean-like values (`empty?`).
- Methods ending in `!` perform dangerous operations (mutate or raise). Remember: this is a convention, not enforced by Ruby.

```ruby
def valid?(input)
  input.size > 3
end

def save!
  raise "Not implemented"
end
```

## Blocks and yielding

Methods can accept blocks implicitly and execute them with `yield`.

```ruby
def with_logging
  puts "Starting"
  result = yield
  puts "Finished"
  result
end

with_logging { 1 + 2 }
```

Check `block_given?` before yielding to avoid `LocalJumpError`.

```ruby
def maybe(&block)
  return unless block
  block.call
end
```

Blocks can be converted to `Proc` objects by prefixing the parameter with `&`. Blocks passed explicitly can be forwarded.

```ruby
def wrap(&block)
  puts "Before"
  block.call
  puts "After"
end

def greet
  puts "hi"
end

wrap(&method(:greet))
```

## Visibility: public, private, protected

Methods are public by default. Use `private` or `protected` to restrict access.

```ruby
class Account
  def balance
    calculate_balance
  end

  private

  def calculate_balance
    # implementation details
  end
end
```

`private` methods can’t be called with an explicit receiver, even `self`. `protected` methods allow calls with another instance of the same class/subclass.

## Defining methods dynamically

Ruby allows meta-programming via `define_method` and `method_missing`.

```ruby
class Settings
  def initialize(store = {})
    @store = store
  end

  def self.define_boolean(name)
    define_method("#{name}?") do
      !!@store[name]
    end
  end
end

Settings.define_boolean(:dark_mode)
Settings.new(dark_mode: true).dark_mode? # => true
```

Use such power sparingly and document generated methods for maintainability.

## Method objects and reflection

`object.method(:name)` returns a `Method` object you can call or pass around.

```ruby
adder = 5.method(:+)
adder.call(3) # => 8
```

`respond_to?` checks if a method exists, enabling duck typing.

## Guard clauses and control flow

Guard clauses keep methods slim by exiting early.

```ruby
def publish(post)
  return false unless post.valid?
  return false if post.published?

  post.publish!
end
```

Prefer small, focused methods that do one thing well. Decompose complex logic into private helper methods when needed.

## Guided practice

1. **Argument forwarding helper**
   - Implement `instrument(name, ...)` that logs method start/finish, forwards arguments using `...`, and returns the original result.

2. **Config builder**
   - Write `build_config(defaults = {}, **overrides)` that merges defaults and overrides while keeping unexpected keys in a `:extras` hash.

3. **Block-required method**
   - Implement `retry_on_error(max_attempts: 3)` that demands a block, retries failures, and returns the block result or raises after the last attempt.

4. **Dynamic boolean methods**
   - Create a module `Flags` exposing `.flag(name)` which defines both setter and `?` reader via `define_method`.

5. **Visibility exercise**
   - Build `Authenticator#login(user)` that calls private methods for password verification and protected methods for token issuance shared across subclasses.

## Self-check questions

1. How do `*args`, `**kwargs`, and `...` differ when defining methods?
2. When should you use `return` explicitly inside Ruby methods?
3. Why is `block_given?` important before calling `yield`?
4. What’s the difference between `respond_to?` and `method_defined?` when reflecting on methods?
5. When does defining methods dynamically with `define_method` make sense, and what trade-offs come with meta-programming?

Mastering Ruby method definitions empowers you to build expressive APIs. Keep your methods small, choose argument styles that make intent clear, and use blocks and keyword arguments to craft elegant, flexible interfaces.
