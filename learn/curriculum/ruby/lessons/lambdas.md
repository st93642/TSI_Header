# Lambdas in Ruby

Lambdas are anonymous functions you can store in variables, pass around, and evaluate on demand. They power callbacks, functional pipelines, and customizable behavior without creating full classes. While lambdas share DNA with `Proc` objects, they enforce stricter argument rules and respect method-return semantics—making them a great fit for predictable functional code.

## Learning goals

- Create lambdas using both `lambda` and stabby (`->`) syntaxes, with positional, optional, and keyword parameters.
- Compare lambdas with procs, especially around arity checking and return semantics.
- Compose lambdas with enumerables, higher-order functions, and partial application techniques.
- Capture state safely via closures, memoization, and currying.
- Integrate lambdas into object-oriented designs for strategies, policies, and event hooks.

## Creating lambdas

```ruby
# Long-form constructor
logger = lambda { |message| puts "[LOG] #{message}" }

# Stabby syntax (preferred)
logger = ->(message) { puts "[LOG] #{message}" }

logger.call("Start")
logger.("Finish")     # shorthand call
logger["Queued"]       # behaves like proc (index syntax)
```

### Multi-line blocks

```ruby
pretty_print = ->(items) do
  items.each_with_index do |item, index|
    puts format("%2d. %s", index + 1, item)
  end
  items
end

pretty_print.call(%w[ruby rails rack])
```

## Parameter options

```ruby
handler = ->(event, metadata = {}, verbose: false) do
  puts "Received #{event} with #{metadata}"
  puts "Verbose log" if verbose
end

handler.call("login")
handler["error", { code: 500 }, verbose: true]
```

- Lambdas enforce arity: calling without the required arguments raises `ArgumentError`.
- Use defaults and keyword arguments to provide flexible interfaces.
- Destructure parameters with pattern matching:

```ruby
process = ->((status, payload)) { puts "#{status}: #{payload}" }
process.call(["ok", { id: 1 }])
```

## Lambdas vs. Procs

| Feature              | Lambda                              | Proc                                      |
|----------------------|-------------------------------------|-------------------------------------------|
| Arity enforcement    | Strict (`ArgumentError` on mismatch)| Lenient (missing args become `nil`)       |
| Return semantics     | `return` exits lambda only          | `return` exits enclosing method           |
| `next`/`break`       | Behave like in blocks (exit lambda) | Exit enclosing method                     |
| Typical use          | Functional helpers, callbacks       | Flexible, legacy APIs, DSLs               |

```ruby
strict = ->(x, y) { x + y }
strict.call(1, 2)      # => 3
# strict.call(1)       # raises ArgumentError

loose = Proc.new { |x, y| x.to_i + y.to_i }
loose.call(1)          # => 1 (y becomes nil)
```

### Return semantics in context

```ruby
def lambda_return
  worker = -> { return "lambda" }
  worker.call
  "method"
end

lambda_return # => "method"

def proc_return
  worker = Proc.new { return "proc" }
  worker.call
  "method"
end

proc_return   # => "proc"
```

Choose lambdas when you want self-contained control flow that won’t unexpectedly exit callers.

## Higher-order functions

```ruby
def execute(a, b, operation)
  operation.call(a, b)
end

add = ->(x, y) { x + y }
execute(3, 4, add)       # => 7

# Inline lambda
result = execute(10, 3, ->(x, y) { x - y })
```

Lambdas allow behavior injection into methods—great for strategy patterns and configurable pipelines.

## Lambdas with enumerables

```ruby
numbers = [1, 2, 3, 4, 5]

square  = ->(n) { n * n }
odd?    = ->(n) { n.odd? }
display = ->(n) { puts "n = #{n}" }

numbers.map(&square).select(&odd?).each(&display)
```

Use the `&` operator to convert lambdas to blocks. The reverse (`to_proc`) works too, enabling method references: `["ruby", "rails"].map(&:upcase)`.

### Stateful closures

```ruby
def counter(start = 0)
  count = start
  -> { count += 1 }
end

next_id = counter(100)
next_id.call # => 101
next_id.call # => 102

another = counter
another.call # => 1
```

Each lambda retains its own enclosed state—useful for memoization, rate limiting, or incremental IDs.

## Currying and partial application

Currying transforms a lambda with multiple parameters into a series of single-argument lambdas.

```ruby
adder = ->(a, b, c) { a + b + c }
curried = adder.curry

add_five = curried.call(2, 3)
add_five.call(10) # => 15
```

For partial application without currying, wrap inside another lambda:

```ruby
def multiplier(factor)
  ->(value) { value * factor }
end

double = multiplier(2)
double.call(10) # => 20
```

## Composing lambdas

```ruby
def compose(*functions)
  ->(value) { functions.reverse.reduce(value) { |memo, fn| fn.call(memo) } }
end

trim   = ->(s) { s.strip }
upcase = ->(s) { s.upcase }
shout  = ->(s) { "#{s}!" }

processor = compose(trim, upcase, shout)
processor.call("  hello  ") # => "HELLO!"
```

Composition keeps transformations modular. Combine with enumerables for powerful data processing pipelines.

## Memoization pattern

```ruby
def memoize(fn)
  cache = {}
  ->(*args) do
    cache.fetch(args) { cache[args] = fn.call(*args) }
  end
end

slow_fib = ->(n) { n < 2 ? n : slow_fib.call(n - 1) + slow_fib.call(n - 2) }
fast_fib = memoize(slow_fib)

fast_fib.call(35) # much faster than raw recursion
```

Memoization caches results per argument tuple, demonstrating lambdas’ ability to capture state and wrap behavior.

## Lambdas in object-oriented design

```ruby
class PaymentProcessor
  def initialize(strategy:)
    @strategy = strategy
  end

  def charge(amount)
    @strategy.call(amount)
  end
end

credit_card = ->(amount) { puts "Charging card #{amount}" }
crypto      = ->(amount) { puts "Sending #{amount} sats" }

PaymentProcessor.new(strategy: credit_card).charge(25)
PaymentProcessor.new(strategy: crypto).charge(0.001)
```

Lambdas inject policies without subclassing—ideal for strategies, guards, or feature toggles.

## Error handling & arity awareness

```ruby
safe_divide = ->(a, b) do
  raise ArgumentError, "Division by zero" if b.zero?
  a / b.to_f
end

begin
  puts safe_divide.call(10, 0)
rescue ArgumentError => e
  warn e.message
end

case safe_divide.arity
when 2 then puts "Binary operation"
else puts "Unexpected arguments"
end
```

The `arity` method reports parameter counts (positive numbers for fixed arity, negative for optional arguments). Use it to validate collaborators before invocation.

## Performance considerations

- Lambdas are lightweight but not free; avoid creating new ones inside tight loops if you can reuse an existing instance.
- Prefer lambdas over methods when behavior truly needs to be dynamic or passed around—otherwise define a private method.
- Instrument with `Benchmark.measure` if performance is critical; compare lambdas, methods, and direct blocks to choose the best fit.

## When to choose lambdas

Use lambdas when you need:

- Callbacks or hooks configurable at runtime.
- Functional transformations (map/filter/reduce) with predictable arity.
- Encapsulated state via closures (counters, memoization, lazy evaluation).
- Strategy objects without extra classes.
- Composition pipelines where each step is a reusable function.

Reach for procs or blocks when you require lenient arity or DSL-style behavior that manipulates the surrounding scope.

## Guided practice

1. **Pipeline builder**
   - Implement `pipeline(*steps)` returning a lambda that threads a value through each step.
   - Provide lambdas for trimming strings, squashing whitespace, and adding suffixes.
   - Verify the pipeline handles both strings and arrays of strings.

2. **Retry wrapper**
   - Write `with_retries(retries:, delay:)` that accepts a lambda and retries it when it raises a transient error.
   - Capture exceptions, sleep between attempts, and re-raise after exhausting retries.

3. **Currying calculator**
   - Create a curried lambda `weighted_average` that first accepts weights, then values.
   - Ensure partial application works (e.g., `grades_average = weighted_average.call([0.4, 0.6])`).

4. **Strategy registry**
   - Build a hash mapping symbols to lambdas (e.g., `:json`, `:yaml`, `:csv`).
   - Write `serializer_for(format)` that returns the lambda and raises on unsupported formats.
   - Demonstrate with sample payloads.

5. **Closure-based rate limiter**
   - Implement `rate_limiter(limit:, interval:)` returning a lambda that yields `true` when allowed and `false` when throttled.
   - Keep timestamps in the closure and prune stale entries.

## Self-check questions

1. How do lambdas differ from procs with respect to argument checking and `return` statements?
2. What advantages do closures provide when capturing external variables inside lambdas, and how can this lead to stateful behavior?
3. How does currying change the way you call a lambda, and when is partial application more appropriate than currying?
4. Why might you pass lambdas into object constructors instead of subclassing or using conditionals?
5. What patterns help avoid performance or readability issues when you introduce lambdas into a codebase?

Lambdas give you a middle ground between single-use blocks and full classes—embrace them for clean, composable, expressive Ruby code.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Lambdas & Procs — Arity, Currying & Use Cases (Appendix — lambdas-ruby2)

Practical guide to when to use `Proc`, `lambda`, and `->` syntax; arity differences and leveraging `#curry` for composable functions.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Behavior</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>lambda</td><td>Strict arity</td><td>Raises ArgumentError on mismatch</td></tr>
    <tr><td>proc</td><td>Lenient arity</td><td>Useful for varargs adapters</td></tr>
    <tr><td>curry</td><td>Partial application</td><td>Useful in functional pipelines</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```ruby
adder = ->(a, b) { a + b }
add1 = adder.curry[1]
add1[2] # => 3

p = proc { |x, y| puts x }
```

### Exercises (Appendix — lambdas-ruby2)

1. Convert a chain of small functions into curried lambdas and test composition.
2. Demonstrate arity differences between `lambda` and `proc` with tests that assert behavior on wrong argument counts.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
