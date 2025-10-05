# Lambdas in Ruby

Lambdas are anonymous functions you can store in variables, pass around, and evaluate on demand. They power callbacks and functional pipelines, and let you inject customizable behavior without creating full classes. While lambdas share DNA with `Proc` objects, they enforce stricter argument rules and respect method-return semantics. This makes them a good fit for predictable, function-like code.

## Learning goals

- Create lambdas using both `lambda` and stabby (`->`) syntaxes. Support
  positional, optional, and keyword parameters.

- Compare lambdas with procs, especially around arity checking and
`return` semantics.

- Compose lambdas with enumerables, higher-order functions, and partial
  application techniques.

- Capture state safely via closures, memoization, and currying.

- Integrate lambdas into object-oriented designs for strategies, policies, and
  event hooks.

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

```ruby
items.each_with_index do |item, index|
  puts format("%2d. %s", index + 1, item)
end

items

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

- Lambdas enforce arity: calling without the required arguments raises
`ArgumentError`.
- Use defaults and keyword arguments to provide flexible interfaces.
- Destructure parameters with pattern matching:

```ruby
process = ->((status, payload)) { puts "#{status}: #{payload}" }
process.call(["ok", { id: 1 }])
```

## Lambdas vs. Procs

<!-- markdownlint-disable MD033 MD013 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Lambda</th><th>Proc</th></tr>
  </thead>
  <tbody>
    <tr><td>Arity enforcement</td><td>Strict (<code>ArgumentError</code> on mismatch)</td><td>Lenient (missing args become <code>nil</code>)</td></tr>
    <tr><td>Return semantics</td><td><code>return</code> exits lambda only</td><td><code>return</code> exits enclosing method</td></tr>
    <tr><td><code>next`/`break</code></td><td>Behave like in blocks (exit lambda)</td><td>Exit enclosing method</td></tr>
    <tr><td>Typical use</td><td>Functional helpers, callbacks</td><td>Flexible, legacy APIs, DSLs</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD013 -->

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

Choose lambdas when you want self-contained control flow that won’t unexpectedly
exit callers.

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

Lambdas allow behavior injection into methods — great for strategy patterns and
configurable pipelines.

## Lambdas with enumerables

```ruby
numbers = [1, 2, 3, 4, 5]

square  = ->(n) { n * n }
is_odd  = ->(n) { n.odd? }
display = ->(n) { puts "n = #{n}" }

numbers.map(&square).select(&is_odd).each(&display)
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

Each lambda retains its own enclosed state — useful for memoization, rate
limiting, or incremental IDs.

## Currying and partial application

```ruby
adder = ->(a, b, c) { a + b + c }
curried = adder.curry

add_five = curried.call(2, 3)
```

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

Composition keeps transformations modular. Combine with enumerables for powerful
data processing pipelines.

## Memoization pattern

```ruby
def memoize(fn)
  cache = {}
  ->(*args) do
    cache.fetch(args) { cache[args] = fn.call(*args) }
  end
end
```

<!-- markdownlint-disable MD033 MD034 MD040 MD010 MD013 -->

## Practical Appendix: Lambdas — Debugging & Composition Tips (Appendix — lambdas-ruby2)

Best practices for composing lambdas, debugging closures, and avoiding
surprising `return` semantics.

<!-- markdownlint-disable MD033 MD013 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Pattern</th><th>Note</th></tr>
  </thead>
  <tbody>
    <tr><td>Unexpected return</td><td>Use lambda not proc</td><td>Lambdas isolate returns from surrounding methods</td></tr>
    <tr><td>State leaks</td><td>Avoid mutable closures</td><td>Prefer immutable captured values or dup copies</td></tr>
    <tr><td>Composition</td><td>Use `compose` helper</td><td>Keep small focused transformations</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Debug tip

Wrap lambda calls with logging during development to inspect captured state:

```ruby
debugger = ->(name, fn) { ->(*args) { puts "#{name} => #{args.inspect}"; fn.call(*args) } }
```

```ruby
slow_fib = ->(n) { n < 2 ? n : slow_fib.call(n - 1) + slow_fib.call(n - 2) }
fast_fib = memoize(slow_fib)

fast_fib.call(35) # much faster than raw recursion
```

Memoization caches results per argument tuple, demonstrating lambdas’ ability to
capture state and wrap behavior.

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

Lambdas inject policies without subclassing — ideal for strategies, guards, or
feature toggles.

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

- Lambdas are lightweight but not free; avoid creating new ones inside tight
  loops if you can reuse an existing instance.
- Prefer lambdas over methods when behavior truly needs to be dynamic or passed
  around — otherwise define a private method.
- Instrument with `Benchmark.measure` if performance is critical; compare
  lambdas, methods, and direct blocks to choose the best fit.

## When to choose lambdas

Use lambdas when you need:

- Callbacks or hooks configurable at runtime.
- Functional transformations (map/filter/reduce) with predictable arity.
- Encapsulated state via closures (counters, memoization, lazy evaluation).
- Strategy objects without extra classes.
- Composition pipelines where each step is a reusable function.

Reach for procs or blocks when you require lenient arity or DSL-style behavior
that manipulates the surrounding scope.

## Guided practice

1. **Pipeline builder**
   - Implement `pipeline(*steps)` returning a lambda that threads a value
     through each step.
   - Provide lambdas for trimming strings, squashing whitespace, and adding
     suffixes.
   - Verify the pipeline handles both strings and arrays of strings.

1. **Retry wrapper**
   - Write `with_retries(retries:, delay:)` that accepts a lambda and retries it
     when it raises a transient error.
   - Capture exceptions, sleep between attempts, and re-raise after exhausting
     retries.

1. **Currying calculator**
   - Create a curried lambda `weighted_average` that first accepts weights, then
     values.
   - Ensure partial application works (e.g., `grades_average =
     weighted_average.call([0.4, 0.6])`).

1. **Strategy registry**
   - Build a hash mapping symbols to lambdas (e.g., `:json`, `:yaml`,
`:csv`).
   - Write `serializer_for(format)` that returns the lambda and raises on
     unsupported formats.
   - Demonstrate with sample payloads.

1. **Closure-based rate limiter**
   - Implement `rate_limiter(limit:, interval:)` returning a lambda that yields
     `true` when allowed and `false` when throttled.
   - Keep timestamps in the closure and prune stale entries.

## Self-check questions

1. How do lambdas differ from procs with respect to argument checking and
`return` statements?
2. What advantages do closures provide when capturing external variables inside
   lambdas, and how can this lead to stateful behavior?
3. How does currying change the way you call a lambda, and when is partial
   application more appropriate than currying?
4. Why might you pass lambdas into object constructors instead of subclassing or
   using conditionals?
5. What patterns help avoid performance or readability issues when you introduce
   lambdas into a codebase?

Lambdas give you a middle ground between single-use blocks and full classes —
embrace them for clean, composable, expressive Ruby code.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 MD013 -->

## Practical Appendix: Lambdas vs Procs — Gotchas & Best Practices (Appendix — lambdas-appendix)

<!-- markdownlint-disable MD033 MD013 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Lambda</th><th>Proc</th></tr>
  </thead>
  <tbody>
    <tr><td>Arity</td><td>Checks arity (raises if mismatch)</td><td>Lenient arity</td></tr>
    <tr><td>Return</td><td>Returns from lambda only</td><td>`return` in a proc returns from enclosing method</td></tr>
    <tr><td>Definition</td><td>`->(x) {}` or `lambda {}`</td><td>`proc {}` or `Proc.new`</td></tr>
    <tr><td>Style</td><td>Prefer lambdas for function-like blocks</td><td>Use procs for callbacks where leniency is helpful</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```ruby
l = ->(x) { return x * 2 }
p = proc { return :from_proc }

def wrapper
  l.call(2)   # returns to caller of l, not from wrapper
  p.call       # would return from wrapper if executed here
  :after
end

puts wrapper.inspect
```

<!-- markdownlint-disable MD013 -->
### Appendix — Exercises

1. Create two higher-order functions that accept a lambda and a proc and observe
   arity/return differences.
2. Rewrite a `method_missing` handler using `define_method` and `public_send` to
   avoid fragile `method_missing` metaprogramming.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- Additional Practical Appendix: Lambdas — Hidden Tips (Appendix — lambdas-hidden-20251005b) -->

<!-- markdownlint-disable MD033 MD013 -->
<table>
  <thead>
    <tr><th>Tip</th><th>Why</th><th>Quick code</th></tr>
  </thead>
  <tbody>
    <tr><td>Reuse lambdas in hot loops</td><td>Avoid allocation costs</td><td>`sq = ->(n) { n*n }; arr.map(&sq)`</td></tr>
    <tr><td>Avoid `return` in procs</td><td>Procs can unexpectedly exit callers</td><td>Prefer lambdas for local returns</td></tr>
    <tr><td>Check `arity` when integrating</td><td>Detect mismatched collaborators early</td><td>`raise unless fn.arity.abs <= expected`</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-enable MD013 -->
### Tiny helper: compose (appendix)

```ruby
def compose(*fns)
  ->(arg) { fns.reverse.reduce(arg) { |acc, f| f.call(acc) } }
end
```

### Exercises — lambdas-hidden-20251005b

1. Refactor an example to hoist lambda definitions out of loops and benchmark
   the memory change.
2. Add an `arity` check helper and write tests demonstrating it rejects
   incompatible lambdas.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 MD013 -->

## Practical Appendix: Lambda Conventions (Appendix — lambdas-calling-20251005)

Insider insights into lambda invocation styles, currying pitfalls, and composition tricks from Ruby's functional toolkit.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Tip</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr><td>Calling styles</td><td>Multiple call styles: .call, [], (), yield</td><td>`fn.call(1) == fn[1] == fn.(1)`</td></tr>
    <tr><td>Currying</td><td>Partial application with .curry; specify arity for var args</td><td>`add.curry.call(1).call(2) == 3`</td></tr>
    <tr><td>Composition</td><td><< and >> for chaining; f << g means g then f</td><td>`(double << inc).call(3) == 8`</td></tr>
    <tr><td>Anonymous params</td><td>_1, _2 for short blocks; can't mix with it or explicit params</td><td>`[1,2,3].map { _1**2 }`</td></tr>
    <tr><td>Arity gotcha</td><td>Lambdas raise ArgumentError on mismatch; check .arity first</td><td>`lambda{}.arity == 0`</td></tr>
    <tr><td>Return semantics</td><td>return exits lambda, not method; lambdas can't be orphaned</td><td>`-> { return 42 }.call == 42`</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises

1. Implement a curried lambda for string formatting that first takes a template,
   then values, and handles variable args.
2. Chain two lambdas using `<<` and `>>` to process an array: filter evens, then
   square them.
3. Write a helper that checks lambda arity and raises a custom error if
   incompatible with expected args.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
