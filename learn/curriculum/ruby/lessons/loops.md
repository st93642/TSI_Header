# Loops and Iteration

Iteration is the heartbeat of automation: we repeat operations to transform data, poll services, retry flaky work, or generate reports. Ruby offers a rich toolkit that balances expressiveness with safety, from classic `while` loops to lazy enumerators. This lesson explores each option, when to use it, and how to write loops that are both efficient and easy to read.

## Learning goals

- Choose the most expressive looping construct for a task.
- Control repetition with `while`, `until`, `loop`, numeric iterators, and
  collection methods.
- Use `break`, `next`, `redo`, and `throw`/`catch` to steer control flow
  precisely.
- Leverage enumerators (`each`, `each_with_index`, `Enumerator::Lazy`) for
  composable pipelines.
- Refactor nested or long-running loops to remain maintainable and performant.

## Choosing the right loop

Ask these questions before writing a loop:

1. **Is the number of repetitions known?** Prefer numeric iterators
   (`Integer#times`, `upto`, `downto`, `step`).
2. **Am I traversing a collection?** Reach for `Enumerable` methods (`each`,
`map`, `each_with_index`).
3. **Do I need a manual condition?** Use `while`/`until` with clear exit
   criteria.
4. **Do I need an infinite loop with manual exits?** Use `Kernel#loop` with
`break`.
5. **Am I building a chain of transformations?** Create an `Enumerator` or use
   lazy evaluation.

## Foundational loops: `while` and `until`

`while` executes its block while the condition is truthy. Always ensure that the condition eventually becomes falsy.

```ruby
count = 1

while count <= 5
  puts "Count: #{count}"
  count += 1
end
```

`until` is the inverse: it runs until the condition becomes truthy.

```ruby
count = 1

until count > 5
  puts "Count: #{count}"
  count += 1
end
```

Prefer meaningful predicates (`while invoices.pending?`) rather than cryptic counters when possible.

### Guarding against infinite loops

Inline safety checks with `break` or `raise` to prevent runaway processes, especially when waiting for external resources.

```ruby
attempts = 0

while attempts < 5
  response = http_client.get(url)
  break if response.success?

  attempts += 1
  sleep(2**attempts)
end

raise "Service unavailable" unless response&.success?
```

## `loop` for explicit infinite iteration

`Kernel#loop` creates an infinite loop that you manually exit. Combine it with `break`, `next`, or `throw` for fine-grained control.

```ruby
loop do
  print "Enter command (quit to exit): "
  input = gets&.chomp
  break if input == "quit" || input.nil?

  puts "You entered: #{input}"
end
```

You can supply a value to `break` to make the loop expression return meaningful data:

```ruby
result = loop do
  token = queue.pop
  break token if token.valid?
end

puts "Processing #{result}"  # => uses the value passed to break
```

## Numeric iterators: `times`, `upto`, `downto`, `step`

Integers enhance readability when the iteration count is clear.

```ruby
5.times do |i|
  puts "Iteration #{i}"  # i starts at 0
end

1.upto(5) { |n| puts n }
10.downto(1) { |n| puts "T-minus #{n}" }
(0..10).step(2) { |even| puts even }
```

These methods return the starting integer, allowing method chaining with
caution:

```ruby
total = 0
10.times.reduce(total) { |acc, _| acc + 1 }  # => 10
```

## Iterating collections: `each` and friends

Most Ruby code iterates using `Enumerable` methods. Here’s an idiomatic traversal with contextual metadata:

```ruby
orders = [
  { id: 1, total: 49.99 },
  { id: 2, total: 120.00 },
  { id: 3, total: 15.25 }
]

orders.each_with_index do |order, index|
  puts "##{index + 1} order ##{order[:id]} => $#{order[:total]}"
end
```

Prefer `each_with_index` over manual counters. For nested structures, chain enumerators or use `flat_map` to keep depth manageable.

### External enumerators

Calling `to_enum` (or the shorthand `enum_for`) converts an iterator into an `Enumerator` object you can traverse manually.

```ruby
enumerator = orders.to_enum

loop do
  order = enumerator.next
  puts "Processing ##{order[:id]}"
end
rescue StopIteration
  puts "All orders processed."
end
```

External iteration is handy when coordinating multiple collections or
pausing/resuming work.

### Lazy enumeration

Large or infinite sequences benefit from `Enumerator::Lazy`, which defers execution until values are consumed.

```ruby
primes = (2..).lazy.select { |n| prime?(n) }

puts primes.take(10).force.inspect
```

Use lazy enumerators to stream files, paginate APIs, or process big datasets
without loading everything into memory.

## Loop control keywords

- `break` exits the loop immediately and can return a value.
- `next` skips to the next iteration without exiting the loop.
- `redo` restarts the current iteration without reevaluating the condition (use
  sparingly).
- `throw`/`catch` provide labeled exits for nested loops.

```ruby
def fetch_first_matching(records)
  catch(:found) do
    records.each do |record|
      next unless record.ready?
      throw(:found, record) if record.valid?
    end
  end
end
```

Be mindful: `redo` can easily create infinite loops if state is not mutated.

## Loop modifiers and one-liners

Inline loops improve brevity for simple cases.

```ruby
puts count += 1 while count < 5
puts count -= 1 until count.zero?
```

Reserve one-liners for truly simple expressions. Multiline loops with clear
indentation remain easier to debug.

## Nested loops and refactoring strategies

Nested loops crop up in matrix operations, reporting, and pairing data. Tame complexity by extracting helper methods, using `product`, or leveraging hashes as lookup tables.

```ruby
products = ["Laptop", "Phone", "Tablet"]
regions = ["NA", "EU", "APAC"]

products.product(regions).each do |product, region|
  puts "Forecasting #{product} in #{region}"
end
```

Break apart deeply nested loops by delegating work to smaller methods or using enumerator combinators (`zip`, `flat_map`).

## Performance and memory considerations

- Prefer lazy enumeration for huge datasets or IO streams.
- Avoid building large intermediate arrays when chaining; use `each` plus
  immediate work, or `lazy`.
- For CPU-bound loops, benchmark (`Benchmark.measure`) to compare `times` vs
  `each` vs `while`.
- Extract pure computations (like regex, math) outside the loop to avoid
  repeated allocation.

```ruby
require "benchmark"

Benchmark.bm do |bm|
  bm.report("times") { 1_000_000.times { |n| n + 1 } }
  bm.report("while") do
    i = 0
    while i < 1_000_000
      i += 1
    end
  end
end
```

## Real-world patterns

### Polling with backoff

```ruby
max_attempts = 5

max_attempts.times do |attempt|
  response = api_client.fetch
  break process(response) if response.success?

  sleep(2**attempt)
end
```

### Paginating API results

```ruby
results = []
page = 1

loop do
  batch = api.fetch(page: page)
  break if batch.empty?

  results.concat(batch)
  page += 1
end
```

### Stream processing with lazy enumerators

```ruby
File.foreach("logs/access.log")
    .lazy
    .select { |line| line.include?("ERROR") }
    .take(20)
    .each { |line| puts line }
```

## Guided practice

1. **Inventory reconciliation**
   - Iterate through a `products` array, each with `:expected` and `:actual`
     counts.
   - Print discrepancies and accumulate the total difference.
   - Exit early if a product is missing (`:actual` is `nil`) and log a warning.

2. **Retry with exponential backoff**
   - Write `attempt_with_retry(max_attempts: 5)` that yields to a block.
   - Retry until the block returns truthy or attempts are exhausted.
   - Use `sleep(2**attempt)` between tries and return the successful value via
     `break`.

3. **Chunked processing**
   - Given an array of 10,000 integers, process them in slices of 250.
   - Use `each_slice` to handle a chunk, compute its sum, and append to
     `chunk_sums`.
   - Verify that the total of `chunk_sums` matches the sum of the original
     array.

4. **Lazy Fibonacci generator**
   - Implement `fibonacci = Enumerator.new { |y| ... }` to generate the
     sequence.
   - Use `lazy` to stream the first 15 numbers greater than 1,000.

5. **Matrix transposition**
   - Build `transpose(matrix)` using nested loops or `zip`.
   - Ensure the function works for square and rectangular matrices.

## Self-check questions

1. When would you choose `while` over `loop { ... break }`?
2. How does `break value` influence the return value of a loop expression?
3. Why might `Enumerator::Lazy` be preferable when reading very large files?
4. How can you exit two nested loops without using a flag variable?
5. What are the trade-offs between inline loop modifiers and multiline loops?

Loops are everywhere in Ruby—data pipelines, background jobs, command-line
tools, even inside framework internals. Mastery comes from choosing the right
construct, naming intent clearly, and building guardrails against infinite runs
or runaway memory. Practice the scenarios above, experiment with enumerators,
and profile long-running code to keep your iterations lean and expressive.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Loops — Safety & Backoff Strategies (Appendix — loops-ruby-safety)

Guidance for safe loops in production: limit retries, add timeouts, and avoid busy-waiting.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>When</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr><td>Exponential backoff</td><td>Retrying flaky IO</td><td>`sleep(2**attempt)`</td></tr>
    <tr><td>Timeouting loops</td><td>Waiting for resource</td><td>Use `Timeout.timeout(seconds) { ... }`</td></tr>
    <tr><td>Throttling</td><td>Rate-limited APIs</td><td>Sleep between batches or use token buckets</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Safety checklist

- Always bound retries and add circuit-breaker logic.
- Prefer `sleep` with jitter to avoid thundering herds.
- Use `Enumerator::Lazy` for large streams to reduce memory.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD013 -->
## Practical Appendix: Loops — Enumerator Performance & Parallelization (Appendix — loops-ruby2)

Performance-minded loop patterns: using `each`, `map`, `reduce`, `Enumerator::Lazy`, and basic parallelization hints with threads/fibers.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>each/map</td><td>Transform collections</td><td>Prefer `map` when collecting results</td></tr>
    <tr><td>lazy</td><td>Large sequences</td><td>Avoid materializing large arrays</td></tr>
    <tr><td>parallel</td><td>IO-bound work</td><td>Use threads or external pools cautiously</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples

```ruby
# lazy chain
(1..1_000_000).lazy.select(&:odd?).first(10)

# simple parallel (IO-bound)
threads = urls.map do |u|
  Thread.new { fetch(u) }
end
threads.each(&:join)
```

### Exercises (Appendix — loops-ruby2)

1. Benchmark `map` vs manual `each <<` for building arrays and report
   allocations.
2. Implement a simple thread pool to perform IO-bound fetches and test
   concurrency correctness.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Loops — Choosing Iterators & Performance (Appendix — loops-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Iterator</th><th>When</th><th>Note</th></tr>
  </thead>
  <tbody>
    <tr><td>`each`</td><td>Default enumerable iteration</td><td>Readable and idiomatic</td></tr>
    <tr><td>`map`</td><td>Transform to new array</td><td>Prefer over `each` + `<<` when creating collection</td></tr>
    <tr><td>`while`/`until`</td><td>Stateful loops</td><td>Use carefully; prefer enumerables when possible</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Tips

- Use `lazy` enumerators for large streams to avoid memory spikes.
- Use `times` for fixed-count loops; it communicates intent.

### Exercises

1. Convert a loop that mutates an array into a `map` and compare readability and
   performance.
2. Implement a lazy pipeline over a large generator and compare memory usage.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Loops — Lazy Pipelines & Control Flow Patterns (Appendix — loops-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>When</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr><td>Lazy enumerators</td><td>Large/streaming data</td><td>Use `lazy` to avoid building intermediates</td></tr>
    <tr><td>`each_with_object`</td><td>Build hashes/arrays</td><td>Prefer over `inject` for mutable accumulators</td></tr>
    <tr><td>`break` value</td><td>Return from loop</td><td>Use `break value` to make loops return meaningful results</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Appendix — Examples

```ruby
# Lazy example
result = File.foreach("big.log").lazy.map { |l| parse(l) }.select(&:valid?).first(10)

# each_with_object
pairs.each_with_object({}) { |(k, v), h| h[k] ||= []; h[k] << v }

# break with value
value = loop do
  item = queue.pop
  break item if item.ready?
end
```

### Appendix — Exercises

1. Replace a `map.flatten.uniq` pipeline with a `flat_map` or `lazy` pipeline
   and add a micro-benchmark.
2. Refactor a nested loop into `product` or `zip` usage and add a test that
   asserts equivalence.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
