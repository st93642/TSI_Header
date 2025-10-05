# Iterators in Ruby

Iterators power Ruby’s expressive style. Instead of manual index juggling, you let objects yield elements while you focus on intent: transform, filter, group, or short-circuit. This lesson dives deep into iterator patterns, advanced `Enumerable` helpers, enumerators, and practical workflows that keep data pipelines elegant and efficient.

## Learning goals

- Understand how `Enumerable` underpins most iteration patterns.
- Choose the right iterator for traversal, transformation, filtering,
  aggregation, and grouping.
- Chain iterators without sacrificing readability by leveraging blocks, procs,
  and external enumerators.
- Use advanced helpers (`partition`, `group_by`, `tally`, `each_with_object`,
  `chunk_while`, lazy enumerators) to solve real problems.
- Guard against performance pitfalls and write iterator-friendly APIs for your
  own classes.

## The iteration contract

Any object that implements the method `each` can mix in `Enumerable` and instantly gain dozens of iterator helpers. `each` must yield item(s) to a block.

```ruby
class TicketQueue
  include Enumerable

  def initialize(tickets)
    @tickets = tickets
  end

  def each
    return enum_for(:each) unless block_given?

    @tickets.each { |ticket| yield ticket }
  end
end

queue = TicketQueue.new(%w[T-101 T-102 T-103])
queue.map { |id| id.downcase }  # => ["t-101", "t-102", "t-103"]
```

Providing `enum_for` allows external iteration when no block is given, a best practice for custom enumerables.

## Traversal: `each`, `each_with_index`, `each_slice`, `each_cons`

- `each`: baseline traversal for arrays, hashes, ranges, sets, and custom
  collections.
- `each_with_index`: pairs elements with zero-based indices.
- `each_slice(n)`: yields groups of `n` elements—great for batch processing.
- `each_cons(n)`: yields consecutive runs (windows) of size `n`.

```ruby
events = %w[login view purchase logout]

events.each_with_index { |event, i| puts "#{i + 1}. #{event}" }

events.each_slice(2) { |pair| p pair }
# => ["login", "view"], then ["purchase", "logout"]

events.each_cons(3) { |window| p window }
# => ["login", "view", "purchase"], then ["view", "purchase", "logout"]
```

Use `each_slice` for pagination or API chunking, and `each_cons` to detect sequences (e.g., three failed logins in a row).

## Transformation pipelines: `map`, `map!`, `flat_map`, `zip`

- `map` returns a new array with transformed values. `map!` mutates in place.
- `flat_map` (a.k.a. `collect_concat`) maps and flattens one level, perfect for
  one-to-many transformations.
- `zip` combines multiple enumerables element by element.

```ruby
users = [
  { name: "Ava", roles: %w[admin] },
  { name: "Ben", roles: %w[editor moderator] }
]

roles = users.flat_map { |user| user[:roles] }
# => ["admin", "editor", "moderator"]

names = users.map { |user| user[:name].downcase }

ids = [1, 2, 3]
values = %w[a b c]
p ids.zip(values)
# => [[1, "a"], [2, "b"], [3, "c"]]
```

`flat_map` avoids intermediate arrays from `map + flatten`, making it memory-friendly for large datasets.

## Filtering and partitioning: `select`, `reject`, `drop_while`, `take_while`, `partition`

Combine filters to express data rules declaratively.

```ruby
transactions = [120, 0, 45, -20, 5, 999]

positive = transactions.select(&:positive?)
small_refunds, charges = transactions.partition { |amount| amount.negative? }

first_charges = transactions.take_while { |amount| amount >= 0 }
remaining = transactions.drop_while { |amount| amount >= 0 }
```

`partition` returns a two-element array `[matches, non_matches]`, handy for quickly splitting datasets.

## Aggregation: `reduce`, `sum`, `count`, `each_with_object`

- `reduce` (aka `inject`) threads an accumulator through the collection.
- `sum` and `count` (Ruby 2.4+) provide optimized shortcuts.
- `each_with_object(initial)` is perfect for building hashes or arrays without
  returning the visited collection.

```ruby
orders = [
  { total: 49.99, status: :paid },
  { total: 18.50, status: :refunded },
  { total: 75.00, status: :paid }
]

revenue = orders.sum { |order| order[:status] == :paid ? order[:total] : 0 }

counts = orders.each_with_object(Hash.new(0)) do |order, acc|
  acc[order[:status]] += 1
end
# => { paid: 2, refunded: 1 }

product = [2, 3, 5].reduce(1, :*)  # => 30
```

`each_with_object` is preferable to `reduce` when the accumulator is a mutable object like a hash or array.

## Searching: `find`, `detect`, `find_index`, `bsearch`

`find`/`detect` return the first element satisfying the block; `find_index` returns its index. For sorted collections, `bsearch` performs binary search.

```ruby
jobs = [
  { id: 1, status: :pending },
  { id: 2, status: :running },
  { id: 3, status: :running }
]

running_job = jobs.find { |job| job[:status] == :running }
running_id = jobs.find_index { |job| job[:status] == :running }

weights = [1, 4, 9, 16, 25]
perfect_square = weights.bsearch { |w| Math.sqrt(w) == Math.sqrt(w).round }  # => 1
```

`bsearch` accepts different block styles (`find-min` vs `find-any`); consult docs when operating on sorted ranges.

## Grouping, tallying, and chunking

- `group_by` buckets elements by a computed key.
- `tally` counts occurrences, returning a hash mapping elements to frequencies.
- `chunk` and `chunk_while` collect consecutive elements sharing a property.

```ruby
logs = [
  { level: :info, message: "boot" },
  { level: :warn, message: "disk" },
  { level: :info, message: "ready" }
]

by_level = logs.group_by { |log| log[:level] }
counts = logs.tally { |log| log[:level] }

temperatures = [20, 21, 22, 30, 31, 18, 19]
heatwaves = temperatures.chunk_while { |a, b| (b - a).abs <= 1 }.to_a
# => [[20, 21, 22], [30, 31], [18, 19]]
```

Use `group_by` to feed reporting dashboards and `chunk_while` to segment time-series data based on deltas.

## Enumerator objects and external iteration

Many iterator methods return an `Enumerator` when called without a block, enabling advanced pipelines.

```ruby
evens = (1..).lazy.select(&:even?).map { |n| n**2 }
puts evens.first(5)  # => [4, 16, 36, 64, 100]

iterator = ["a", "b", "c"].each  # external enumerator

loop do
  puts iterator.next
end
rescue StopIteration
  puts "Done"
```

External enumerators let you pause/resume iteration, merge multiple sequences,
or coordinate asynchronous workflows.

## Enumerator composition and laziness

Use `Enumerator.new` to define custom lazily-evaluated sequences.

```ruby
fibonacci = Enumerator.new do |y|
  a = b = 1
  loop do
    y << a
    a, b = b, a + b
  end
end

puts fibonacci.lazy.select { |n| n.even? }.take(5).force.inspect
# => [2, 8, 34, 144, 610]
```

Laziness prevents precomputing the entire sequence, perfect for infinite streams
or large file processing.

## Writing iterator-friendly APIs

When designing methods, accept blocks and yield results rather than returning
precomputed arrays. Provide fallback enumerators so callers can choose their
style.

```ruby
class LogReader
  include Enumerable

  def initialize(path)
    @path = path
  end

  def each
    return enum_for(:each) unless block_given?

    File.foreach(@path, chomp: true) do |line|
      yield parse(line)
    end
  end

  private

  def parse(line)
    level, message = line.split(" :: ")
    { level: level.to_sym, message: message }
  end
end

reader = LogReader.new("logs/app.log")
reader.select { |entry| entry[:level] == :error }
```

This approach ensures interoperability with the whole `Enumerable` suite.

## Performance tips

- Avoid chaining that materializes large intermediate arrays; use `lazy` or
  `each` with immediate side effects.
- Prefer built-in iterator methods over manual loops—they’re optimized in C.
- For numeric summations, use `sum` instead of `reduce(:+)` for clarity and
  speed.
- Memoize expensive computations outside of loops to avoid redundant work inside
  blocks.
- Profile with `benchmark-ips` or `StackProf` when loops dominate runtime.

```ruby
require "benchmark/ips"

numbers = Array.new(1_000) { rand(10_000) }

Benchmark.ips do |x|
  x.report("map + flatten") { numbers.map { |n| [n, n**2] }.flatten(1) }
  x.report("flat_map") { numbers.flat_map { |n| [n, n**2] } }
  x.compare!
end
```

## Guided practice

1. **Audit trail summarizer**
   - Given an array of audit entries, group them by `:user_id`, count actions
     per user, and produce a report sorted by action count.
   - Use `group_by`, `transform_values`, and `sort_by`.

2. **SKU expansion**
   - Start with a list of base SKUs and a mapping of suffixes. Use `flat_map` to
     generate the full SKU list, then `tally` to verify duplicates.

3. **Sliding window alerts**
   - Implement `spikes(data, threshold)` that returns indices where the average
     of a 3-element moving window exceeds the threshold.
   - Use `each_cons` combined with `with_index`.

4. **Chunked file upload**
   - Read a binary file in 1 KB slices with `each_slice`. For each slice,
     simulate an upload call and stop when a simulated failure occurs, returning
     the number of successful chunks.

5. **Lazy prime stream**
   - Build a lazy enumerator that yields prime numbers. Use `take` to fetch the
     first 25 primes greater than 1,000 and sum them.

## Self-check questions

1. Why is it important to return `enum_for(:each)` when an iterator is called
   without a block?
2. When would `flat_map` be more appropriate than `map` followed by `flatten`?
3. How does `each_with_object` differ from `reduce` in terms of mutating
   accumulators?
4. What advantages do lazy enumerators provide when processing large logs or API
   streams?
5. How can `partition` simplify code that used to rely on two separate
`select`/`reject` operations?

Iterators are the idiomatic way to express loops in Ruby. Mastering them lets
you replace ten lines of index juggling with a single, intention-revealing
method call. Combine traversal helpers, aggregators, and lazy streams to build
data transformations that read like plain English while staying performant.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Iterators & Enumerator Recipes (Appendix — iterators-appendix)

<!-- markdownlint-disable MD033 MD013 -->
<table>
  <thead>
    <tr><th>Feature</th><th>When</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr>
      <td>`each_with_index`</td>
      <td>Need index during iteration</td>
      <td>Use destructuring: `each_with_index { |v, i| ... }`</td>
    </tr>
    <tr>
      <td>`each_slice`/`each_cons`</td>
      <td>Windowed processing</td>
      <td>`each_slice(3)` for batching</td>
    </tr>
    <tr>
      <td>`Enumerator` / `lazy`</td>
      <td>Streams / large datasets</td>
      <td>Call `lazy` before heavy transforms</td>
    </tr>
    <tr>
      <td>`&:symbol`</td>
      <td>Short single-method block</td>
      <td>Use when succinct and readable; avoid when arguments differ</td>
    </tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD013 -->

### Examples

```ruby
# Batching
(1..10).each_slice(3) { |s| p s }

# Sliding window
[1,2,3,4].each_cons(2) { |a,b| p a+b }

# Lazy pipeline
(1..Float::INFINITY).lazy.map { |i| expensive(i) }
  .select(&:positive?).first(5)

# Custom enumerator
enum = Enumerator.new do |y|
  (1..5).each { |i| y << i*i }
end
enum.map { |x| x + 1 }
```

<!-- markdownlint-disable MD013 -->
### Appendix — Exercises

1. Implement a small CSV processor that uses `lazy` to find the first 20 rows
   matching a predicate without reading the whole file.
2. Implement a custom `Enumerator.new` that yields prime numbers and test the
   first 10 primes.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Advanced Iterator Methods & Hidden Gems

(Appendix — iterators-advanced-20251005)

Discover lesser-known iterator methods from Ruby's `Enumerable` module for powerful data manipulation.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Description</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr>
      <td>`each_entry`</td>
      <td>Converts multiple values from yield to arrays</td>
      <td>Useful for custom `each` yielding multiple args</td>
    </tr>
    <tr>
      <td>`reverse_each`</td>
      <td>Iterates in reverse order</td>
      <td>Efficient for stacks or reverse processing</td>
    </tr>
    <tr>
      <td>`cycle`</td>
      <td>Infinite cycling of elements</td>
      <td>Use with `take` to limit cycles</td>
    </tr>
    <tr>
      <td>`chain`</td>
      <td>Chains multiple enumerables</td>
      <td>Merges sequences without concatenation</td>
    </tr>
    <tr>
      <td>`chunk`</td>
      <td>Groups consecutive elements by block</td>
      <td>Special symbols like `:_alone` for isolation</td>
    </tr>
    <tr>
      <td>`chunk_while`</td>
      <td>Chunks based on adjacent pairs</td>
      <td>Opposite of `slice_when`</td>
    </tr>
    <tr>
      <td>`slice_after`</td>
      <td>Partitions after matching elements</td>
      <td>Great for parsing delimited data</td>
    </tr>
    <tr>
      <td>`slice_before`</td>
      <td>Partitions before matching elements</td>
      <td>Handles complex separators</td>
    </tr>
    <tr>
      <td>`slice_when`</td>
      <td>Splits when block returns true</td>
      <td>Complements `chunk_while`</td>
    </tr>
    <tr>
      <td>`tally`</td>
      <td>Counts occurrences of elements</td>
      <td>Faster than manual hashing</td>
    </tr>
    <tr>
      <td>`compact`</td>
      <td>Removes `nil` elements</td>
      <td>Shortcut for `reject(&:nil?)`</td>
    </tr>
    <tr>
      <td>`filter_map`</td>
      <td>Maps and filters in one pass</td>
      <td>Avoids intermediate arrays</td>
    </tr>
    <tr>
      <td>`sum`</td>
      <td>Optimized summation</td>
      <td>Handles initial values and blocks</td>
    </tr>
    <tr>
      <td>`zip`</td>
      <td>Combines enumerables element-wise</td>
      <td>Fills with `nil` for uneven lengths</td>
    </tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Advanced Examples

```ruby
# each_entry for multi-yield
class MultiYield
  include Enumerable
  def each
    yield 1, 2
    yield 3
  end
end
MultiYield.new.each_entry { |e| p e }  # [1,2] then 3

# reverse_each
[1,2,3].reverse_each { |x| print x }  # 321

# cycle with take
[1,2].cycle.take(5)  # [1,2,1,2,1]

# chain
a = [1,2]
b = [3,4]
a.chain(b).to_a  # [1,2,3,4]

# chunk with :_alone
[0,0,1,1].chunk { |i| i.even? ? :_alone : true }.to_a
# [[:_alone, [0]], [:_alone, [0]],
#  [true, [1,1]]]

# tally
[1,2,2,3].tally  # {1=>1, 2=>2, 3=>1}

# filter_map
[1,2,3].filter_map { |x| x*2 if x.odd? }  # [2,6]

# sum with block
[1,2,3].sum { |x| x**2 }  # 14

# zip
[1,2].zip(['a','b','c'])
# [[1,'a'], [2,'b'], [nil,'c']]
```

### Exercises

1. Use `each_entry` to handle a custom enumerable that yields multiple values
   per iteration.
2. Implement a function that uses `chunk_while` to group consecutive numbers
   differing by at most 1.
3. Write a script that uses `tally` to count word frequencies in a text file.
4. Experiment with `slice_before` to parse a log file into entries starting with
   timestamps.

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Enumerator Features (Appendix — iterators-external-20251005)

Explore Ruby's Enumerator for external iteration, lazy evaluation, and chaining.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Feature</th><th>Tip</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr>
      <td><code>next</code></td>
      <td>External iteration</td>
      <td><code>enum.next</code></td>
    </tr>
    <tr>
      <td><code>peek</code></td>
      <td>Look ahead without advancing</td>
      <td><code>enum.peek</code></td>
    </tr>
    <tr>
      <td><code>feed</code></td>
      <td>Send value to yield</td>
      <td><code>enum.feed(value)</code></td>
    </tr>
    <tr>
      <td><code>rewind</code></td>
      <td>Reset to beginning</td>
      <td><code>enum.rewind</code></td>
    </tr>
    <tr>
      <td><code>with_index</code></td>
      <td>Add index to iteration</td>
      <td><code>enum.with_index</code></td>
    </tr>
    <tr>
      <td><code>with_object</code></td>
      <td>Carry state object</td>
      <td><code>enum.with_object(obj)</code></td>
    </tr>
    <tr>
      <td><code>produce</code></td>
      <td>Infinite sequences</td>
      <td><code>Enumerator.produce</code></td>
    </tr>
    <tr>
      <td><code>lazy</code></td>
      <td>Deferred evaluation</td>
      <td><code>enum.lazy</code></td>
    </tr>
    <tr>
      <td><code>ArithmeticSequence</code></td>
      <td>Range stepping</td>
      <td><code>(1..).step(2)</code></td>
    </tr>
    <tr>
      <td><code>chain</code></td>
      <td>Concatenate enumerators</td>
      <td><code>enum1 + enum2</code></td>
    </tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Insider Notes

- External iteration uses Fibers; adds overhead but allows fine control.
- `peek` and `next` raise StopIteration at end; catch for termination.
- `feed` sets return value for next yield; cleared after use.
- Lazy enumerators avoid creating intermediate arrays for large data.
- `produce` creates infinite enumerators; use `take` to limit.

### Examples — External Iteration

```ruby
# Basic external iteration
enum = [1,2,3].each
puts enum.next  # 1
puts enum.next  # 2
puts enum.peek  # 3 (doesn't advance)
puts enum.next  # 3

# Using feed
enum = [1,2,3].map  # map uses yield return
enum.next  # 1
enum.feed("a")  # sets return for next yield
enum.next  # 2
enum.feed("b")
enum.next  # 3
enum.feed("c")
# enum.next raises StopIteration with result ["a","b","c"]

# Rewind
enum = (1..3).each
enum.next  # 1
enum.next  # 2
enum.rewind
enum.next  # 1 again

# with_index
[10,20].each.with_index { |x,i| puts "#{i}: #{x}" }
# 0: 10
# 1: 20

# with_object
[1,2,3].each.with_object([]) { |x,arr| arr << x*2 }
# [2,4,6]

# produce for infinite
fib = Enumerator.produce([0,1]) { |a,b| [b, a+b] }
fib.take(5).map(&:first)  # [0,1,1,2,3]

# lazy evaluation
(1..Float::INFINITY).lazy.select(&:odd?).take(3).to_a  # [1,3,5]

# ArithmeticSequence
seq = (1..).step(2)  # infinite odd numbers
seq.take(3)  # [1,3,5]

# Chain enumerators
enum1 = [1,2].each
enum2 = [3,4].each
(enum1 + enum2).to_a  # [1,2,3,4]
```

### Exercises — Advanced Enumerators

1. Implement a custom iterator using external iteration with `next` and `peek`.
2. Use `Enumerator.produce` to create a sequence of random numbers until a
   condition.
3. Write a lazy filter that processes large files line-by-line without loading
   all.
4. Chain multiple enumerators and use `with_object` to collect results.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
