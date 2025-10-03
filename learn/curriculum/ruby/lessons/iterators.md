# Iterators in Ruby

Iterators power Ruby’s expressive style. Instead of manual index juggling, you let objects yield elements while you focus on intent: transform, filter, group, or short-circuit. This lesson dives deep into iterator patterns, advanced `Enumerable` helpers, enumerators, and practical workflows that keep data pipelines elegant and efficient.

## Learning goals

- Understand how `Enumerable` underpins most iteration patterns.
- Choose the right iterator for traversal, transformation, filtering, aggregation, and grouping.
- Chain iterators without sacrificing readability by leveraging blocks, procs, and external enumerators.
- Use advanced helpers (`partition`, `group_by`, `tally`, `each_with_object`, `chunk_while`, lazy enumerators) to solve real problems.
- Guard against performance pitfalls and write iterator-friendly APIs for your own classes.

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

- `each`: baseline traversal for arrays, hashes, ranges, sets, and custom collections.
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
- `flat_map` (a.k.a. `collect_concat`) maps and flattens one level, perfect for one-to-many transformations.
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
- `each_with_object(initial)` is perfect for building hashes or arrays without returning the visited collection.

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

External enumerators let you pause/resume iteration, merge multiple sequences, or coordinate asynchronous workflows.

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

Laziness prevents precomputing the entire sequence, perfect for infinite streams or large file processing.

## Writing iterator-friendly APIs

When designing methods, accept blocks and yield results rather than returning precomputed arrays. Provide fallback enumerators so callers can choose their style.

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

- Avoid chaining that materializes large intermediate arrays; use `lazy` or `each` with immediate side effects.
- Prefer built-in iterator methods over manual loops—they’re optimized in C.
- For numeric summations, use `sum` instead of `reduce(:+)` for clarity and speed.
- Memoize expensive computations outside of loops to avoid redundant work inside blocks.
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
   - Given an array of audit entries, group them by `:user_id`, count actions per user, and produce a report sorted by action count.
   - Use `group_by`, `transform_values`, and `sort_by`.

2. **SKU expansion**
   - Start with a list of base SKUs and a mapping of suffixes. Use `flat_map` to generate the full SKU list, then `tally` to verify duplicates.

3. **Sliding window alerts**
   - Implement `spikes(data, threshold)` that returns indices where the average of a 3-element moving window exceeds the threshold.
   - Use `each_cons` combined with `with_index`.

4. **Chunked file upload**
   - Read a binary file in 1 KB slices with `each_slice`. For each slice, simulate an upload call and stop when a simulated failure occurs, returning the number of successful chunks.

5. **Lazy prime stream**
   - Build a lazy enumerator that yields prime numbers. Use `take` to fetch the first 25 primes greater than 1,000 and sum them.

## Self-check questions

1. Why is it important to return `enum_for(:each)` when an iterator is called without a block?
2. When would `flat_map` be more appropriate than `map` followed by `flatten`?
3. How does `each_with_object` differ from `reduce` in terms of mutating accumulators?
4. What advantages do lazy enumerators provide when processing large logs or API streams?
5. How can `partition` simplify code that used to rely on two separate `select`/`reject` operations?

Iterators are the idiomatic way to express loops in Ruby. Mastering them lets you replace ten lines of index juggling with a single, intention-revealing method call. Combine traversal helpers, aggregators, and lazy streams to build data transformations that read like plain English while staying performant.
