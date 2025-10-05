# Advanced Enumerables

The `Enumerable` mixin is Ruby’s Swiss Army knife for working with collections. Once you understand the basics (`each`, `map`, `select`), the advanced methods unlock expressive data pipelines, streaming transformations, and concise algorithms. This lesson pushes beyond fundamentals and explores powerful patterns, performance considerations, and design techniques.

## Learning goals

- Combine advanced `Enumerable` helpers to perform complex transformations in a single readable chain.
- Use grouping, chunking, windowing, and statistical helpers (`group_by`, `tally`, `slice_when`, `filter_map`, `minmax`) to analyze datasets.
- Apply lazy enumerables to process large or infinite sequences without materializing everything in memory.
- Customize your own enumerables that cooperate with the full suite of methods.
- Recognize opportunities to replace imperative loops with higher-level operations that better match the problem domain.

## The power core: `Enumerable`

`Enumerable` assumes the including class defines an `each` method. From there, methods like `map`, `select`, `reduce`, `grep`, `sum`, and more become available. Keep in mind:

- Most methods return an `Enumerator` when called without a block—handy for deferring execution or reusing logic later.
- The methods are optimized in C, so let them do the heavy lifting instead of rewriting loops yourself.

## Transformation and mapping patterns

### `map`, `map!`, `flat_map`, and `filter_map`

- `map` transforms each element (non-destructive).
- `map!` mutates the receiver—use sparingly.
- `flat_map` maps and flattens one level, eliminating nested arrays.
- `filter_map` (Ruby 2.7+) combines filtering and mapping in one pass, returning non-`nil` results.

```ruby
products = [
  { sku: "A-101", tags: %w[featured sale] },
  { sku: "B-202", tags: %w[new] }
]

tag_index = products.flat_map { |p| p[:tags].map { |tag| [tag, p[:sku]] } }
# => [["featured", "A-101"], ["sale", "A-101"], ["new", "B-202"]]

# Extract non-empty promo codes
promos = products.filter_map do |product|
  code = product[:promo]
  code&.upcase
end
```

### `each_with_object`

Ideal when you need to build hashes or arrays without remembering to return the accumulator.

```ruby
roles = %w[admin editor viewer]

permissions = roles.each_with_object({}) do |role, acc|
  acc[role] = PermissionLoader.for(role)
end
```

## Filtering, slicing, and segmenting

### `select`, `reject`, `filter`, `filter_map`

`filter` is an alias for `select`. Prefer the name that reads best in context.

### `drop_while`, `take_while`, `drop`, `take`

Craft pipelines that skip or capture leading segments.

```ruby
events = %w[init init ready ready error error recover]

errors = events.drop_while { |event| event != "error" }
leading_ok = events.take_while { |event| event != "error" }
```

### `slice_when`, `chunk_while`, `chunk`

Segment streams based on adjacency rules.

```ruby
temperatures = [20, 21, 40, 41, 42, 19]

heatwaves = temperatures.slice_when { |prev, curr| (curr - prev).abs > 5 }.to_a
# => [[20, 21], [40, 41, 42], [19]]

log_levels = %i[info info warn warn warn error info]

runs = log_levels.chunk_while { |prev, curr| prev == curr }.to_a
# => [[:info, :info], [:warn, :warn, :warn], [:error], [:info]]
```

`chunk` groups by the result of the block but yields pairs of `[key, group]`. Use it when you want to keep group metadata.

### `partition`

Split a collection into two arrays: matches and non-matches.

```ruby
successes, failures = results.partition(&:success?)
```

## Grouping, counting, and summarizing

### `group_by` and `transform_values`

```ruby
sessions = [
  { user: "alice", duration: 35 },
  { user: "bob", duration: 12 },
  { user: "alice", duration: 18 }
]

totals = sessions
  .group_by { |session| session[:user] }
  .transform_values { |sessions| sessions.sum { |s| s[:duration] } }
# => {"alice"=>53, "bob"=>12}
```

### `tally`

Count occurrences without writing a loop.

```ruby
words = %w[ruby rails ruby bundler ruby]
words.tally
# => {"ruby"=>3, "rails"=>1, "bundler"=>1}
```

`tally` accepts a block in Ruby 3.1+, enabling `words.tally(&:size)`.

### `count`, `sum`, `average`

`count` can take an argument or block. Ruby lacks a built-in `average`, but you can compose `sum.to_f / count` safely.

```ruby
temperatures.count { |t| t > 30 }
```

## Searching and ordering

### `min`, `max`, `minmax`, `min_by`, `max_by`, `minmax_by`

Emerging Ruby versions add `minmax` to reduce passes over the array.

```ruby
min_temp, max_temp = temperatures.minmax
longest, shortest = words.minmax_by(&:length)
```

### `sort_by` vs `sort`

`sort` expects a block returning -1, 0, or 1, while `sort_by` computes a key once per element—ideal when key computation is expensive.

```ruby
records.sort_by { |record| [record.status, -record.updated_at.to_i] }
```

### `grep` and `grep_v`

Pattern-based filtering using the `===` operator.

```ruby
mixed = [1, "two", :three, /four/]

numbers = mixed.grep(Integer)     # => [1]
non_numbers = mixed.grep_v(Integer)  # everything else

log_lines = File.readlines("logs/app.log")
errors = log_lines.grep(/ERROR/)
```

`grep` can take a block to transform matches in place.

## Enumerating combinations and permutations

Enumerables have combinatoric helpers on Array:

- `combination(n)` yields combinations of size `n`.
- `permutation(n)` yields permutations.
- `repeated_permutation`, `repeated_combination` for variations with repetition.

```ruby
teams = %w[A B C D]

teams.combination(2).each { |pair| p pair }
teams.permutation(3).take(2) # first two permutations of 3-team orders
```

Use these for scheduling, pairing bots, or exploring search spaces.

## Lazy enumerables and streaming data

`Enumerator::Lazy` defers computation until elements are requested. This is essential when dealing with large datasets, file streams, or infinite series.

```ruby
require "csv"

large_csv = CSV.foreach("metrics.csv", headers: true)

high_cpu = large_csv
  .lazy
  .select { |row| row["cpu"].to_f > 80 }
  .map { |row| row["service"] }
  .uniq

puts high_cpu.take(5).force
```

Combine `lazy` with `chunk_while` to process log windows without slurping entire files.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Lazy Enumerables & Streaming (Appendix — enumerables_advanced-ruby2)

Using `Enumerator::Lazy` to process large sequences without allocating intermediate arrays.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Technique</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Enumerator::Lazy</td><td>Large pipelines</td><td>Chain maps/filters without intermediates</td></tr>
    <tr><td>Streams</td><td>I/O</td><td>Yield lines to avoid full reads</td></tr>
    <tr><td>Chunking</td><td>Batch work</td><td>Process in fixed-size blocks</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: lazy chain

```ruby
(1..Float::INFINITY).lazy.select { |n| prime?(n) }.first(10)
```

### Exercises (Appendix — enumerables_advanced-ruby2)

1. Process a very large file line-by-line using `File.foreach` and a lazy enumerator to transform and count matching lines.
2. Implement a memory-safe pipeline that computes running aggregates using `each_slice`.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

## Designing your own enumerable objects

Custom collections should:

1. Implement `each` and yield logical elements.
2. Return an enumerator when no block is provided.
3. Include `Enumerable` to gain advanced methods.

```ruby
class SensorStream
  include Enumerable

  def initialize(client)
    @client = client
  end

  def each
    return enum_for(:each) unless block_given?

    @client.readings do |reading|
      yield JSON.parse(reading, symbolize_names: true)
    end
  end
end

stream = SensorStream.new(RemoteClient.new)
alerts = stream.select { |r| r[:temperature] > 40 }
```

By exposing `each`, the entire enumerable toolbox becomes available to consumers.

## Putting it together: complex pipelines

### Example: revenue analytics

```ruby
orders = DataWarehouse.fetch_recent_orders

top_customers = orders
  .reject { |order| order.refunded? }
  .group_by(&:customer_id)
  .transform_values { |orders| orders.sum(&:total_cents) }
  .sort_by { |_customer_id, total| -total }
  .take(10)
```

### Example: detecting anomalies in telemetry

```ruby
anomalies = telemetry
  .lazy
  .chunk_while { |prev, curr| (curr[:timestamp] - prev[:timestamp]) <= 5 }
  .map do |chunk|
    avg = chunk.sum { |sample| sample[:value] } / chunk.size.to_f
    { range: chunk.first[:timestamp]..chunk.last[:timestamp], average: avg }
  end
  .select { |window| window[:average] > threshold }
  .force
```

## Performance considerations

- Prefer `lazy` when working with large or infinite sequences to avoid intermediate arrays.
- Avoid repeated sorting within loops; compute once and reuse.
- Benchmark critical pipelines using `Benchmark.bm` or `benchmark-ips`.
- `sum`, `tally`, and `count` perform their work in C and are typically faster than hand-rolled `reduce` equivalents.
- When chaining many transformations, consider readability: break pipelines into named scopes or helper methods if they become unwieldy.

## Guided practice

1. **Subscription retention**
   - Start with an array of subscription hashes containing `:user_id`, `:status`, `:plan`, and `:monthly_revenue`.
   - Build a report showing revenue lost to cancellations grouped by plan, sorted descending.
   - Use `group_by`, `transform_values`, `sum`, and `sort_by`.

2. **Error burst detection**
   - Process log entries ordered by timestamp.
   - Use `slice_when` to break the stream whenever the gap between entries exceeds 60 seconds.
   - Within each slice, compute the ratio of error lines; flag slices where the error rate is above 40%.

3. **Inventory reconciliation**
   - Given an array of products with `:sku`, `:warehouse_counts` (array of integers), and `:expected_total`.
   - Use `sum`, `partition`, and `each_with_object` to return a hash of discrepancies keyed by SKU.

4. **Lazy RSS feed reader**
   - Simulate an infinite enumerator of RSS entries.
   - Use `lazy` to filter by category, `grep` for keyword matches, and `take(15)` to display the first 15 matches without loading the entire feed.

5. **Permutation-based scheduling**
   - Generate all pairings of three engineers for on-call rotations using `combination` and `cycle`.
   - Ensure the schedule avoids repeating the same pair in consecutive weeks using `each_cons` to inspect windows.

## Self-check questions

1. What benefits does `filter_map` provide compared to chaining `map` and `compact`?
2. When should you reach for `slice_when` instead of `group_by`?
3. How does returning an enumerator when no block is given improve API design for custom collections?
4. Why can `minmax_by` be more efficient than calling `min_by` and `max_by` separately?
5. How does lazy evaluation help when chaining `select`, `map`, and `take` on a large file stream?

Mastery of advanced enumerables is less about memorizing every method and more about recognizing the pattern: describe the transformation, let `Enumerable` orchestrate it, and keep your code declarative. As you practice, challenge yourself to simplify imperative loops into fluent method chains that clearly express intent.
