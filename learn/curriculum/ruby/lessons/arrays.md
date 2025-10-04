# Arrays

Arrays are Ruby’s workhorse collection. They preserve order, accept any object type, and power everything from command-line argument parsing to JSON payload handling. Mastering arrays means knowing how to build them, reshape them, combine them, and apply enumerable transformations efficiently.

## Learning goals

- Create, transform, and access arrays using idiomatic Ruby.
- Understand how array indexing, slicing, and destructuring work.
- Apply enumerable methods for searching, mapping, grouping, and reducing data.
- Manage performance trade-offs with array mutation, freezing, and lazy enumeration.
- Combine arrays with other core classes (hashes, ranges, strings) for richer data pipelines.

## Building arrays

```ruby
empty = []
numbers = [1, 2, 3, 4]
mixed = [1, "hello", :symbol, { currency: "USD" }]

Array.new(3)          # => [nil, nil, nil]
Array.new(4, "hi")   # => ["hi", "hi", "hi", "hi"]

# Dynamic initialization
Array.new(5) { |i| i * i } # => [0, 1, 4, 9, 16]

%w[ruby rails sorbet]  # shorthand for an array of strings
```

Use splats to convert arguments to arrays: `def log(*events) events end`.

## Access patterns

```ruby
letters = %w[a b c d e]

letters[0]     # => "a"
letters[-1]    # => "e"
letters.fetch(10, "missing") # safe access with default

letters[1, 2]  # => ["b", "c"] slice by starting index and length
letters[1..3]  # => ["b", "c", "d"] inclusive range
letters[1...3] # => ["b", "c"] exclusive end

# Destructuring
first, *rest = letters
# first => "a", rest => ["b", "c", "d", "e"]

head, mid, tail = [1, 2, 3]
# head => 1, mid => 2, tail => 3
```

`Array#values_at(*indexes)` pulls discontinuous items: `letters.values_at(0, -1)`.

## Mutation and sharing

Arrays are mutable. Mutating a shared array changes all references.

```ruby
config = %w[staging api]
alias_config = config
config << "v2"

alias_config # => ["staging", "api", "v2"]

config_dup = config.dup
config_dup << "v3"
config     # => ["staging", "api", "v2"]
config_dup # => ["staging", "api", "v2", "v3"]
```

Freeze arrays when you want an immutable collection: `config.freeze`.

## Adding and removing

```ruby
queue = []
queue.push("first")
queue << "second"

queue.unshift("urgent") # add to front
queue.insert(1, "middle")

queue.pop   # remove last
queue.shift # remove first
queue.delete("middle")

queue.delete_if { |item| item.start_with?("s") }
```

Use `Array#compact` to drop `nil`s and `Array#flatten` to collapse nested arrays.

## Enumerating and transforming

```ruby
sales = [120, 95, 130, 160]

sales.map { |amount| amount * 1.1 }
sales.select { |amount| amount >= 100 }
sales.reject { |amount| amount < 100 }
sales.partition { |amount| amount >= 120 }

sales.reduce(0, :+) # => sum
sales.any?(&:zero?)
sales.all? { |amount| amount > 50 }
sales.none? { |amount| amount < 0 }

# Enum helpers via symbols
sales.map(&:to_s)
```

For chained transformations, use `Enumerator::Lazy`:

```ruby
File.foreach("events.log")
    .lazy
    .map(&:strip)
    .reject(&:empty?)
    .take(10)
    .force
```

`force` realizes the lazy enumerator.

## Searching and grouping

```ruby
orders = [
  { id: 1, status: :open },
  { id: 2, status: :shipped },
  { id: 3, status: :open }
]

orders.find { |o| o[:status] == :open }
orders.filter_map { |o| o[:id] if o[:status] == :open }

orders.group_by { |o| o[:status] }
orders.each_with_object(Hash.new(0)) { |o, tally| tally[o[:status]] += 1 }
```

## Set-like operations and combination

```ruby
a = [1, 2, 3]
b = [3, 4, 5]

a + b   # concatenation
a - b   # => [1, 2]
a & b   # => [3]
a | b   # => [1, 2, 3, 4, 5]
a.zip(%w[one two three])
```

`Array#product` builds Cartesian products; `Array#permutation` and `#combination` explore ordering.

## Converting between arrays and other types

- Strings: `"2025-10-03".split("-")` and `["api", "v2"].join('/')`.
- Ranges: `(1..5).to_a` and `%w[a b c].to_enum(:cycle)` to repeat.
- Hash pairs: `{ currency: "USD" }.to_a # => [[:currency, "USD"]]`.

## Performance notes

- `Array#push`, `#pop` are O(1); `#shift` and `#unshift` are O(n) because items reindex.
- `Array#map`, `#select`, and friends copy the entire array; prefer lazy enumerators for large or infinite streams.
- `freeze` reduces accidental mutation and enables sharing between threads without locks.
- When order doesn’t matter, consider `Set` for faster membership checks.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Arrays & Enumerable (Appendix — arrays-ruby-appendix)

Short notes about array manipulation, Enumerable helpers, and testing patterns for array-heavy examples.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Method</th><th>Use</th><th>Reference</th></tr>
  </thead>
  <tbody>
    <tr><td>map/select/reject</td><td>Transform & filter</td><td>Enumerable docs</td></tr>
    <tr><td>each_with_index</td><td>Index-aware iteration</td><td>Use for position-aware transforms</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (arrays-ruby-appendix)

1. Implement a small data transformation using `map` and `select` and add tests verifying correctness.
2. Demonstrate `each_slice` and `each_cons` for grouping data and document use-cases.

<!-- markdownlint-enable MD010 -->

## Guided practice

1. **CSV rows to objects**
   - Read lines from a sample CSV string.
   - Split each line into fields and map to hashes with symbol keys.
   - Use `group_by` to cluster rows by a status column.

2. **Rolling averages**
   - Accept an array of numbers and a window size.
   - Use `each_cons(window)` to compute rolling average values.
   - Return a new array of averages up to two decimal places.

3. **Tag normalizer**
   - Given user-provided tags with spaces, uppercase letters, and duplicates, normalize by trimming, downcasing, replacing spaces with hyphens, and deduplicating while keeping original order.

4. **Inventory reconciliation**
   - You have arrays of SKUs from warehouse scans and shipping manifests.
   - Use set operations and `difference` (`-`) to report items missing or extra.

5. **Lazy log analyzer**
   - Stream a large log file lazily.
   - Filter only lines matching a pattern, take the first 50, and write them to an array without loading the whole file.

## Self-check questions

1. How does destructuring assignment work with arrays, and how can you capture the “rest” of the elements?
2. What is the difference between `Array#map` and `Array#map!`, and when would you choose one over the other?
3. How does Ruby’s method lookup handle `Array#method(:sample)` compared to iterating with `Enumerator::Lazy`?
4. Why might you freeze an array, and how does that impact attempts to mutate it?
5. What performance implications arise from frequent use of `Array#shift`, and what alternatives could you use for queue-like workloads?

Arrays are ubiquitous in Ruby. Keep them clean, avoid unnecessary mutation, and lean on Enumerable to express data transformations succinctly.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Arrays — External Links & Notes (Appendix — External Links)

Recommended references and quick recipes for common array operations.

- Ruby Array docs: [Ruby Array docs](https://ruby-doc.org/core/Array.html)
- Enumerable patterns: [Ruby Enumerable docs](https://ruby-doc.org/core/Enumerable.html)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Operation</th><th>Doc</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Mapping</td><td><a href="https://ruby-doc.org/core/Array.html#method-i-map">Array#map</a></td><td>Prefer map over manual loops for transformations</td></tr>
    <tr><td>Filtering</td><td><a href="https://ruby-doc.org/core/Array.html#method-i-select">Array#select</a></td><td>Use select/reject for clarity</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Replace manual index-based array transformations with `map` and add unit tests.
2. Benchmark `Array#map` vs manual loops for a large dataset and report results.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Arrays & Enumerable — Recipes (Appendix — arrays-ruby-appendix2)

Quick recipes for lazy enumeration, testing large datasets, and small benchmarks comparing builders.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>Why</th><th>Examples</th></tr>
  </thead>
  <tbody>
    <tr><td>Lazy enumerators</td><td>Avoid full materialization</td><td>File.foreach(...).lazy</td></tr>
    <tr><td>StringIO builder</td><td>Efficient string assembly</td><td>Use for large reports</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Lazy example

```ruby
lines = File.foreach('large.log').lazy.map(&:strip).reject(&:empty?).take(100).force
```

### Benchmark builder

```ruby
require 'benchmark'
require 'stringio'

n = 10_000
Benchmark.realtime do
  s = ''
  n.times { |i| s << "line #{i}\n" }
end

Benchmark.realtime do
  io = StringIO.new
  n.times { |i| io << "line #{i}\n" }
  io.string
end
```

### Exercises (Appendix — arrays-ruby-appendix2)

1. Implement a lazy pipeline that reads a large CSV and returns the first 50 distinct users.
2. Compare `+=` vs `StringIO` for building a large string and report timings.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
