# Ranges

Ranges model intervals—continuous sequences with a beginning and an optional end. Ruby uses them for iteration, slicing, validation, pattern matching, and even domain-specific features like endless pagination. Understanding ranges unlocks expressive, performant code for everything from numeric loops to date filters.

## Learning goals

- Distinguish inclusive (`..`) and exclusive (`...`) ranges, plus endless/beginless variants.
- Iterate, slice, and transform ranges without unnecessary array allocations.
- Choose between `include?` and `cover?` based on precision and performance.
- Combine ranges with dates, strings, and `case` expressions for clear intent.
- Apply advanced methods (`step`, `clamp`, pattern matching) to real-world problems.

## Creating ranges

```ruby
(1..5)        # inclusive: 1, 2, 3, 4, 5
(1...5)       # exclusive end: 1, 2, 3, 4
("a".."f")   # lexical range: "a" through "f"
Range.new(1, 10, true) # same as (1...10)
```

Ruby 2.6+ adds endless ranges; Ruby 2.7+ adds beginless ranges:

```ruby
(5..)   # 5, 6, 7, ... infinity
(..5)   # -infinity up to 5 inclusive
(...5)  # -infinity up to 4
```

Endless/beginless ranges shine in slicing (`array[3..]`) and guards (`if amount.in?(..0)` to detect non-positive values).

## Inclusive vs. exclusive

```ruby
(1..4).to_a   # => [1, 2, 3, 4]
(1...4).to_a  # => [1, 2, 3]
```

Use exclusive ranges when your upper bound should not be included—e.g., zero-indexed slicing or time windows that end right before a cutoff.

## Inspecting endpoints

```ruby
range = 10...20
range.begin        # => 10
range.end          # => 20
range.exclude_end? # => true
```

`range.first`/`range.last` read more naturally when communicating intent.

## Iterating without arrays

Ranges are enumerable; iterate directly instead of materializing arrays.

```ruby
(1..5).each { |n| puts n }

squares = (1..5).map { |n| n * n } # returns a new array

(0..10).step(2) { |n| puts n }     # => 0, 2, 4, 6, 8, 10
```

`Range#step` handles integers and floats (Ruby 2.4+). When stepping floats, guard for rounding differences: `((0.0..1.0).step(0.1).map { |x| x.round(1) })`.

## Membership: `include?` vs. `cover?`

```ruby
range = 1..10

range.include?(5)    # => true (checks enumeration)
range.cover?(5.5)    # => true (bounds only)
range.include?(5.5)  # => false (5.5 isn’t enumerated)
```

- Use `include?` when you need exact membership in discrete ranges.
- Use `cover?` for numeric checks where partial values between integers count, or to avoid expensive enumeration over large ranges.

## Slicing arrays and strings

```ruby
array = %w[zero one two three four]
array[1..3]    # => ["one", "two", "three"]
array[1...3]   # => ["one", "two"]
array[2..]     # => ["two", "three", "four"]

text = "developer"
text[0..2]     # => "dev"
text[-3..]     # => "per"
```

Ranges make slices self-documenting: `array[FEATURE_START..FEATURE_END]` reads as a discrete interval.

## Case statements and pattern matching

`case` expressions use `===`, so ranges slot in naturally.

```ruby
def grade(score)
  case score
  when 90..100 then "A"
  when 80...90 then "B"
  when 70...80 then "C"
  when 60...70 then "D"
  else "F"
  end
end
```

Pattern matching (Ruby 2.7+) supports ranges, too:

```ruby
case event
in { duration: 0...60 } then :fast
in { duration: 60..120 } then :standard
else :long
end
```

## Dates and times

```ruby
require "date"

period = Date.new(2025, 1, 1)..Date.new(2025, 12, 31)
period.cover?(Date.today)      # fast bounds check
period.include?(Date.today)    # enumerates every date (slow)
```

Use `cover?` for date comparisons to avoid generating every day in the range. For ranges of `Time`, remember they’re continuous; using `step` with seconds helps iterate discrete intervals.

## Clamping values

`Range#clamp` constrains a value within bounds.

```ruby
(0..100).clamp(120)  # => 100
(0..100).clamp(-5)   # => 0
(0..100).clamp(42)   # => 42
```

Great for sanitizing user input or normalizing metrics.

## Converting ranges

```ruby
(1..3).to_a           # => [1, 2, 3]
("a".."c").to_a       # => ["a", "b", "c"]
(1..).to_a            # => raises (endless ranges can’t become arrays)
```

Only convert when you truly need a materialized sequence; otherwise iterate lazily to save memory.

## Performance considerations

- Ranges store just two endpoints. `(1..1_000_000)` is cheap; `(1..1_000_000).to_a` allocates a huge array.
- `cover?` avoids enumeration, making it ideal for big intervals or floats.
- Avoid `include?` on large non-integer ranges (e.g., dates) unless you really need discrete membership.

## Common patterns

- **Pagination**: `(page_start..page_end)` to slice records.
- **Validation**: `raise unless (1..5).cover?(input)`.
- **Chunking**: `range.each_slice(size)` after converting (or use numeric stepping).
- **Guards**: `case value when ..0 then :negative when 1...10 then :small else :large end`.

## Guided practice

1. **Score buckets**
   - Given a list of exam scores, build a hash grouping them into ranges (`0..59`, `60..69`, etc.).
   - Use `Range#cover?` inside `group_by` for clarity.

2. **Sliding windows**
   - Generate overlapping slices of an array using ranges (`array[i..i+4]`).
   - Compare performance against `each_cons`.

3. **Date filter**
   - Create a `events_in_period(events, range)` helper that accepts a `Date` range.
   - Use `cover?` to include events on the boundaries and test beginless/endless inputs.

4. **Normalization**
   - Write `normalize(value, source_range, target_range)` that maps numbers from one range into another (e.g., scale 0..1023 sensor readings into 0.0..1.0 floats).

5. **Pattern matching audit**
   - Implement a log parser that uses case/`in` with ranges to classify response times (fast/acceptable/slow).
   - Ensure a fallback branch captures unexpected values.

## Self-check questions

1. When would you choose `include?` instead of `cover?`, and why?
2. How do endless and beginless ranges simplify slicing or guard clauses?
3. What trade-offs exist when converting a range to an array?
4. How does `step` behave differently for integers and floats, and how can you mitigate floating-point drift?
5. Why might `Range#clamp` be preferable to manual `min`/`max` comparisons?

Ranges make interval logic expressive and efficient. Whether you’re validating input, iterating over discrete steps, or matching structured data, lean on ranges to communicate intent clearly with minimal code.
