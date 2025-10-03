# Numbers and Math in Ruby

Ruby treats numbers as full-fledged objects with powerful behavior built in. Whether you’re summing invoices, running physics simulations, or manipulating bit fields, Ruby’s numeric toolkit balances expressiveness with precision. This lesson dives into the major number classes, arithmetic operations, conversion APIs, and performance tips.

## Learning goals

- Distinguish between integers, floats, rationals, complex numbers, and BigDecimal.
- Perform arithmetic safely, accounting for integer division, operator precedence, and precision pitfalls.
- Format, parse, and convert numeric values in idiomatic Ruby.
- Harness the `Math` and `BigDecimal` libraries for advanced calculations.
- Generate random numbers, work with ranges, and leverage enumerable helpers.

## Integer fundamentals

Ruby unifies small and large integers under the `Integer` class—no more `Fixnum` vs. `Bignum`. Integers expand to arbitrary precision automatically.

```ruby
age = 25
population = 1_000_000_000_000   # underscores improve readability
factorial = (1..50).inject(:*)   # enormous but accurate
```

Helper methods:

- `even?`, `odd?` to test parity.
- `digits` returns an array of base-10 digits (or digits in another base if you pass `base`).
- `bit_length` reveals how many bits are required to represent the integer.

```ruby
42.even?        # => true
123.digits      # => [3, 2, 1]
1024.bit_length # => 11
```

## Float arithmetic and caveats

`Float` uses double-precision IEEE 754. Expect rounding errors when representing fractions like `0.1`.

```ruby
total = 0.1 + 0.2
puts total           # => 0.30000000000000004
puts total == 0.3    # => false
```

Mitigate by using `BigDecimal` or rounding with `round(precision)` when comparing results:

```ruby
total.round(2) == 0.3 # => true
```

## Beyond integers and floats

### Rational numbers

Use `Rational(numerator, denominator)` or the literal `3r` to store exact fractions.

```ruby
half = Rational(1, 2)
quarter = 1r / 4
puts half + quarter   # => (3/4)
```

### Complex numbers

`Complex` handles real + imaginary parts. Create them with `Complex(real, imag)` or the literal `Complex(0, 1)` for `i`.

```ruby
require "complex"
z = Complex(2, 3)
puts z * z.conjugate  # => (13+0i)
```

### BigDecimal for financial accuracy

Floats can introduce rounding errors in financial calculations. Use `BigDecimal` from the standard library.

```ruby
require "bigdecimal"
require "bigdecimal/util"

price = "19.95".to_d
quantity = 3
total = price * quantity
puts total.to_s("F")  # => "59.85"
```

## Arithmetic operations refresher

```ruby
10 + 5    # addition
10 - 5    # subtraction
10 * 5    # multiplication
10 / 5    # integer division when both operands are integers
10 / 3    # => 3
10.fdiv(3) # => 3.3333333333333335 (always returns float)
10 % 3    # modulo (remainder)
10.remainder(3) # remainder with sign of dividend
2 ** 3    # exponentiation
```

`divmod` returns quotient and remainder simultaneously:

```ruby
quotient, remainder = 10.divmod(3)
# quotient => 3, remainder => 1
```

## Operator precedence and parentheses

Ruby follows standard precedence: exponentiation > unary +/- > multiplication/division > addition/subtraction. Use parentheses to make intentions obvious.

```ruby
result = 10 + 5 * 2       # => 20
result = (10 + 5) * 2     # => 30
```

## Numeric conversions and parsing

```ruby
42.to_f          # => 42.0
3.99.to_i        # => 3 (truncates toward zero)
"42".to_i        # => 42
Integer("42")    # => 42, raises if invalid
Integer("abc", exception: false) # => nil
"3.14".to_f      # => 3.14
"0b1010".to_i(2) # => 10 (binary parsing)
```

`Integer`, `Float`, and `Rational` constructors accept `exception: false` to avoid raising; they return `nil` instead.

## Formatting numbers

Use `format` or `Kernel#sprintf` for human-friendly output:

```ruby
format("%.2f", 3.456)     # => "3.46"
format("%08d", 42)        # => "00000042"
format("%#x", 255)        # => "0xff"
format("%-10s %5.2f", "Total", 19.9)
```

`Numeric#to_s(base)` converts integers to other bases; `Integer(str, base)` parses from that base.

## Math module highlights

```ruby
Math::PI        # => 3.141592653589793
Math::E         # Euler's number
Math.sqrt(16)   # => 4.0
Math.exp(1)     # => Math::E
Math.log(100, 10) # => 2.0
Math.sin(Math::PI / 2) # => 1.0
```

Trigonometric methods expect radians. Use `Math::PI / 180` to convert degrees to radians.

## Bitwise operations (integers only)

```ruby
flags = 0b1010
flags & 0b1100 # bitwise AND
flags | 0b0101 # bitwise OR
flags ^ 0b1111 # bitwise XOR
flags << 1     # left shift
flags >> 2     # right shift
```

Handy for working with permissions, feature flags, or low-level protocols.

## Random numbers

Ruby’s `Random` class generates pseudo-random numbers. Seeding allows reproducible sequences (excellent for tests).

```ruby
rng = Random.new(1234)
rng.rand              # float between 0.0 and 1.0
rng.rand(100)         # integer between 0 and 99
rng.rand(1.0..2.0)    # float within range
```

Use the global `rand` helper for quick tasks; pass a range to specify bounds.

## Numeric ranges and iteration

Numeric ranges (`start..finish` or `start...finish`) integrate with loops and enumerators.

```ruby
(1..5).each { |n| puts n }
(0...10).step(2) { |n| puts n }
```

`Range#step` accepts floats in Ruby 2.4+; it handles rounding carefully.

## Enumerating numeric sequences

`Integer#times`, `#upto`, `#downto` provide expressive loops:

```ruby
5.times { |i| puts "Iteration #{i}" }
1.upto(3) { |n| puts n }
3.downto(1) { |n| puts n }
```

## Performance and memory notes

- Integers are immediate values; Ruby stores them inside the variable reference.
- Floats allocate objects; reuse them when possible in tight loops.
- `BigDecimal` trades speed for precision—cache results if used frequently.
- Profile long-running math heavy tasks; consider using gems like `Numo::NArray` or bindings to C libraries when necessary.

## Error handling and overflow

Ruby raises `ZeroDivisionError` when dividing by zero (except float division, which returns `Infinity` or `NaN`). Guard your denominators or rescue the error.

```ruby
begin
  10 / 0
rescue ZeroDivisionError
  puts "Cannot divide by zero!"
end
```

`Float::INFINITY`, `-Float::INFINITY`, and `Float::NAN` help detect special float values.

```ruby
value = 1.0 / 0
puts value.infinite?   # => 1
```

## Guided practice

1. **Mortgage calculator**
   - Ask for principal, annual interest rate, and years.
   - Convert the interest to a monthly rate (`rate / 12.0 / 100`).
   - Compute the monthly payment using the amortization formula with `BigDecimal` to avoid rounding issues.

2. **Prime inspector**
   - Write a method `prime?(n)` using trial division up to `Math.sqrt(n)`.
   - Use it to list all primes between 2 and 100.

3. **Precise currency totals**
   - Parse a CSV line like `"coffee,3,4.75"`.
   - Multiply quantity and price using `BigDecimal`, then accumulate totals for multiple rows.

4. **Bitmask flags**
   - Model permissions (e.g., read = 0b001, write = 0b010, execute = 0b100).
   - Write helpers `grant(mask, flag)` and `granted?(mask, flag)` using bitwise operators.

5. **Random sampler**
   - Seed `Random.new(123)` and generate 5 random floats between -1.0 and 1.0.
   - Map them to angles, compute their sine using `Math.sin`, and print the results formatted to three decimals.

## Self-check questions

1. Why does Ruby no longer distinguish between `Fixnum` and `Bignum`, and how does that help your programs?
2. When comparing floats, why might you use `Float#round` or `BigDecimal` instead of direct equality?
3. How do `divmod`, `remainder`, and `%` differ when working with negative numbers?
4. What are practical scenarios for using `Rational` or `Complex` classes in day-to-day development?
5. How can you generate reproducible random sequences, and why is that valuable in testing?

Math is more than memorizing operators—it’s about choosing precise types, writing clear formulas, and guarding against edge cases. Keep experimenting with Ruby’s numeric classes to make your calculations both elegant and reliable.
