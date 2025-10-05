# Lesson 8.4: Operator Overloading

## Overview

Operator overloading allows you to redefine how operators work with your custom classes. In Ruby, operators are actually method calls, so you can define methods like `+`, `-`, `==`, `<=>`, etc. to customize behavior.

## Basic Arithmetic Operators

```ruby
class Vector
  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  # Overload + operator
  def +(other)
    Vector.new(@x + other.x, @y + other.y)
  end

  # Overload - operator
  def -(other)
    Vector.new(@x - other.x, @y - other.y)
  end

  # Overload * operator for scalar multiplication
  def *(scalar)
    Vector.new(@x * scalar, @y * scalar)
  end

  def to_s
    "(#{@x}, #{@y})"
  end
end

v1 = Vector.new(2, 3)
v2 = Vector.new(1, 4)

puts v1 + v2  # => (3, 7)
puts v1 - v2  # => (1, -1)
puts v1 * 3   # => (6, 9)
```

## Comparison Operators

The `<=>` (spaceship) operator is fundamental for comparisons:

```ruby
class Person
  attr_reader :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end

  # Spaceship operator for comparisons
  def <=>(other)
    @age <=> other.age
  end

  # These are automatically available when <=> is defined
  # def <(other);  (@age <=> other.age) < 0;  end
  # def >(other);  (@age <=> other.age) > 0;  end
  # def ==(other); (@age <=> other.age) == 0; end
  # def <=(other); (@age <=> other.age) <= 0; end
  # def >=(other); (@age <=> other.age) >= 0; end

  def to_s
    "#{@name} (#{@age})"
  end
end

alice = Person.new("Alice", 30)
bob = Person.new("Bob", 25)
charlie = Person.new("Charlie", 35)

puts alice > bob    # => true
puts bob < charlie # => true
puts alice == alice # => true
```

## Equality Operators

Override `==` and `!=` for custom equality:

```ruby
class Point
  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  # Override == for value equality
  def ==(other)
    other.is_a?(Point) && @x == other.x && @y == other.y
  end

  # != is automatically the negation of ==
  def to_s
    "(#{@x}, #{@y})"
  end
end

p1 = Point.new(1, 2)
p2 = Point.new(1, 2)
p3 = Point.new(2, 3)

puts p1 == p2  # => true
puts p1 == p3  # => false
puts p1 != p3  # => true
```

## Unary Operators

```ruby
class ComplexNumber
  attr_reader :real, :imaginary

  def initialize(real, imaginary = 0)
    @real = real
    @imaginary = imaginary
  end

  # Unary minus
  def -@
    ComplexNumber.new(-@real, -@imaginary)
  end

  # Unary plus (usually just returns self)
  def +@
    self
  end

  def to_s
    "#{@real}#{@imaginary >= 0 ? '+' : ''}#{@imaginary}i"
  end
end

c = ComplexNumber.new(3, 4)
puts c     # => 3+4i
puts -c    # => -3-4i
puts +c    # => 3+4i
```

## Index Operators

```ruby
class Matrix
  def initialize(rows, cols)
    @rows = rows
    @cols = cols
    @data = Array.new(rows) { Array.new(cols, 0) }
  end

  # Getter with []
  def [](row, col)
    @data[row][col]
  end

  # Setter with []=
  def []=(row, col, value)
    @data[row][col] = value
  end

  def to_s
    @data.map { |row| row.join(' ') }.join("\n")
  end
end

matrix = Matrix.new(2, 3)
matrix[0, 0] = 1
matrix[0, 1] = 2
matrix[1, 2] = 3

puts matrix[0, 0]  # => 1
puts matrix[1, 2]  # => 3
puts matrix
# Output:
# 1 2 0
# 0 0 3
```

## Conversion Operators

```ruby
class Temperature
  attr_reader :celsius

  def initialize(celsius)
    @celsius = celsius
  end

  # Convert to string
  def to_s
    "#{@celsius}°C"
  end

  # Convert to integer (round to nearest degree)
  def to_i
    @celsius.round
  end

  <!-- markdownlint-disable MD033 MD034 MD040 MD010 MD022 MD032 MD024 -->

  ## Practical Appendix: Operator Overloading — Patterns & Tests (Appendix — operator-quick)

  When to overload operators, common pitfalls, and test strategies.

  <!-- markdownlint-disable MD033 MD013 -->
  <table>
    <thead>
      <tr><th>Operator</th><th>When</th><th>Test tip</th></tr>
    </thead>
    <tbody>
      <tr><td>+</td><td>Combine values (e.g., vectors)</td><td>Assert commutativity when expected</td></tr>
      <tr><td><=></td><td>Ordering</td><td>Test `a < b`, `a == b`, `a > b` for consistency</td></tr>
      <tr><td>[] / []=</td><td>Index-like access</td><td>Cover bounds and nil behavior</td></tr>
    </tbody>
  </table>
  <!-- markdownlint-enable MD033 -->

  Exercises:

  1. Implement `Vector#==` and test equality, ensuring `eql?`/`hash` align for Hash keys.
  2. Add range and boundary tests for `Matrix#[]`/`[]=` to ensure safe indexing.

  <!-- markdownlint-enable MD033 MD034 MD040 MD010 MD022 MD032 MD024 -->

  # Convert to float
  def to_f
    @celsius.to_f
  end

  # Convert to array [celsius, fahrenheit]
  def to_a
    [@celsius, @celsius * 9.0 / 5.0 + 32.0]
  end
end

temp = Temperature.new(20.5)
puts temp.to_s    # => "20.5°C"
puts temp.to_i    # => 21
puts temp.to_f    # => 20.5
p temp.to_a       # => [20.5, 68.9]
```

## Assignment Operators

Ruby automatically handles compound assignments when you define the base
operator:

```ruby
class Counter
  attr_reader :value

  def initialize(value = 0)
    @value = value
  end

  def +(other)
    Counter.new(@value + other)
  end

  # These become available automatically:
  # +=, -=, *=, /=, etc.
end

counter = Counter.new(5)
counter += 3  # Same as: counter = counter + 3
puts counter.value  # => 8
```

## Common Overloaded Operators

<!-- markdownlint-disable MD033 MD013 -->
<table>
  <thead>
    <tr><th>Operator</th><th>Method</th><th>Purpose</th></tr>
  </thead>
  <tbody>
    <tr><td><code>+</code></td><td><code>+</code></td><td>Addition</td></tr>
    <tr><td><code>-</code></td><td><code>-</code></td><td>Subtraction</td></tr>
    <tr><td><code>*</code></td><td><code>*</code></td><td>Multiplication</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD013 -->
<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Operator</th><th>Method</th><th>Purpose</th></tr>
  </thead>
  <tbody>
    <tr><td><code>+</code></td><td><code>+</code></td><td>Addition</td></tr>
    <tr><td><code>-</code></td><td><code>-</code></td><td>Subtraction</td></tr>
    <tr><td><code>*</code></td><td><code>*</code></td><td>Multiplication</td></tr>
    <tr><td><code>/</code></td><td><code>/</code></td><td>Division</td></tr>
    <tr><td><code>%</code></td><td><code>%</code></td><td>Modulo</td></tr>
    <tr><td><code>**</code></td><td><code>**</code></td><td>Exponentiation</td></tr>
    <tr><td><code>==</code></td><td><code>==</code></td><td>Equality</td></tr>
    <tr><td><code>!=</code></td><td><code>!=</code></td><td>Inequality</td></tr>
    <tr><td><code><</code></td><td><code><</code></td><td>Less than</td></tr>
    <tr><td><code><=</code></td><td><code><=</code></td><td>Less than or equal</td></tr>
    <tr><td><code>></code></td><td><code>></code></td><td>Greater than</td></tr>
    <tr><td><code>>=</code></td><td><code>>=</code></td><td>Greater than or equal</td></tr>
    <tr><td><code><=></code></td><td><code><=></code></td><td>Comparison (spaceship)</td></tr>
    <tr><td><code>[]</code></td><td><code>[]</code></td><td>Index access</td></tr>
    <tr><td><code>[]=`</code></td><td><code>[]=`</code></td><td>Index assignment</td></tr>
    <tr><td><code>-@</code></td><td><code>-@</code></td><td>Unary minus</td></tr>
    <tr><td><code>+@</code></td><td><code>+@</code></td><td>Unary plus</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

## Best Practices

1. **Keep it intuitive**: Operators should behave as users expect
2. **Return appropriate types**: Binary operators usually return new instances
3. **Handle type errors**: Check types in your operator methods
4. **Document behavior**: Make operator behavior clear
5. **Consider commutativity**: `a + b` should equal `b + a` when possible

```ruby
class Money
  attr_reader :amount, :currency

  def initialize(amount, currency = 'USD')
    @amount = amount
    @currency = currency
  end

  def +(other)
    raise ArgumentError, "Currency mismatch" unless @currency == other.currency
    Money.new(@amount + other.amount, @currency)
  end

  def ==(other)
    other.is_a?(Money) && @amount == other.amount && @currency == other.currency
  end

  def to_s
    "$#{@amount} #{@currency}"
  end
end

dollar1 = Money.new(10)
dollar2 = Money.new(5)
euro = Money.new(10, 'EUR')

puts dollar1 + dollar2  # => $15 USD
puts dollar1 == dollar2 # => false

# This would raise an error:
# puts dollar1 + euro  # => ArgumentError: Currency mismatch
```

## Advanced Example: Custom Collection

```ruby
class CustomArray
  def initialize(*elements)
    @elements = elements
  end

  def +(other)
    CustomArray.new(*(@elements + other.elements))
  end

  def -(other)
    CustomArray.new(*(@elements - other.elements))
  end

  def [](index)
    @elements[index]
  end

  def []=(index, value)
    @elements[index] = value
  end

  def <<(element)
    @elements << element
    self
  end

  def size
    @elements.size
  end

  def each(&block)
    @elements.each(&block)
  end

  protected
  attr_reader :elements
end

arr1 = CustomArray.new(1, 2, 3)
arr2 = CustomArray.new(3, 4, 5)

combined = arr1 + arr2  # CustomArray with [1, 2, 3, 3, 4, 5]
diff = arr1 - arr2      # CustomArray with [1, 2]

arr1 << 4               # Add element
puts arr1[0]           # => 1
```

Operator overloading makes your classes more intuitive and Ruby-like. Use it to
create domain-specific languages and make your APIs more expressive.

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Operator Overloading — Testing & Contracts (Appendix  operator-overloading-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Check</th><th>Tip</th></tr>
  </thead>
  <tbody>
    <tr>
      <td>`==` vs `eql?`</td>
      <td>Value equality vs hash equality</td>
      <td>Override `hash` when defining `eql?` so objects work as Hash keys</td>
    </tr>
    <tr>
      <td>Commutativity</td>
      <td>`a + b == b + a`?</td>
      <td>Document whether your operator is commutative; include tests</td>
    </tr>
    <tr>
      <td>Edge cases</td>
      <td>Bounds, nils, type mismatches</td>
      <td>Add defensive tests and explicit type checks when appropriate</td>
    </tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-disable MD013 -->
### Appendix — Examples

```ruby
def eql?(other)
  other.is_a?(Point) && x == other.x && y == other.y
end

def hash
  [x, y].hash
end
```

<!-- markdownlint-enable MD013 -->
### Appendix — Exercises

1. Add `hash` and `eql?` implementations for `Vector` and write tests using
   instances as Hash keys.
2. Test operator symmetry/commutativity where appropriate and document expected
   behavior.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->

<!-- markdownlint-disable MD013 -->
## Practical Appendix: Advanced Operator Patterns & Case Equality (Appendix — operator-overloading-advanced-20251005)

Leverage <=> for sorting, === for case statements, and method_missing for dynamic operators.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Operator</th><th>Use Case</th><th>Insider Tip</th></tr>
  </thead>
  <tbody>
    <tr>
      <td>`<=>`</td>
      <td>Sorting and comparison</td>
      <td>Include Comparable for free < > <= >= methods</td>
    </tr>
    <tr>
      <td>`===`</td>
      <td>Case equality</td>
      <td>Define for when objects match in case statements</td>
    </tr>
    <tr>
      <td>`method_missing`</td>
      <td>Dynamic operators</td>
      <td>Use for DSLs with custom operators like `obj.+(other)`</td>
    </tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Examples — Case Equality & Sorting

```ruby
class Range
  def ===(value)
    include?(value)
  end
end

case 5
when 1..10
  puts "In range"
end

class Version
  include Comparable

  def initialize(major, minor)
    @major, @minor = major, minor
  end

  def <=>(other)
    [@major, @minor] <=> [other.major, other.minor]
  end
end

versions = [Version.new(1,2), Version.new(1,1)]
versions.sort  # Uses <=>
```

### Exercises — Advanced Patterns

1. Implement `===` for a custom class and use it in a case statement.
2. Add Comparable to a class with `<=>` and test sorting behavior.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
