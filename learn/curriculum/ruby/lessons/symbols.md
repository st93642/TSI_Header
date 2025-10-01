# Symbols in Ruby

Learn about symbols - one of Ruby's most distinctive and efficient features.

## What is a Symbol?

A symbol is an immutable identifier represented by a name preceded by a colon:

```ruby
:name
:email
:status
:user_id
```

## Symbols vs Strings

Symbols look like strings but behave very differently:

```ruby
# Strings
"hello".object_id  # 70123456789000
"hello".object_id  # 70123456789020 (different!)

# Symbols
:hello.object_id   # 1234567
:hello.object_id   # 1234567 (same!)
```

**Key Difference:** Each unique symbol exists only once in memory, while strings create new objects each time.

## When to Use Symbols

### 1. Hash Keys (Most Common)

```ruby
# Good: Symbols as hash keys
user = {
  name: "Alice",
  age: 25,
  email: "alice@example.com"
}

# Accessing
user[:name]  # "Alice"
user[:age]   # 25

# Less efficient: Strings as keys
user = {
  "name" => "Alice",
  "age" => 25
}
```

### 2. Method Names and Identifiers

```ruby
# send method uses symbols
object.send(:method_name, args)

# attr_accessor uses symbols
class Person
  attr_accessor :name, :age
end

# respond_to? checks method existence
user.respond_to?(:save)  # true or false
```

### 3. Constants and Configuration

```ruby
STATUS = {
  pending: 0,
  approved: 1,
  rejected: 2
}

def set_status(status)
  case status
  when :pending
    "Waiting for approval"
  when :approved
    "All good!"
  when :rejected
    "Sorry, denied"
  end
end
```

## Symbol Properties

### Immutable

Symbols cannot be changed:

```ruby
name = :hello
# No methods like upcase!, reverse!, etc.
# Symbols are frozen by default
```

### Memory Efficient

```ruby
# Creating 10,000 strings uses more memory
10_000.times { "hello" }  # Creates 10,000 objects

# Creating 10,000 symbols uses less memory
10_000.times { :hello }   # References same object 10,000 times
```

### Faster Comparisons

```ruby
# Symbol comparison is faster
:name == :name  # Compare object IDs (fast)

# String comparison is slower
"name" == "name"  # Compare character by character
```

## Converting Between Symbols and Strings

```ruby
# Symbol to String
:hello.to_s   # "hello"

# String to Symbol
"hello".to_sym   # :hello
"hello".intern   # :hello (older syntax)

# Useful for user input
user_input = "name"
hash_key = user_input.to_sym  # :name
user[hash_key]
```

## Symbol Methods

Symbols support some string-like operations:

```ruby
:hello.upcase     # :HELLO
:hello.capitalize # :Hello
:hello.length     # 5
:hello.to_s.reverse  # "olleh" (converts to string first)
```

## Modern Hash Syntax

Ruby provides clean syntax for symbol keys:

```ruby
# Old style (hashrocket)
user = { :name => "Alice", :age => 25 }

# New style (JSON-like)
user = { name: "Alice", age: 25 }

# They're equivalent!
{ :name => "Alice" } == { name: "Alice" }  # true
```

## When NOT to Use Symbols

### Don't Use for User Input

```ruby
# Bad: Creating symbols from user input
user_input = gets.chomp
symbol = user_input.to_sym  # Dangerous!
```

**Why?** Symbols are never garbage collected. If users can create arbitrary symbols, you'll have a memory leak.

### Don't Use for Changing Text

```ruby
# Bad: Using symbol for text that changes
message = :hello  # Can't modify

# Good: Use string
message = "hello"
message.upcase!   # Can modify
```

## Practical Examples

### Configuration Hash

```ruby
config = {
  host: "localhost",
  port: 3000,
  timeout: 30,
  retry: true
}

# Access with symbol
config[:host]  # "localhost"
```

### State Machine

```ruby
class Order
  def initialize
    @state = :pending
  end
  
  def approve!
    @state = :approved
  end
  
  def reject!
    @state = :rejected
  end
  
  def approved?
    @state == :approved
  end
end
```

### Method Dispatch

```ruby
operations = {
  add: ->(a, b) { a + b },
  subtract: ->(a, b) { a - b },
  multiply: ->(a, b) { a * b }
}

def calculate(operation, a, b)
  operations[operation].call(a, b)
end

calculate(:add, 5, 3)       # 8
calculate(:multiply, 4, 2)  # 8
```

## Quick Reference

```ruby
# Create symbols
:symbol
:"symbol with spaces"

# Check type
:hello.class  # Symbol

# Convert
:hello.to_s   # "hello"
"hello".to_sym  # :hello

# Compare
:a == :a  # true (same object)
:a == :b  # false

# List all symbols (debugging)
Symbol.all_symbols.size  # Shows how many symbols exist
```

## Best Practices

1. **Use symbols for identifiers** - method names, hash keys, states
2. **Use strings for data** - user input, text that changes, display text
3. **Never create symbols from untrusted input** - memory leak risk
4. **Prefer symbol keys in hashes** - faster and cleaner
5. **Use symbols for constants** - status codes, configuration keys

## Try It Yourself

Complete the exercise to practice working with symbols!
