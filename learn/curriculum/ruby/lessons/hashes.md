# Hashes: Key-Value Collections

Hashes are collections of key-value pairs, similar to dictionaries in other languages. They provide fast lookup by key.

## Creating Hashes

```ruby
# Empty hash
person = {}

# Hash with string keys
person = {
  "name" => "Leandro",
  "nickname" => "TK",
  "nationality" => "Brazilian"
}

# Modern symbol syntax (preferred)
person = {
  name: "Leandro",
  nickname: "TK",
  nationality: "Brazilian",
  age: 24
}
```

## Accessing Values

```ruby
person = {
  name: "Alice",
  age: 30,
  city: "New York"
}

person[:name]   # => "Alice"
person[:age]    # => 30
person[:city]   # => "New York"
```

## Adding and Updating

```ruby
person = { name: "Bob" }

# Add new key-value
person[:age] = 25
person[:city] = "Boston"

# Update existing value
person[:age] = 26

puts person
# => {:name=>"Bob", :age=>26, :city=>"Boston"}
```

## Hash Methods

```ruby
person = { name: "Alice", age: 30, city: "NYC" }

person.keys     # => [:name, :age, :city]
person.values   # => ["Alice", 30, "NYC"]
person.length   # => 3
person.empty?   # => false
person.has_key?(:name)  # => true
```

## Iterating Over Hashes

```ruby
person = {
  name: "Alice",
  age: 30,
  city: "NYC"
}

# Iterate with key and value
person.each do |key, value|
  puts "#{key}: #{value}"
end

# Output:
# name: Alice
# age: 30
# city: NYC
```

## Symbols vs Strings as Keys

**Symbols** (`:name`) are preferred over strings (`"name"`) as keys because:
- More memory efficient
- Faster lookup
- Conventional in Ruby

```ruby
# Using symbols (preferred)
user = { name: "Alice", age: 30 }

# Using strings (less common)
user = { "name" => "Alice", "age" => 30 }
```

## Default Values

```ruby
# Returns nil for missing keys
hash = {}
hash[:missing]  # => nil

# Set default value
hash = Hash.new(0)
hash[:missing]  # => 0
```

## Key Takeaways

- Hashes store key-value pairs: `{ key: value }`
- Use symbols as keys: `:name`, `:age`
- Access with square brackets: `hash[:key]`
- Add/update: `hash[:key] = value`
- Iterate with `.each do |key, value|`
- Methods: `.keys`, `.values`, `.has_key?`
