# Hashes

Hashes map keys to values. They power configuration objects, JSON parsing, HTTP headers—any time you need fast lookups by identifier. Ruby’s `Hash` preserves insertion order, supports flexible defaults, and offers fluent transformation methods. Mastering them means handling nested data, merging safely, and choosing the right key types.

## Learning goals

- Create hashes using literal syntax, keyword arguments, and default procs.
- Fetch, update, and delete entries while guarding against missing keys.
- Transform and filter hashes with `map`, `each_with_object`, `transform_keys`, and friends.
- Merge deep structures predictably and reason about symbol vs string keys.
- Navigate nested hashes safely with `dig`, pattern matching, and default values.

## Building hashes

```ruby
empty = {}

user = {
  name: "Leandro",
  nickname: "TK",
  nationality: "Brazilian",
  age: 24
}

scores = Hash[[:ruby, 95], [:js, 88]]
#=> {:ruby=>95, :js=>88}

# From keyword arguments

def build_user(name:, email:)
  { name:, email: }
end
```

Hash literals accept both symbol (`key: value`) and string (`"key" => value`) styles. Stick to symbols for API-like keys unless external data uses strings.

## Accessing values

```ruby
person = { name: "Alice", age: 30, city: "New York" }

person[:name]        #=> "Alice"
person[:missing]     #=> nil (default)
person.fetch(:age)   #=> 30

person.fetch(:role, "guest")
#=> "guest" (provides fallback)

person.fetch(:role) { compute_role } # lazy block
```

Use `fetch` when a missing key is exceptional—it raises `KeyError` without a fallback, making bugs easier to spot.

## Adding, updating, deleting

```ruby
person[:city] = "Boston"     # add or update
person[:age] += 1             # mutate existing value

person.delete(:nickname)      # returns deleted value or nil
deleted = person.delete(:nickname) { "already removed" }

person.compact!               # remove keys with nil values
person[:tags] ||= []          # set default array
```

`Hash#store` is an alias for `[]=`. Use `delete_if` or `reject!` to filter in place.

## Iterating and transforming

```ruby
settings = { theme: "dark", per_page: 50, debug: false }

settings.each do |key, value|
  puts "#{key}=#{value}"
end

uppercased = settings.transform_values { |value| value.to_s.upcase }

symbolized = { "enabled" => true, "level" => "info" }
  .transform_keys(&:to_sym)

selected = settings.select { |key, _| key != :debug }

# Build a new hash from scratch
lookup = %w[ruby rails redis].each_with_index.each_with_object({}) do |(lang, index), hash|
  hash[lang.to_sym] = index
end
```

`Hash#map` returns an array; to remain a hash, use `to_h` or `each_with_object({})`.

## Merge strategies

```ruby
defaults = { retries: 3, timeout: 5 }
overrides = { timeout: 10 }

combined = defaults.merge(overrides)
#=> { retries: 3, timeout: 10 }

# Custom merge logic per key
combined = defaults.merge(overrides) do |_key, default, override|
  [default, override].max
end

# Deep merge helper
require "active_support/core_ext/hash/deep_merge"

app_config = defaults.deep_merge(overrides) # if ActiveSupport is available
```

Without ActiveSupport, write a recursive helper to merge nested hashes carefully.

## Default values and procs

```ruby
counts = Hash.new(0)
counts[:ruby] += 1
counts[:ruby] += 1
counts[:python]
#=> 0 (returns copy of default)

list_hash = Hash.new { |hash, key| hash[key] = [] }
list_hash[:errors] << "Missing email"
list_hash[:errors] << "Password too short"
```

Default procs can initialize nested structures on demand—perfect for grouping or counting operations.

## Symbol vs string keys

Symbols are immutable and reused, making them a natural default. Use strings when:

- You interact with JSON or external APIs that specify string keys.
- Keys contain spaces or dynamic content.
- You rely on case-sensitive user input as key names.

Normalize keys on ingestion (`transform_keys(&:to_sym)`), but document expectations so callers know what to provide.

## Order and enumeration

Ruby preserves insertion order. Inserted keys appear in `each`, `keys`, and `values` in the order they were added. Use `hash.to_a` to convert to `[[key, value], ...]` arrays, or `hash.sort_by` to reorder.

```ruby
order = { first: "setup", second: "migrate" }
order[:third] = "deploy"
order.each { |step, action| puts "#{step}: #{action}" }
```

## Nested hashes and `dig`

```ruby
profile = {
  name: "Cam",
  preferences: {
    notifications: { email: true, push: false }
  }
}

profile.dig(:preferences, :notifications, :email) #=> true

# Avoid KeyError when nesting may be missing
timezone = profile.dig(:preferences, :timezone) || "UTC"
```

Use `dig` for safe navigation. For destructive updates, combine with safe assignment:

```ruby
profile[:preferences] ||= {}
profile[:preferences][:timezone] = "UTC"
```

## Pattern matching with hashes

```ruby
case response
in { status: 200, body: }
  parse(body)
in { status: 404, path: }
  warn "Not found: #{path}"
in { status: 500..599, error: }
  raise error
else
  raise "Unhandled response"
end
```

Pattern matching (Ruby 2.7+) destructures hashes, binds values, and lets you guard with `if`/`unless`.

## Serialization and conversion

```ruby
params = { name: "Ada", language: "Ruby" }

params.to_a          #=> [[:name, "Ada"], [:language, "Ruby"]]
params.to_h          #=> {:name=>"Ada", :language=>"Ruby"}

require "json"
json = params.to_json
Hash[JSON.parse(json)] #=> {"name"=>"Ada", "language"=>"Ruby"}
```

Use `to_h` on enumerables of pairs to create hashes, and `hash1 <= hash2` to check subset relationships.

## Performance notes

- Hash lookups are O(1) on average; collisions can degrade performance but Ruby handles them well.
- Symbols are reused but aren’t garbage collected on older Ruby versions—avoid generating unbounded symbol keys dynamically.
- For large static mappings, freeze the hash (`HASH = {...}.freeze`) to prevent accidental mutation and enable sharing.
- `Hash#compact` and `Hash#slice` (ActiveSupport) reduce payload size before serialization.

## Guided practice

1. **Environment loader**
   - Parse an array of `KEY=VALUE` strings into a hash with symbol keys.
   - Use `transform_values` to coerce numbers to integers and `"true"/"false"` to booleans.

2. **Nested fetch with defaults**
   - Write `setting_for(config, key_path, default)` that walks a list of keys using `dig` and returns the default if any key is missing.
   - Support key paths specified as arrays or dot-delimited strings.

3. **Deep merge builder**
   - Implement `deep_merge(hash, other)` recursively without ActiveSupport.
   - Add an optional block that receives conflicting values and returns the merged result.

4. **Frequency counter**
   - Count word occurrences in a string, returning the top three words and their counts.
   - Use a hash with default proc and `sort_by` for ranking.

5. **Immutable settings**
   - Build a frozen configuration hash and show how attempts to mutate it raise errors.
   - Provide `with_override(config, overrides)` that returns a new merged hash without mutating the original.

## Self-check questions

1. What’s the difference between `hash[:key]` and `hash.fetch(:key)` when the key is missing?
2. How does a default proc change behavior compared to a static default value in `Hash.new(default)`?
3. When merging hashes, how can you customize conflict resolution per key?
4. How does `dig` help avoid nested `nil` checks, and what does it return if any key along the path is missing?
5. Why might you freeze a hash, and how do you provide updated copies without mutating the original?

Hashes are Ruby’s flexible dictionaries—lean on them to organize structured data, memoize results, and communicate intent through clear key names. Combine them with Enumerable power, and they’ll become your go-to data structure for everyday Ruby work.
