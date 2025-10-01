# Symbols and Identifiers Exercise
# Practice working with Ruby symbols and understanding their use cases

# Instructions:
# Symbols are immutable identifiers in Ruby, often used as keys in hashes,
# method names, and for representing states or constants.

# Your task is to implement the methods that the tests expect.
# Look at the test failures to understand what each method should do.

# 1. Symbol Creation and Comparison
# Create a method that returns a symbol version of a string
def string_to_symbol(string)
  # Convert string to symbol
end

# Create a method that checks if two symbols are the same
def symbols_equal?(sym1, sym2)
  # Compare two symbols
end

# 2. Hash Keys with Symbols
# Create a method that creates a hash with symbol keys
def create_person_hash(name, age, city)
  # Return hash with :name, :age, :city as keys
end

# Create a method that accesses values using symbol keys
def get_person_name(person_hash)
  # Return the :name value from the hash
end

# 3. Symbol vs String Performance
# Create a method that demonstrates symbol immutability
def symbols_are_immutable
  # Show that symbols with same content are identical objects
  # Return true if :test.object_id == :test.object_id
end

# Example usage (uncomment these to test your code):
# puts string_to_symbol("hello")        # Should return :hello
# puts symbols_equal?(:test, :test)     # Should return true
# person = create_person_hash("Alice", 25, "NYC")
# puts get_person_name(person)          # Should return "Alice"
# puts symbols_are_immutable            # Should return true