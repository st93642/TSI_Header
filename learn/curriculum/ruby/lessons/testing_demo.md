# Testing System Demo

## Overview

This lesson demonstrates the enhanced testing capabilities of the TSI Header extension. The testing system now supports multiple types of tests to validate different aspects of your code behavior.

## Test Types

### 1. Output Tests

Output tests capture what your code prints to the console using `puts`, `print`, or `p`. This is essential for validating user-facing output and debugging information.

```ruby
def print_greeting(name)
  puts "Hello, #{name}!"
end

# This will be captured and compared against expected output
print_greeting("Alice") # Expected: "Hello, Alice!"
```

### 2. Exception Tests

Exception tests verify that your code properly raises errors when given invalid input. This ensures robust error handling.

```ruby
def validate_age(age)
  raise ArgumentError, "Age cannot be negative" if age < 0
  age
end

# This will be tested to ensure it raises ArgumentError
validate_age(-5) # Should raise ArgumentError
```

### 3. Side Effect Tests

Side effect tests validate operations that modify external state, such as file creation, database updates, or other persistent changes.

```ruby
def create_log_file(message)
  File.write('test.log', message)
end

# This will be tested to ensure the file is created with correct content
create_log_file('Test message') # Should create test.log with "Test message"
```

### 4. Regular Return Value Tests

Traditional tests that validate return values from method calls.

```ruby
def calculate_total(items)
  items.sum
end

# This will be tested by comparing the return value
calculate_total([1, 2, 3, 4]) # Expected: 10
```

## Implementation Details

### Output Capture

The testing system uses `StringIO` to capture stdout:

```ruby
require 'stringio'

captured_output = StringIO.new
original_stdout = $stdout
$stdout = captured_output

# Your code that prints output
puts "Hello, World!"

$stdout = original_stdout
output = captured_output.string # => "Hello, World!\n"
```

### Exception Testing

Exception tests use `assert_raises` to verify expected errors:

```ruby
assert_raises(ArgumentError) do
  validate_age(-1)
end
```

### Side Effect Validation

Side effect tests can include custom setup and assertion code:

```ruby
# Setup (optional)
File.delete('test.log') if File.exist?('test.log')

# Execute code
create_log_file('message')

# Assert side effects
assert(File.exist?('test.log'))
assert_equal('message', File.read('test.log').strip)
```

## Best Practices

1. **Use appropriate test types**: Choose the test type that best validates your code's behavior
2. **Test edge cases**: Include tests for boundary conditions and error scenarios
3. **Clean up side effects**: Remove temporary files or reset state after tests
4. **Be specific with expectations**: Make sure expected values match exactly
5. **Document test purposes**: Use descriptive test names that explain what they're validating

## Cross-Language Support

The enhanced testing system supports all languages:

- **Ruby**: Uses Minitest with StringIO for output capture
- **Python**: Uses pytest with io.StringIO for output capture
- **JavaScript**: Uses in-process evaluation with custom output capture

This ensures consistent testing behavior across different programming languages while maintaining language-specific idioms and best practices.
