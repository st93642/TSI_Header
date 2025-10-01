# Testing Basics with Minitest

## Overview

Automated testing is crucial for ensuring your code works correctly and preventing regressions. Ruby's standard testing framework is Minitest, which provides a simple yet powerful way to write and run tests.

## Why Test?

- **Catch bugs early**: Tests help identify issues before they reach production
- **Prevent regressions**: Tests ensure new changes don't break existing functionality
- **Document behavior**: Tests serve as living documentation of what your code should do
- **Enable refactoring**: Tests give confidence when restructuring code

## Minitest Basics

Minitest is Ruby's built-in testing framework. It comes in two main flavors:

- **Minitest::Test**: Unit testing framework
- **Minitest::Spec**: Behavior-driven development (BDD) style testing

## Writing Your First Test

```ruby
require 'minitest/autorun'

class CalculatorTest < Minitest::Test
  def test_addition
    calculator = Calculator.new
    result = calculator.add(2, 3)
    assert_equal 5, result
  end
end
```

## Common Assertions

### Equality Assertions

```ruby
assert_equal expected, actual    # Check if values are equal
assert_same expected, actual     # Check if objects are the same instance
refute_equal expected, actual    # Check if values are NOT equal
```

### Boolean Assertions

```ruby
assert truthy_value              # Check if value is truthy
refute falsy_value               # Check if value is falsy
assert_nil value                 # Check if value is nil
refute_nil value                 # Check if value is NOT nil
```

### Collection Assertions

```ruby
assert_includes collection, item    # Check if collection includes item
assert_empty collection            # Check if collection is empty
assert_kind_of Class, object        # Check object type
```

### Exception Assertions

```ruby
assert_raises ExceptionClass do
  # Code that should raise exception
end
```

## Test Structure

```ruby
require 'minitest/autorun'

class MyClassTest < Minitest::Test
  def setup
    # Code to run before each test method
    @object = MyClass.new
  end

  def teardown
    # Code to run after each test method
    # Cleanup code here
  end

  def test_some_behavior
    # Test code here
    result = @object.some_method
    assert_equal expected_result, result
  end

  def test_another_behavior
    # Another test
  end
end
```

## Running Tests

### Run all tests in a file

```bash
ruby test_file.rb
```

### Run specific test

```ruby
# In your test file
ruby -e "require './test_file.rb'; MyClassTest.new(:test_method_name).run"
```

### Using rake (if you have a Rakefile)

```bash
rake test
```

## Test Naming Conventions

- Test methods should start with `test_`
- Test names should describe what they're testing
- Use descriptive names that explain the expected behavior

```ruby
# Good test names
def test_add_returns_sum_of_two_numbers
def test_empty_list_returns_zero_for_sum
def test_invalid_input_raises_argument_error

# Bad test names
def test_method1
def test_add
def test_error
```

## Testing Best Practices

1. **Test one thing per test**: Each test should verify a single behavior
2. **Use descriptive names**: Test names should explain what they're testing
3. **Test edge cases**: Don't just test the happy path
4. **Keep tests independent**: Tests shouldn't rely on each other
5. **Use setup/teardown**: Initialize test data properly
6. **Test both positive and negative cases**: Test what should work and what shouldn't

## Example: Testing a Simple Class

```ruby
class Calculator
  def add(a, b)
    a + b
  end

  def divide(a, b)
    raise ArgumentError, 'Cannot divide by zero' if b == 0
    a / b
  end
end

# Test file
require 'minitest/autorun'
require_relative 'calculator'

class CalculatorTest < Minitest::Test
  def setup
    @calc = Calculator.new
  end

  def test_add_positive_numbers
    result = @calc.add(2, 3)
    assert_equal 5, result
  end

  def test_add_negative_numbers
    result = @calc.add(-2, -3)
    assert_equal -5, result
  end

  def test_divide_normal_case
    result = @calc.divide(10, 2)
    assert_equal 5, result
  end

  def test_divide_by_zero_raises_error
    assert_raises ArgumentError do
      @calc.divide(10, 0)
    end
  end
end
```

## Understanding Test Output

When tests run, you'll see output like:

```text
Run options: --seed 12345

# Running:

.....

Finished in 0.001234s, 4065.0409 runs/s, 4065.0409 assertions/s.

5 runs, 5 assertions, 0 failures, 0 errors, 0 skips
```

- **Runs**: Number of test methods executed
- **Assertions**: Number of assert statements that passed
- **Failures**: Tests that failed (assertions that were false)
- **Errors**: Unexpected exceptions during test execution
- **Skips**: Tests that were intentionally skipped
