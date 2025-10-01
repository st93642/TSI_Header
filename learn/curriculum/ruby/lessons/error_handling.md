# Error Handling with Exceptions

Error handling in Ruby uses exceptions to gracefully handle problems that occur during program execution.

## What are Exceptions?

Exceptions are objects representing errors or unexpected events:

```ruby
# This raises an exception
10 / 0  # => ZeroDivisionError
```

## Basic Exception Handling

Use `begin...rescue...end` to catch exceptions:

```ruby
begin
  # Code that might raise an exception
  result = 10 / 0
  puts result
rescue
  # Handle the error
  puts "An error occurred!"
end

# Program continues
puts "Program still running"
```

## Rescue Specific Exceptions

```ruby
begin
  file = File.open("missing.txt")
rescue Errno::ENOENT
  puts "File not found!"
rescue StandardError => e
  puts "Error: #{e.message}"
end
```

## Multiple Rescue Clauses

```ruby
begin
  # Some risky operation
  result = perform_calculation
rescue ZeroDivisionError
  puts "Cannot divide by zero"
rescue TypeError
  puts "Wrong type provided"
rescue StandardError => e
  puts "Something else went wrong: #{e.message}"
end
```

## The `ensure` Clause

Code in `ensure` always executes, even if exception occurs:

```ruby
begin
  file = File.open("data.txt", "w")
  file.write("Important data")
  # Something goes wrong here
  risky_operation()
rescue StandardError => e
  puts "Error: #{e.message}"
ensure
  file.close if file  # Always close the file
  puts "Cleanup completed"
end
```

## The `else` Clause

Runs only if no exception was raised:

```ruby
begin
  result = 10 / 2
rescue ZeroDivisionError
  puts "Cannot divide by zero"
else
  puts "Success! Result: #{result}"
ensure
  puts "Operation complete"
end
```

## Raising Exceptions

You can raise your own exceptions:

```ruby
def withdraw(amount)
  if amount > balance
    raise "Insufficient funds"
  end
  @balance -= amount
end

# Raise specific exception type
def set_age(age)
  if age < 0
    raise ArgumentError, "Age cannot be negative"
  end
  @age = age
end
```

## Retry Failed Operations

```ruby
attempts = 0

begin
  attempts += 1
  # Try to connect to server
  connect_to_server
rescue ConnectionError => e
  if attempts < 3
    puts "Retrying... (attempt #{attempts})"
    sleep(1)
    retry  # Try the begin block again
  else
    puts "Failed after 3 attempts"
    raise  # Re-raise the exception
  end
end
```

## Custom Exception Classes

```ruby
class InsufficientFundsError < StandardError
  def initialize(msg = "Not enough funds in account")
    super
  end
end

class BankAccount
  attr_reader :balance

  def initialize(balance)
    @balance = balance
  end

  def withdraw(amount)
    if amount > @balance
      raise InsufficientFundsError, "Tried to withdraw #{amount}, but only have #{@balance}"
    end
    @balance -= amount
  end
end

# Usage
account = BankAccount.new(100)
begin
  account.withdraw(150)
rescue InsufficientFundsError => e
  puts "Error: #{e.message}"
end
```

## Common Exception Types

- `StandardError` - Base for most errors
- `ArgumentError` - Wrong number/type of arguments
- `TypeError` - Wrong type of object
- `NameError` - Undefined variable or method
- `NoMethodError` - Method doesn't exist
- `ZeroDivisionError` - Division by zero
- `IOError` - Input/output error
- `RuntimeError` - Generic runtime error

## Exception Hierarchy

```ruby
Exception
  ├── NoMemoryError
  ├── ScriptError
  ├── SignalException
  ├── StandardError  # <-- Rescue this one
  │   ├── ArgumentError
  │   ├── IOError
  │   ├── NameError
  │   ├── TypeError
  │   ├── ZeroDivisionError
  │   └── RuntimeError
  └── SystemExit
```

## Best Practices

```ruby
# Good: Specific exceptions
begin
  risky_operation
rescue SpecificError => e
  handle_error(e)
end

# Bad: Catch all exceptions (hides bugs)
begin
  risky_operation
rescue Exception => e  # DON'T DO THIS
  # Catches SystemExit and other critical errors
end

# Good: Always use StandardError or more specific
begin
  risky_operation
rescue StandardError => e
  puts e.message
  puts e.backtrace  # Show stack trace
end
```

## Practical Examples

### Safe division

```ruby
def safe_divide(a, b)
  a / b
rescue ZeroDivisionError
  puts "Cannot divide by zero"
  nil
end

result = safe_divide(10, 0)  # => nil
```

### File reading with error handling

```ruby
def read_config(filename)
  File.read(filename)
rescue Errno::ENOENT
  puts "Config file not found, using defaults"
  "{}"
rescue StandardError => e
  puts "Error reading config: #{e.message}"
  "{}"
end
```

## Key Takeaways

- Use `begin...rescue...end` to handle exceptions
- `rescue` catches and handles errors
- `ensure` always executes (cleanup code)
- `else` runs if no exception occurred
- `raise` to throw exceptions
- `retry` to attempt operation again
- Create custom exception classes
- Rescue `StandardError`, not `Exception`
- Always provide meaningful error messages
