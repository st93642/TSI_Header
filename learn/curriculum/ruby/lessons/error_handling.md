# Error Handling with Exceptions

Error handling in Ruby uses exceptions to gracefully handle problems that occur
during program execution.

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

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Error Handling — Custom Errors, Retries & Fail-Fast (Appendix — error_handling-ruby3)

Practical guidance on designing error classes, retry strategies for transient failures, and patterns for fail-fast behaviour.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Custom errors</td><td>Domain-specific failures</td><td>Inherit from `StandardError`</td></tr>
    <tr><td>Retries</td><td>Transient network failures</td><td>Use exponential backoff and limit retries</td></tr>
    <tr><td>Fail-fast</td><td>Detect catastrophic state</td><td>Raise early and let CI catch regressions</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: retry

```ruby
require 'net/http'

retries = 0
begin
  res = Net::HTTP.get_response(uri)
rescue StandardError
  retries += 1
  retry if retries < 3
  raise
end
```

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — error_handling-ruby3)

1. Implement a retry wrapper with exponential backoff and add tests that
   simulate transient failures.
2. Create a small custom error class hierarchy and add tests asserting rescue
   behavior only catches intended error types.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Observability & Error Reporting (Appendix — error_handling-ruby4)

Notes on turning runtime errors into actionable signals: structured logging,
correlation ids, and graceful degradation.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Technique</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Structured logs</td><td>JSON log lines</td><td>Include correlation_id and error class</td></tr>
    <tr><td>Correlation</td><td>Pass request ids</td><td>Helps trace failures across components</td></tr>
    <tr><td>Graceful fallback</td><td>Fallback strategies</td><td>Prefer default behaviour over crash when safe</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: custom error with metadata

```ruby
class PaymentError < StandardError
  attr_reader :code, :details
  def initialize(msg = nil, code: nil, details: {})
    super(msg)
    @code = code
    @details = details
  end
end

raise PaymentError.new('failed', code: 402, details: {order: 42})
```

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — error_handling-ruby4)

1. Create a middleware that captures exceptions, logs them with a correlation id
   and returns a sanitized error response.
2. Add tests that assert the middleware logs the expected JSON fields and that
   sensitive fields are redacted.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-enable MD013 -->
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
- Always provide meaningful error messages

<!-- markdownlint-disable MD033 MD034 MD040 MD010 MD022 MD032 MD024 -->

## Practical Appendix: Error Handling — Retries, Observability & Tips (Appendix  error_handling-hidden-20251005)

Concise, practical patterns for reliable error handling and observability.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Pattern</th><th>When</th><th>Note</th></tr>
  </thead>
  <tbody>
    <tr><td>Retry with backoff</td><td>Transient network failures</td><td>limit attempts; log each retry</td></tr>
    <tr><td>Custom errors</td><td>Domain failures</td><td>inherit from StandardError and include metadata</td></tr>
    <tr><td>Structured logs</td><td>Production errors</td><td>JSON lines with correlation ids</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Tiny example — retry wrapper

```ruby
def with_retries(max = 3)
  attempts = 0
  begin
    attempts += 1
    yield
  rescue StandardError => e
    raise if attempts >= max
    sleep(2**attempts * 0.1)
    retry
  end
end
```

### Exercises

1. Implement `with_retries` with exponential jitter and add a test that
   simulates a transient failure.
2. Create a small custom error `MyApp::ValidationError` with a `details` hash
   and write a test that asserts rescue scope.


<!-- Practical Appendix: Reference and further reading -->

### Practical Appendix
This appendix contains brief practical notes and quick references to complement the lesson content. It is intentionally short and safe: no code execution or large data dumps.

- Reference: Official documentation and language core references are excellent further reading sources. Follow the standard docs for authoritative examples.
- Quick tips:
  - Re-run the examples in a REPL to experiment with small changes.
  - Use small, focused test cases when validating behavior.
  - Prefer idiomatic standard-library helpers for clarity and maintainability.

Further reading and sources:
- Official language documentation (search for "official <LANG> docs" where <LANG> is the lesson's language).
- Standard library reference and API pages.
- For curriculum authors: keep examples minimal and include runnable snippets in fenced code blocks.

*End of Practical Appendix.*
