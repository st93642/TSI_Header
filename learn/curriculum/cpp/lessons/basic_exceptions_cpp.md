# Basic Exception Handling

Exception handling is C++'s mechanism for dealing with runtime errors and exceptional conditions. Unlike error codes, exceptions separate error handling from normal program flow, making code cleaner and more robust.

## Why Exception Handling Matters

Traditional error handling with return codes has several problems:

- **Cluttered code**: Error checks interrupt normal logic
- **Ignored errors**: Developers often forget to check return values
- **Resource leaks**: Early returns can skip cleanup code
- **Complex control flow**: Nested error handling becomes unreadable

Exceptions solve these by:

- **Separating concerns**: Error handling is separate from normal flow
- **Automatic propagation**: Exceptions unwind the stack automatically
- **Resource safety**: Destructors run during stack unwinding
- **Clear intent**: Exception specifications make interfaces explicit

## Learning goals

- Use try, catch, and throw for basic exception handling.
- Understand exception propagation and stack unwinding.
- Handle different exception types appropriately.
- Write exception-safe code with proper cleanup.

## Basic exception syntax

The three keywords: try, catch, throw.

### Throwing exceptions

```cpp
#include <stdexcept>
#include <iostream>

double divide(double numerator, double denominator) {
    if (denominator == 0.0) {
        throw std::invalid_argument("Division by zero");
    }
    return numerator / denominator;
}

int main() {
    try {
        double result = divide(10.0, 0.0);
        std::cout << "Result: " << result << std::endl;
    } catch (const std::invalid_argument& e) {
        std::cout << "Error: " << e.what() << std::endl;
    }
    
    std::cout << "Program continues..." << std::endl;
}
```

### Exception types

C++ provides standard exception classes in `<stdexcept>`:

```cpp
#include <stdexcept>

// Logic errors (programmer mistakes)
std::logic_error - Base for logic errors
  std::invalid_argument - Invalid arguments
  std::domain_error - Domain errors (math functions)
  std::length_error - Length exceeds limits
  std::out_of_range - Index out of bounds

// Runtime errors (environmental issues)
std::runtime_error - Base for runtime errors
  std::range_error - Result out of range
  std::overflow_error - Arithmetic overflow
  std::underflow_error - Arithmetic underflow

// Other standard exceptions
std::bad_alloc - Memory allocation failure
std::bad_cast - Dynamic cast failure
std::bad_exception - Unexpected exception type
```

### Catching exceptions

```cpp
try {
    // Code that might throw
    risky_operation();
} catch (const std::invalid_argument& e) {
    // Handle invalid arguments
    std::cout << "Invalid argument: " << e.what() << std::endl;
} catch (const std::out_of_range& e) {
    // Handle out of range errors
    std::cout << "Out of range: " << e.what() << std::endl;
} catch (const std::exception& e) {
    // Catch any std::exception-derived type
    std::cout << "Standard exception: " << e.what() << std::endl;
} catch (...) {
    // Catch anything (including non-std::exception types)
    std::cout << "Unknown exception occurred" << std::endl;
}
```

## Exception propagation

Exceptions unwind the call stack until caught.

```cpp
void level3() {
    throw std::runtime_error("Something went wrong in level3");
}

void level2() {
    level3();  // Exception propagates through here
}

void level1() {
    try {
        level2();  // Exception propagates through here too
    } catch (const std::runtime_error& e) {
        std::cout << "Caught in level1: " << e.what() << std::endl;
    }
}

int main() {
    level1();  // Exception is caught here
}
```

### Stack unwinding

When an exception is thrown:

1. Current function stops executing
2. Local objects are destroyed (destructors run)
3. Control returns to calling function
4. Process repeats until exception is caught
5. If never caught, `std::terminate()` is called

```cpp
class Resource {
public:
    Resource() { std::cout << "Resource acquired" << std::endl; }
    ~Resource() { std::cout << "Resource released" << std::endl; }
};

void risky_function() {
    Resource r;  // Constructor called
    throw std::runtime_error("Error!");
    // Destructor called during stack unwinding
}

int main() {
    try {
        risky_function();
    } catch (const std::exception& e) {
        std::cout << "Exception caught: " << e.what() << std::endl;
    }
    // Resource is properly cleaned up
}
```

## Custom exception classes

Create your own exception types by inheriting from `std::exception`.

```cpp
#include <exception>
#include <string>

class DatabaseError : public std::exception {
private:
    std::string message_;
    int error_code_;
    
public:
    DatabaseError(const std::string& message, int error_code)
        : message_(message), error_code_(error_code) {}
    
    const char* what() const noexcept override {
        return message_.c_str();
    }
    
    int error_code() const noexcept {
        return error_code_;
    }
};

class ConnectionFailed : public DatabaseError {
public:
    ConnectionFailed(const std::string& details)
        : DatabaseError("Connection failed: " + details, 1001) {}
};

void connect_to_database() {
    // Simulate connection failure
    throw ConnectionFailed("Timeout after 30 seconds");
}

int main() {
    try {
        connect_to_database();
    } catch (const ConnectionFailed& e) {
        std::cout << "Connection failed: " << e.what() 
                  << " (code: " << e.error_code() << ")" << std::endl;
    } catch (const DatabaseError& e) {
        std::cout << "Database error: " << e.what() << std::endl;
    } catch (const std::exception& e) {
        std::cout << "Other error: " << e.what() << std::endl;
    }
}
```

## Exception specifications

C++11 introduced `noexcept` for specifying whether functions throw.

```cpp
// Function that never throws
void safe_function() noexcept {
    // This function guarantees not to throw
}

// Function that might throw specific types (deprecated in C++17)
void old_style() throw(std::runtime_error, std::logic_error) {
    // Only throws these types
}

// Modern noexcept with condition
template <typename T>
void process(T value) noexcept(std::is_nothrow_copy_constructible<T>::value) {
    // noexcept depends on T's properties
}
```

### Benefits of noexcept

- **Optimization**: Compiler can optimize code knowing no exceptions
- **Documentation**: Clear contract about throwing behavior
- **Safety**: Prevents unexpected exceptions from propagating

## Rethrowing exceptions

Sometimes you want to catch, inspect, and rethrow an exception.

```cpp
void log_and_rethrow() {
    try {
        risky_operation();
    } catch (const std::exception& e) {
        // Log the error
        std::cerr << "Error occurred: " << e.what() << std::endl;
        
        // Add context and rethrow
        throw std::runtime_error(std::string("Context: ") + e.what());
    }
}

void inspect_and_rethrow() {
    try {
        risky_operation();
    } catch (...) {
        // Inspect without knowing the type
        std::cout << "An exception occurred" << std::endl;
        
        // Rethrow the original exception
        throw;
    }
}
```

## Nested try-catch blocks

Handle exceptions at different levels.

```cpp
void inner_operation() {
    try {
        // Low-level error handling
        if (some_condition) {
            throw std::runtime_error("Low-level error");
        }
    } catch (const std::runtime_error& e) {
        // Convert to higher-level exception
        throw std::logic_error("Operation failed due to: " + std::string(e.what()));
    }
}

void outer_operation() {
    try {
        inner_operation();
    } catch (const std::logic_error& e) {
        // Handle at application level
        std::cout << "Application error: " << e.what() << std::endl;
        // Maybe retry or graceful degradation
    }
}
```

## Best practices

### When to use exceptions

- **Unexpected errors**: Network failures, file I/O errors, allocation failures
- **Constructor failures**: Can't return error codes from constructors
- **Deep call stacks**: Error needs to propagate through many layers

### When NOT to use exceptions

- **Expected conditions**: Use return codes for normal flow control
- **Performance critical**: Exceptions have overhead
- **Simple errors**: Sometimes error codes are simpler

### Exception safety guidelines

- **Catch by const reference**: `catch (const std::exception& e)`
- **Order matters**: Catch derived types before base types
- **Don't swallow exceptions**: Re-throw if you can't handle
- **Use RAII**: Resources automatically cleaned up
- **Document exceptions**: Specify what your functions throw

## Mini project: Safe calculator

Create a calculator that handles various error conditions safely.

1. Implement basic arithmetic operations with error checking
2. Handle division by zero, overflow, invalid input
3. Use appropriate exception types
4. Demonstrate proper cleanup with RAII

### Success criteria

- All error conditions properly handled
- No resource leaks
- Clear error messages
- Graceful error recovery

## Guided practice challenges

1. Implement a file reader with exception handling
2. Create a custom exception hierarchy for a game
3. Write exception-safe container operations
4. Add error handling to existing code
5. Implement retry logic with exceptions

## Self-check questions

1. What are the three keywords for exception handling?
2. How does stack unwinding work?
3. What is the difference between `throw` and `throw e`?
4. When should you use `noexcept`?
5. Why catch by const reference?

## Recap and next steps

Exception handling enables robust error management. Next, explore exception safety and RAII.
