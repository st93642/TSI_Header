# Exception Safety and RAII

Exception safety ensures that programs remain in a valid, predictable state even when exceptions occur. RAII (Resource Acquisition Is Initialization) is the fundamental C++ idiom that makes exception safety possible.

## Why Exception Safety Matters

Without proper exception safety, programs can:

- **Leak resources**: Files, memory, network connections left open
- **Corrupt data**: Partial operations leave objects in invalid states
- **Cause crashes**: Destructors not called, invariants violated
- **Behave unpredictably**: Different behavior on success vs failure

Exception safety provides:

- **Predictable behavior**: Clear guarantees about program state
- **Resource management**: Automatic cleanup on errors
- **Composability**: Safe components can be combined safely
- **Reliability**: Robust programs that handle errors gracefully

## Learning goals

- Understand exception safety guarantees (basic, strong, no-throw).
- Implement RAII for automatic resource management.
- Write exception-safe functions and classes.
- Use smart pointers and containers safely.
- Apply copy-and-swap idiom for strong exception safety.

## Exception safety guarantees

Three levels of exception safety that operations can provide.

### No-throw guarantee

Operation never throws exceptions (best guarantee).

```cpp
class SafeContainer {
private:
    std::vector<int> data_;
    
public:
    // No-throw operations
    void clear() noexcept {
        data_.clear();  // vector::clear() is noexcept
    }
    
    bool empty() const noexcept {
        return data_.empty();  // noexcept
    }
    
    size_t size() const noexcept {
        return data_.size();  // noexcept
    }
    
    void swap(SafeContainer& other) noexcept {
        data_.swap(other.data_);  // noexcept
    }
};
```

### Strong guarantee

If operation fails, state remains unchanged (rollback on error).

```cpp
class TransactionalContainer {
private:
    std::vector<int> data_;
    
public:
    // Strong exception safety: copy-and-swap
    void add_all(const std::vector<int>& items) {
        std::vector<int> temp = data_;  // Copy current state
        
        try {
            for (int item : items) {
                temp.push_back(item);  // May throw
            }
        } catch (...) {
            // If anything fails, original state unchanged
            return;  // temp is discarded, data_ unchanged
        }
        
        // Success: commit the changes
        data_.swap(temp);
    }
    
    // Alternative: direct strong guarantee
    void push_back_strong(int value) {
        size_t original_size = data_.size();
        
        try {
            data_.push_back(value);  // May throw
        } catch (...) {
            // Ensure we're back to original state
            if (data_.size() > original_size) {
                data_.pop_back();
            }
            throw;  // Re-throw
        }
    }
};
```

### Basic guarantee

Valid but unspecified state after exception (minimal guarantee).

```cpp
class BasicSafeContainer {
private:
    std::vector<int> data_;
    
public:
    // Basic exception safety: valid state, but may be modified
    void add_range(std::vector<int>::iterator begin, 
                   std::vector<int>::iterator end) {
        try {
            for (auto it = begin; it != end; ++it) {
                data_.push_back(*it);  // May throw
                // If exception here, some items may be added
            }
        } catch (...) {
            // State is valid but partially modified
            // Client must handle this
            throw;
        }
    }
};
```

## RAII: Resource Acquisition Is Initialization

RAII binds resource lifetime to object lifetime.

### Basic RAII pattern

```cpp
class FileHandle {
private:
    FILE* file_;
    
public:
    FileHandle(const char* filename, const char* mode) {
        file_ = fopen(filename, mode);
        if (!file_) {
            throw std::runtime_error("Failed to open file");
        }
    }
    
    ~FileHandle() {
        if (file_) {
            fclose(file_);
        }
    }
    
    // Disable copying (or implement properly)
    FileHandle(const FileHandle&) = delete;
    FileHandle& operator=(const FileHandle&) = delete;
    
    // Allow moving
    FileHandle(FileHandle&& other) noexcept : file_(other.file_) {
        other.file_ = nullptr;
    }
    
    FileHandle& operator=(FileHandle&& other) noexcept {
        if (this != &other) {
            if (file_) fclose(file_);
            file_ = other.file_;
            other.file_ = nullptr;
        }
        return *this;
    }
    
    FILE* get() const { return file_; }
};

void process_file(const char* filename) {
    FileHandle file(filename, "r");  // File opened here
    
    // Use file...
    char buffer[1024];
    if (fgets(buffer, sizeof(buffer), file.get())) {
        std::cout << "Read: " << buffer << std::endl;
    }
    
    // File automatically closed when function exits
}  // Destructor called here
```

### RAII for different resources

```cpp
// Memory
class Buffer {
    char* data_;
    size_t size_;
public:
    Buffer(size_t size) : data_(new char[size]), size_(size) {}
    ~Buffer() { delete[] data_; }
    char* data() { return data_; }
    size_t size() { return size_; }
};

// Mutex
class LockGuard {
    std::mutex& mutex_;
public:
    LockGuard(std::mutex& m) : mutex_(m) { mutex_.lock(); }
    ~LockGuard() { mutex_.unlock(); }
};

// Network connection
class Connection {
    int socket_;
public:
    Connection(const char* host, int port) {
        // Open connection...
        socket_ = /* socket creation */;
    }
    ~Connection() {
        // Close connection...
    }
};
```

## Smart pointers and RAII

C++ standard library provides RAII wrappers.

### unique_ptr

```cpp
#include <memory>

void process_data() {
    // Automatic memory management
    std::unique_ptr<int[]> buffer(new int[1000]);
    
    // Use buffer...
    buffer[0] = 42;
    
    // Memory automatically freed
}

class Widget {
public:
    Widget() { std::cout << "Widget created" << std::endl; }
    ~Widget() { std::cout << "Widget destroyed" << std::endl; }
};

void factory_example() {
    std::unique_ptr<Widget> w = std::make_unique<Widget>();
    // Widget automatically destroyed
}

std::unique_ptr<Widget> create_widget() {
    return std::make_unique<Widget>();
}
```

### shared_ptr

```cpp
#include <memory>

class ConnectionPool {
private:
    struct Connection {
        // Connection data
        int ref_count = 0;
    };
    
    std::shared_ptr<Connection> connection_;
    
public:
    ConnectionPool() {
        connection_ = std::make_shared<Connection>();
    }
    
    std::shared_ptr<Connection> get_connection() {
        return connection_;  // Shared ownership
    }
};
```

### weak_ptr

```cpp
#include <memory>

class Cache {
private:
    std::weak_ptr<ExpensiveObject> cached_;
    
public:
    std::shared_ptr<ExpensiveObject> get_object() {
        std::shared_ptr<ExpensiveObject> obj = cached_.lock();
        if (!obj) {
            // Create new object
            obj = std::make_shared<ExpensiveObject>();
            cached_ = obj;
        }
        return obj;
    }
};
```

## Exception-safe class design

### Rule of Zero/Three/Five

```cpp
class SafeClass {
private:
    std::vector<int> data_;
    std::unique_ptr<Widget> widget_;
    
public:
    // Rule of Zero: Use RAII types, no custom copy/move/destroy
    SafeClass() = default;
    
    // Compiler generates all special members correctly
    // No need to write copy constructor, assignment, destructor
};
```

### Copy-and-swap idiom

```cpp
class Container {
private:
    std::vector<int> data_;
    
public:
    // Copy assignment with strong exception safety
    Container& operator=(const Container& other) {
        if (this != &other) {
            Container temp(other);  // May throw
            swap(temp);             // No-throw
        }
        return *this;
    }
    
    // Move assignment
    Container& operator=(Container&& other) noexcept {
        swap(other);
        return *this;
    }
    
    void swap(Container& other) noexcept {
        data_.swap(other.data_);
    }
    
    friend void swap(Container& a, Container& b) noexcept {
        a.swap(b);
    }
};
```

## Exception-safe STL usage

### Safe container operations

```cpp
#include <vector>
#include <algorithm>

void safe_operations(std::vector<int>& vec) {
    // Safe: no exceptions in these operations
    vec.clear();           // noexcept
    vec.size();            // noexcept
    vec.empty();           // noexcept
    
    // Potentially throwing
    vec.push_back(42);     // May throw bad_alloc
    
    // Exception-safe algorithms
    std::sort(vec.begin(), vec.end());  // Strong guarantee
    
    // Exception-safe erase-remove idiom
    auto it = std::remove_if(vec.begin(), vec.end(), 
                            [](int x) { return x < 0; });
    vec.erase(it, vec.end());  // noexcept
}
```

### Safe map operations

```cpp
#include <map>

void safe_map_operations(std::map<std::string, int>& m) {
    // Safe insertion with checking
    auto [it, inserted] = m.emplace("key", 42);
    if (!inserted) {
        // Key already exists
        it->second = 42;  // Safe assignment
    }
    
    // Safe lookup
    auto it2 = m.find("key");
    if (it2 != m.end()) {
        int value = it2->second;  // Safe access
    }
}
```

## Exception-safe function design

### Parameter passing

```cpp
// Safe parameter passing
void process_data(const std::vector<int>& data) {  // const ref: no copy
    // Read-only access, no exceptions from copying
}

void add_item(std::vector<int>& vec, int value) {
    vec.push_back(value);  // May throw, but vec remains valid
}

// Return by value (safe with move semantics)
std::vector<int> create_data() {
    std::vector<int> result;
    // Fill result...
    return result;  // Move constructor called
}
```

### Two-phase construction

```cpp
class ComplexObject {
private:
    std::unique_ptr<Resource1> res1_;
    std::unique_ptr<Resource2> res2_;
    bool initialized_ = false;
    
public:
    // Constructor doesn't do real work
    ComplexObject() = default;
    
    // Separate initialization that can fail
    void initialize() {
        try {
            res1_ = std::make_unique<Resource1>();
            res2_ = std::make_unique<Resource2>();
            initialized_ = true;
        } catch (...) {
            // Cleanup partial initialization
            res1_.reset();
            res2_.reset();
            throw;
        }
    }
    
    bool is_initialized() const { return initialized_; }
};
```

## Testing exception safety

### Testing strategies

```cpp
#include <gtest/gtest.h>

// Test no-throw guarantee
TEST(ContainerTest, ClearIsNoThrow) {
    std::vector<int> vec = {1, 2, 3};
    EXPECT_NO_THROW(vec.clear());
}

// Test strong guarantee
TEST(ContainerTest, PushBackStrongGuarantee) {
    std::vector<ThrowingType> vec = {ThrowingType(1), ThrowingType(2)};
    auto original = vec;
    
    // This should either succeed or leave vec unchanged
    try {
        vec.push_back(ThrowingType(3));
    } catch (...) {
        EXPECT_EQ(vec, original);  // Strong guarantee
    }
}

// Test with mock exceptions
class ThrowingAllocator : public std::allocator<int> {
private:
    static int throw_after_;
    
public:
    static void set_throw_after(int n) { throw_after_ = n; }
    
    pointer allocate(size_type n) {
        if (--throw_after_ <= 0) {
            throw std::bad_alloc();
        }
        return std::allocator<int>::allocate(n);
    }
};
```

## Mini project: Exception-safe database

Create a database class with exception safety guarantees.

1. Implement RAII for database connections
2. Provide strong exception safety for transactions
3. Handle connection failures gracefully
4. Use smart pointers for resource management

### Success criteria

- No resource leaks on exceptions
- Strong exception safety for operations
- Proper cleanup on failures
- Clear error propagation

## Guided practice challenges

1. Implement RAII wrapper for file handles
2. Create exception-safe stack with strong guarantee
3. Write copy-and-swap for custom container
4. Add exception safety to existing class
5. Test exception safety with custom allocators

## Self-check questions

1. What are the three exception safety guarantees?
2. How does RAII work?
3. What is the copy-and-swap idiom?
4. When should you use unique_ptr vs shared_ptr?
5. How do you test exception safety?

## Recap and next steps

Exception safety and RAII enable robust C++ programs. Next, explore file I/O and streams.
