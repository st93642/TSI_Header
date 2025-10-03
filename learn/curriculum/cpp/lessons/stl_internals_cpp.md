# STL Internals and Custom Containers

The Standard Template Library (STL) is built on templates, iterators, and algorithms. This lesson explores how STL containers work internally, how to implement custom containers that integrate with STL, and advanced template techniques.

## Why STL Internals Matter

Understanding STL internals is crucial for:

- **Writing efficient code**: Know when to use which container
- **Creating custom containers**: Build STL-compatible types
- **Debugging performance issues**: Understand algorithmic complexity
- **Advanced C++ programming**: Master template metaprogramming
- **Interview preparation**: Common topic in C++ interviews

## Learning goals

- Understand iterator categories and concepts.
- Implement custom iterators for containers.
- Create containers compatible with STL algorithms.
- Use allocator for memory management.
- Design exception-safe containers.

## Iterator concepts

Iterators are the glue that connects STL containers and algorithms. They provide a uniform interface for traversing and accessing container elements.

### Iterator Categories

STL defines five iterator categories with increasing capabilities:

1. **Input Iterator**: Read-only, single-pass
2. **Output Iterator**: Write-only, single-pass  
3. **Forward Iterator**: Read/write, multi-pass
4. **Bidirectional Iterator**: Forward + backward traversal
5. **Random Access Iterator**: Bidirectional + random access

### Iterator Requirements

Each category requires specific operations:

```cpp
// Input iterator requirements
class InputIterator {
public:
    using value_type = T;
    using reference = T&;
    using pointer = T*;
    using iterator_category = std::input_iterator_tag;
    
    reference operator*() const;           // Dereference
    pointer operator->() const;            // Member access
    InputIterator& operator++();           // Pre-increment
    InputIterator operator++(int);         // Post-increment
    bool operator==(const InputIterator&) const;  // Equality
    bool operator!=(const InputIterator&) const;  // Inequality
};

// Forward iterator adds:
class ForwardIterator : public InputIterator {
public:
    using iterator_category = std::forward_iterator_tag;
    // Default constructible
    ForwardIterator();
};

// Bidirectional iterator adds:
class BidirectionalIterator : public ForwardIterator {
public:
    using iterator_category = std::bidirectional_iterator_tag;
    BidirectionalIterator& operator--();   // Pre-decrement
    BidirectionalIterator operator--(int); // Post-decrement
};

// Random access iterator adds:
class RandomAccessIterator : public BidirectionalIterator {
public:
    using iterator_category = std::random_access_iterator_tag;
    RandomAccessIterator& operator+=(difference_type);
    RandomAccessIterator& operator-=(difference_type);
    RandomAccessIterator operator+(difference_type) const;
    RandomAccessIterator operator-(difference_type) const;
    difference_type operator-(const RandomAccessIterator&) const;
    reference operator[](difference_type) const;
    bool operator<(const RandomAccessIterator&) const;
    bool operator>(const RandomAccessIterator&) const;
    bool operator<=(const RandomAccessIterator&) const;
    bool operator>=(const RandomAccessIterator&) const;
};
```

### Iterator Traits

The `std::iterator_traits` class extracts information from iterators:

```cpp
template <typename Iterator>
struct iterator_traits {
    using value_type = typename Iterator::value_type;
    using reference = typename Iterator::reference;
    using pointer = typename Iterator::pointer;
    using iterator_category = typename Iterator::iterator_category;
    using difference_type = typename Iterator::difference_type;
};
```

### Container Iterator Examples

```cpp
int main() {
    std::vector<int> vec = {1, 2, 3, 4, 5};
    std::list<int> lst = {1, 2, 3, 4, 5};
    std::set<int> s = {1, 2, 3, 4, 5};
    
    // All have begin() and end(), but different iterator types
    auto vec_it = vec.begin();    // random_access_iterator_tag
    auto lst_it = lst.begin();    // bidirectional_iterator_tag  
    auto set_it = s.begin();      // bidirectional_iterator_tag
    
    // Algorithms choose implementation based on iterator category
    std::sort(vec.begin(), vec.end());  // Works - random access
    // std::sort(lst.begin(), lst.end());  // Won't compile - bidirectional only
    std::sort(lst.begin(), lst.end(), std::greater<>());  // But this works
}
```

### Checkpoint: Iterator types

1. Identify iterator categories for vector, list, set.
2. Explain why `std::sort` requires random access iterators.
3. Create a simple input iterator for reading from a stream.

## Implementing custom container

Creating STL-compatible containers requires implementing specific type aliases, methods, and iterator support. Let's build a custom vector-like container.

### Basic Structure

```cpp
template <typename T, typename Allocator = std::allocator<T>>
class MyVector {
public:
    // Type aliases (required for STL compatibility)
    using value_type = T;
    using allocator_type = Allocator;
    using size_type = size_t;
    using difference_type = ptrdiff_t;
    using reference = T&;
    using const_reference = const T&;
    using pointer = T*;
    using const_pointer = const T*;
    using iterator = T*;  // Pointer as iterator
    using const_iterator = const T*;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

private:
    T* data_;
    size_type size_;
    size_type capacity_;
    Allocator alloc_;

public:
    // Constructor
    MyVector() : data_(nullptr), size_(0), capacity_(0) {}
    
    explicit MyVector(size_type count, const T& value = T()) 
        : MyVector() {
        resize(count, value);
    }
    
    // Destructor
    ~MyVector() { 
        clear(); 
        alloc_.deallocate(data_, capacity_); 
    }
    
    // Copy constructor
    MyVector(const MyVector& other) : MyVector() {
        reserve(other.size_);
        for (const auto& item : other) {
            push_back(item);
        }
    }
    
    // Move constructor
    MyVector(MyVector&& other) noexcept 
        : data_(other.data_), size_(other.size_), capacity_(other.capacity_), alloc_(std::move(other.alloc_)) {
        other.data_ = nullptr;
        other.size_ = 0;
        other.capacity_ = 0;
    }
    
    // Assignment operators
    MyVector& operator=(const MyVector& other) {
        if (this != &other) {
            MyVector temp(other);
            swap(temp);
        }
        return *this;
    }
    
    MyVector& operator=(MyVector&& other) noexcept {
        if (this != &other) {
            clear();
            alloc_.deallocate(data_, capacity_);
            data_ = other.data_;
            size_ = other.size_;
            capacity_ = other.capacity_;
            alloc_ = std::move(other.alloc_);
            other.data_ = nullptr;
            other.size_ = 0;
            other.capacity_ = 0;
        }
        return *this;
    }
    
    void swap(MyVector& other) noexcept {
        std::swap(data_, other.data_);
        std::swap(size_, other.size_);
        std::swap(capacity_, other.capacity_);
        std::swap(alloc_, other.alloc_);
    }
};
```

### Core Operations

```cpp
// MyVector implementation (continued)
void push_back(const T& value) {
    if (size_ == capacity_) {
        reserve(capacity_ == 0 ? 1 : capacity_ * 2);
    }
    alloc_.construct(data_ + size_, value);
    ++size_;
}

void push_back(T&& value) {
    if (size_ == capacity_) {
        reserve(capacity_ == 0 ? 1 : capacity_ * 2);
    }
    alloc_.construct(data_ + size_, std::move(value));
    ++size_;
}

void reserve(size_type new_capacity) {
    if (new_capacity <= capacity_) return;
    
    T* new_data = alloc_.allocate(new_capacity);
    
    // Move existing elements
    for (size_type i = 0; i < size_; ++i) {
        alloc_.construct(new_data + i, std::move_if_noexcept(data_[i]));
        alloc_.destroy(data_ + i);
    }
    
    alloc_.deallocate(data_, capacity_);
    data_ = new_data;
    capacity_ = new_capacity;
}

void resize(size_type new_size, const T& value = T()) {
    if (new_size < size_) {
        // Shrink
        for (size_type i = new_size; i < size_; ++i) {
            alloc_.destroy(data_ + i);
        }
    } else if (new_size > size_) {
        // Grow
        reserve(new_size);
        for (size_type i = size_; i < new_size; ++i) {
            alloc_.construct(data_ + i, value);
        }
    }
    size_ = new_size;
}

void clear() {
    for (size_type i = 0; i < size_; ++i) {
        alloc_.destroy(data_ + i);
    }
    size_ = 0;
}
```

### Iterator Support

```cpp
// Iterator methods
iterator begin() { return data_; }
iterator end() { return data_ + size_; }
const_iterator begin() const { return data_; }
const_iterator end() const { return data_ + size_; }
const_iterator cbegin() const { return data_; }
const_iterator cend() const { return data_ + size_; }

reverse_iterator rbegin() { return reverse_iterator(end()); }
reverse_iterator rend() { return reverse_iterator(begin()); }
const_reverse_iterator rbegin() const { return const_reverse_iterator(end()); }
const_reverse_iterator rend() const { return const_reverse_iterator(begin()); }
const_reverse_iterator crbegin() const { return const_reverse_iterator(end()); }
const_reverse_iterator crend() const { return const_reverse_iterator(begin()); }

// Element access
reference operator[](size_type index) { return data_[index]; }
const_reference operator[](size_type index) const { return data_[index]; }

reference at(size_type index) { 
    if (index >= size_) throw std::out_of_range("Index out of range");
    return data_[index]; 
}
const_reference at(size_type index) const { 
    if (index >= size_) throw std::out_of_range("Index out of range");
    return data_[index]; 
}

reference front() { return data_[0]; }
const_reference front() const { return data_[0]; }
reference back() { return data_[size_ - 1]; }
const_reference back() const { return data_[size_ - 1]; }

// Capacity
bool empty() const { return size_ == 0; }
size_type size() const { return size_; }
size_type capacity() const { return capacity_; }
size_type max_size() const { return alloc_.max_size(); }
```

### Checkpoint: Custom vector

1. Implement basic `MyVector` with push_back and iterators.
2. Add reserve, resize, and clear methods.
3. Ensure proper copy/move semantics.

## Integrating with STL algorithms

Once your container has proper iterators, it automatically works with most STL algorithms. However, some algorithms have specific requirements.

### Basic Algorithm Usage

```cpp
#include <algorithm>
#include <numeric>

int main() {
    MyVector<int> vec;
    vec.push_back(3);
    vec.push_back(1);
    vec.push_back(4);
    vec.push_back(1);
    vec.push_back(5);
    
    // Non-modifying algorithms (work with any iterators)
    auto it = std::find(vec.begin(), vec.end(), 4);
    if (it != vec.end()) {
        std::cout << "Found 4 at index " << (it - vec.begin()) << std::endl;
    }
    
    int count = std::count(vec.begin(), vec.end(), 1);
    std::cout << "Count of 1s: " << count << std::endl;
    
    // Modifying algorithms (need mutable iterators)
    std::sort(vec.begin(), vec.end());
    std::cout << "Sorted: ";
    for (int x : vec) std::cout << x << " ";
    std::cout << std::endl;
    
    // Numeric algorithms
    int sum = std::accumulate(vec.begin(), vec.end(), 0);
    std::cout << "Sum: " << sum << std::endl;
    
    // Transform with lambda
    MyVector<int> squares;
    squares.resize(vec.size());
    std::transform(vec.begin(), vec.end(), squares.begin(), 
                   [](int x) { return x * x; });
    
    std::cout << "Squares: ";
    for (int x : squares) std::cout << x << " ";
    std::cout << std::endl;
}
```

### Iterator Category Requirements

Different algorithms require different iterator capabilities:

```cpp
// Input iterator requirements (minimal)
std::for_each(vec.begin(), vec.end(), [](int x) { std::cout << x << " "; });
std::count(vec.begin(), vec.end(), 5);

// Forward iterator requirements
std::replace(vec.begin(), vec.end(), 1, 99);

// Bidirectional iterator requirements  
std::reverse(vec.begin(), vec.end());
std::partition(vec.begin(), vec.end(), [](int x) { return x % 2 == 0; });

// Random access iterator requirements
std::sort(vec.begin(), vec.end());
std::nth_element(vec.begin(), vec.begin() + 2, vec.end());
std::binary_search(vec.begin(), vec.end(), 5);
```

### Range-based Algorithms (C++20)

```cpp
#include <ranges>

int main() {
    MyVector<int> vec = {1, 2, 3, 4, 5};
    
    // Range-based operations
    auto even_numbers = vec | std::views::filter([](int x) { return x % 2 == 0; });
    auto doubled = even_numbers | std::views::transform([](int x) { return x * 2; });
    
    std::cout << "Even numbers doubled: ";
    for (int x : doubled) std::cout << x << " ";
    std::cout << std::endl;
}
```

### Custom Algorithms

You can write algorithms that work with any container:

```cpp
template <typename Container, typename Predicate>
void remove_if_custom(Container& c, Predicate pred) {
    auto it = std::remove_if(c.begin(), c.end(), pred);
    c.erase(it, c.end());  // Requires erase method
}

template <typename Container>
void print_container(const Container& c) {
    std::cout << "[";
    if (!c.empty()) {
        auto it = c.begin();
        std::cout << *it;
        ++it;
        for (; it != c.end(); ++it) {
            std::cout << ", " << *it;
        }
    }
    std::cout << "]" << std::endl;
}
```

### Checkpoint: STL integration

1. Use std::sort and std::find on custom container.
2. Implement a generic print function for containers.
3. Test with different STL algorithms.

## Exception safety

Exception safety ensures that containers remain in a valid state even when operations throw exceptions. STL containers provide different levels of exception safety guarantees.

### Exception Safety Levels

1. **No-throw guarantee**: Operation never throws (destructor, swap)
2. **Strong guarantee**: If operation fails, state unchanged (rollback on error)
3. **Basic guarantee**: Valid but unspecified state after exception
4. **No guarantee**: May leave container in invalid state

### Implementing Strong Exception Safety

```cpp
void push_back(const T& value) {
    // Pre-check: ensure we can allocate
    if (size_ == capacity_) {
        size_t new_cap = capacity_ == 0 ? 1 : capacity_ * 2;
        
        // Allocate new memory first
        T* new_data = alloc_.allocate(new_cap);
        size_t constructed = 0;
        
        try {
            // Copy existing elements
            for (size_t i = 0; i < size_; ++i) {
                alloc_.construct(new_data + i, data_[i]);
                ++constructed;
            }
            
            // Add new element
            alloc_.construct(new_data + size_, value);
            ++constructed;
            
        } catch (...) {
            // Rollback: destroy constructed elements
            for (size_t i = 0; i < constructed; ++i) {
                alloc_.destroy(new_data + i);
            }
            alloc_.deallocate(new_data, new_cap);
            throw;  // Re-throw original exception
        }
        
        // Commit: update members
        std::swap(data_, new_data);
        std::swap(capacity_, new_cap);
        alloc_.deallocate(new_data, new_cap - 1);  // Old capacity
        ++size_;
    } else {
        // Simple case: just construct
        alloc_.construct(data_ + size_, value);
        ++size_;
    }
}
```

### Move-Aware Exception Safety

```cpp
void push_back(T&& value) {
    if (size_ == capacity_) {
        size_t new_cap = capacity_ == 0 ? 1 : capacity_ * 2;
        T* new_data = alloc_.allocate(new_cap);
        size_t constructed = 0;
        
        try {
            // Move existing elements (more efficient)
            for (size_t i = 0; i < size_; ++i) {
                alloc_.construct(new_data + i, std::move_if_noexcept(data_[i]));
                ++constructed;
            }
            
            // Move new element
            alloc_.construct(new_data + size_, std::move(value));
            ++constructed;
            
        } catch (...) {
            // Rollback
            for (size_t i = 0; i < constructed; ++i) {
                alloc_.destroy(new_data + i);
            }
            alloc_.deallocate(new_data, new_cap);
            throw;
        }
        
        // Commit
        std::swap(data_, new_data);
        std::swap(capacity_, new_cap);
        alloc_.deallocate(new_data, new_cap - 1);
        ++size_;
    } else {
        alloc_.construct(data_ + size_, std::move(value));
        ++size_;
    }
}
```

### Exception-Safe Operations

```cpp
// No-throw operations
void swap(MyVector& other) noexcept {
    std::swap(data_, other.data_);
    std::swap(size_, other.size_);
    std::swap(capacity_, other.capacity_);
    std::swap(alloc_, other.alloc_);
}

// Strong guarantee operations
void assign(size_type count, const T& value) {
    MyVector temp(count, value);  // May throw
    swap(temp);  // No-throw
}

template <typename InputIt>
void assign(InputIt first, InputIt last) {
    MyVector temp(first, last);  // May throw
    swap(temp);  // No-throw
}
```

### Testing Exception Safety

```cpp
class ThrowOnCopy {
private:
    int value;
    static int copy_count;
public:
    ThrowOnCopy(int v) : value(v) {}
    ThrowOnCopy(const ThrowOnCopy& other) : value(other.value) {
        if (++copy_count == 3) throw std::runtime_error("Copy failed");
    }
};

int main() {
    MyVector<ThrowOnCopy> vec;
    try {
        vec.push_back(ThrowOnCopy(1));
        vec.push_back(ThrowOnCopy(2));
        vec.push_back(ThrowOnCopy(3));  // This should fail
    } catch (const std::exception& e) {
        std::cout << "Exception caught: " << e.what() << std::endl;
        std::cout << "Vector size: " << vec.size() << std::endl;  // Should be 2
    }
}
```

### Checkpoint: Exception safety

1. Make push_back exception-safe with strong guarantee.
2. Implement assign methods with copy-and-swap idiom.
3. Test exception safety with throwing types.

### Checkpoint: Exception Safety Implementation

1. Make push_back exception-safe with strong guarantee.
2. Implement assign methods with copy-and-swap idiom.
3. Test exception safety with throwing types.

## Custom allocators

Control memory allocation.

```cpp
template <typename T>
class LoggingAllocator {
public:
    using value_type = T;
    
    LoggingAllocator() = default;
    
    T* allocate(size_t n) {
        std::cout << "Allocating " << n << " objects" << std::endl;
        return static_cast<T*>(::operator new(n * sizeof(T)));
    }
    
    void deallocate(T* p, size_t n) {
        std::cout << "Deallocating " << n << " objects" << std::endl;
        ::operator delete(p);
    }
    
    // Other required methods...
};

int main() {
    std::vector<int, LoggingAllocator<int>> vec;
    vec.push_back(1); // Logs allocation
}
```

- Custom allocation strategies.

### Checkpoint: Custom allocator

1. Implement simple logging allocator.

## Container adapters

Build on existing containers.

```cpp
template <typename T, typename Container = std::deque<T>>
class MyStack {
private:
    Container c;
public:
    void push(const T& value) { c.push_back(value); }
    void pop() { c.pop_back(); }
    T& top() { return c.back(); }
    bool empty() const { return c.empty(); }
};
```

- Use composition for adapters.

### Checkpoint: Adapter

1. Implement `MyQueue` using std::deque.

## Type traits and SFINAE

Advanced template constraints.

```cpp
#include <type_traits>

template <typename T>
typename std::enable_if<std::is_copy_constructible<T>::value>::type
copy_construct(T* dest, const T& src) {
    new (dest) T(src);
}
```

- Compile-time type checking.

### Checkpoint: Type traits

1. Use std::enable_if for copyable types.

## Mini project: Custom list

Implement doubly-linked list with STL interface.

1. Node structure with prev/next.
2. Iterator class.
3. push_back, erase, begin/end.
4. Test with STL algorithms.

### Success criteria

- STL compatible.
- Correct iteration.
- Memory safe.

## Guided practice challenges

1. Implement a circular buffer container.
2. Create a sparse array with custom iterator.
3. Build a priority queue with custom comparator.
4. Add move semantics to custom container.
5. Implement a memory pool allocator.

## Self-check questions

1. What are iterator categories?
2. How to make container STL-compatible?
3. What is exception safety?
4. How do allocators work?
5. What is SFINAE?

## Recap and next steps

STL is template-based. Next, explore advanced topics.
