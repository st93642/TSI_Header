#!/usr/bin/env ruby
# Generate additional advanced Ruby lessons

require 'fileutils'

lessons_dir = File.expand_path('../learn/curriculum/ruby/lessons', __dir__)

lessons = {
  'iterators.md' => <<~CONTENT,
    # Iterators in Ruby

    Iterators are methods that loop through collections. Ruby provides powerful iterator methods that make working with arrays and hashes elegant and expressive.

    ## The `.each` Iterator

    The most common iterator - processes each element:

    ```ruby
    # Array iteration
    fruits = ["apple", "banana", "orange"]

    fruits.each do |fruit|
      puts fruit
    end

    # Hash iteration
    person = { name: "Alice", age: 30, city: "NYC" }

    person.each do |key, value|
      puts "\#{key}: \#{value}"
    end
    ```

    ## `.each_with_index`

    Iterate with element and index:

    ```ruby
    colors = ["red", "green", "blue"]

    colors.each_with_index do |color, index|
      puts "\#{index}: \#{color}"
    end

    # Output:
    # 0: red
    # 1: green
    # 2: blue
    ```

    ## `.map` (Transform Elements)

    Creates a new array by transforming each element:

    ```ruby
    numbers = [1, 2, 3, 4, 5]

    # Double each number
    doubled = numbers.map { |n| n * 2 }
    # => [2, 4, 6, 8, 10]

    # Convert to strings
    strings = numbers.map { |n| n.to_s }
    # => ["1", "2", "3", "4", "5"]

    # Uppercase names
    names = ["alice", "bob", "charlie"]
    upper = names.map { |name| name.upcase }
    # => ["ALICE", "BOB", "CHARLIE"]
    ```

    ## `.select` (Filter Elements)

    Returns elements that meet a condition:

    ```ruby
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    # Get even numbers
    evens = numbers.select { |n| n.even? }
    # => [2, 4, 6, 8, 10]

    # Get numbers greater than 5
    large = numbers.select { |n| n > 5 }
    # => [6, 7, 8, 9, 10]
    ```

    ## `.reject` (Inverse of Select)

    Returns elements that DON'T meet a condition:

    ```ruby
    numbers = [1, 2, 3, 4, 5, 6]

    # Reject even numbers (keep odds)
    odds = numbers.reject { |n| n.even? }
    # => [1, 3, 5]
    ```

    ## `.reduce` (Accumulate)

    Combines all elements into a single value:

    ```ruby
    numbers = [1, 2, 3, 4, 5]

    # Sum all numbers
    sum = numbers.reduce(0) { |total, n| total + n }
    # => 15

    # Shorter version
    sum = numbers.reduce(:+)
    # => 15

    # Product of all numbers
    product = numbers.reduce(1) { |prod, n| prod * n }
    # => 120
    ```

    ## `.find` (First Match)

    Returns first element matching condition:

    ```ruby
    numbers = [1, 3, 5, 8, 9, 10]

    # Find first even number
    first_even = numbers.find { |n| n.even? }
    # => 8
    ```

    ## `.any?` and `.all?`

    ```ruby
    numbers = [2, 4, 6, 8]

    # Check if any are even
    numbers.any? { |n| n.even? }  # => true

    # Check if all are even
    numbers.all? { |n| n.even? }  # => true

    # Check if all are greater than 5
    numbers.all? { |n| n > 5 }    # => false
    ```

    ## `.times` Iterator

    Repeat code N times:

    ```ruby
    5.times { puts "Hello!" }

    # With index
    3.times do |i|
      puts "Iteration \#{i}"
    end
    ```

    ## Range Iterators

    ```ruby
    # Count up
    1.upto(5) { |i| puts i }

    # Count down
    5.downto(1) { |i| puts i }

    # Step through
    (0..10).step(2) { |i| puts i }  # Even numbers
    ```

    ## Chaining Iterators

    ```ruby
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    # Chain multiple operations
    result = numbers
      .select { |n| n.even? }  # Get evens: [2,4,6,8,10]
      .map { |n| n * 2 }        # Double: [4,8,12,16,20]
      .reduce(:+)               # Sum: 60

    puts result  # => 60
    ```

    ## Key Takeaways

    - `.each` - iterate over all elements
    - `.map` - transform each element, return new array
    - `.select` - filter elements that match condition
    - `.reject` - filter elements that don't match
    - `.reduce` - accumulate into single value
    - `.find` - return first matching element
    - `.any?` / `.all?` - check conditions
    - Iterators can be chained for powerful operations
  CONTENT

  'blocks_procs.md' => <<~CONTENT,
    # Blocks and Procs

    Blocks are chunks of code that can be passed to methods. They are fundamental to Ruby's iterator pattern.

    ## What is a Block?

    A block is code enclosed in `do...end` or curly braces `{}`:

    ```ruby
    # Block with do...end
    [1, 2, 3].each do |num|
      puts num
    end

    # Block with curly braces (single line)
    [1, 2, 3].each { |num| puts num }
    ```

    ## Convention

    - Use `{}` for single-line blocks
    - Use `do...end` for multi-line blocks

    ```ruby
    # Single line - use {}
    numbers.each { |n| puts n }

    # Multiple lines - use do...end
    numbers.each do |n|
      squared = n ** 2
      puts "Square of \#{n} is \#{squared}"
    end
    ```

    ## Block Parameters

    ```ruby
    # Single parameter
    [1, 2, 3].each { |num| puts num }

    # Multiple parameters
    { a: 1, b: 2 }.each { |key, value| puts "\#{key}: \#{value}" }
    ```

    ## yield Keyword

    Methods can execute blocks using `yield`:

    ```ruby
    def greet
      puts "Before block"
      yield
      puts "After block"
    end

    greet do
      puts "Inside block"
    end

    # Output:
    # Before block
    # Inside block
    # After block
    ```

    ## Passing Arguments to Blocks

    ```ruby
    def greet_twice
      yield("Alice")
      yield("Bob")
    end

    greet_twice { |name| puts "Hello, \#{name}!" }

    # Output:
    # Hello, Alice!
    # Hello, Bob!
    ```

    ## block_given?

    Check if a block was provided:

    ```ruby
    def maybe_yield
      if block_given?
        yield
      else
        puts "No block provided"
      end
    end

    maybe_yield                  # => "No block provided"
    maybe_yield { puts "Hi!" }   # => "Hi!"
    ```

    ## What are Procs?

    Procs are objects that hold blocks of code:

    ```ruby
    # Create a Proc
    my_proc = Proc.new { |name| puts "Hello, \#{name}!" }

    # Call the Proc
    my_proc.call("Alice")   # => "Hello, Alice!"

    # Alternative syntax
    my_proc.("Bob")         # => "Hello, Bob!"
    my_proc["Charlie"]      # => "Hello, Charlie!"
    ```

    ## Procs as Method Parameters

    ```ruby
    def execute_proc(my_proc)
      my_proc.call
    end

    greeting = Proc.new { puts "Hello!" }
    execute_proc(greeting)  # => "Hello!"
    ```

    ## Converting Blocks to Procs

    Use `&` to convert block to Proc:

    ```ruby
    def run_block(&block)
      block.call
    end

    run_block { puts "Hi!" }  # => "Hi!"
    ```

    ## Practical Example

    ```ruby
    # Timer method that times block execution
    def benchmark
      start_time = Time.now
      yield
      end_time = Time.now
      puts "Time taken: \#{end_time - start_time} seconds"
    end

    benchmark do
      sum = 0
      1_000_000.times { |i| sum += i }
      puts "Sum: \#{sum}"
    end
    ```

    ## Key Takeaways

    - Blocks are code chunks: `{}` or `do...end`
    - Use `{}` for one line, `do...end` for multiple
    - Methods execute blocks with `yield`
    - Check for blocks with `block_given?`
    - Procs are objects containing code blocks
    - Create with `Proc.new { code }`
    - Call Procs with `.call()`
    - Convert blocks to Procs with `&block`
  CONTENT

  'mixins.md' => <<~CONTENT,
    # Mixins and Modules

    Modules are Ruby's way of grouping methods, classes, and constants. They enable code reuse across multiple classes through mixins.

    ## What is a Module?

    A module is a collection of methods and constants:

    ```ruby
    module Skill
      def average_speed
        puts "My average speed is 20mph"
      end
    end
    ```

    ## Including Modules (Mixins)

    Use `include` to add module methods to a class:

    ```ruby
    module Skill
      def average_speed
        "20 mph"
      end

      def max_speed
        "25 mph"
      end
    end

    class Runner
      include Skill

      def initialize(name)
        @name = name
      end
    end

    runner = Runner.new("Alice")
    puts runner.average_speed  # => "20 mph"
    puts runner.max_speed      # => "25 mph"
    ```

    ## Multiple Mixins

    A class can include multiple modules:

    ```ruby
    module Swimming
      def swim
        "Swimming..."
      end
    end

    module Running
      def run
        "Running..."
      end
    end

    class Triathlete
      include Swimming
      include Running

      def compete
        "Competing in triathlon"
      end
    end

    athlete = Triathlete.new
    puts athlete.swim       # => "Swimming..."
    puts athlete.run        # => "Running..."
    puts athlete.compete    # => "Competing in triathlon"
    ```

    ## Namespacing with Modules

    Modules can group related classes:

    ```ruby
    module Transportation
      class Car
        def drive
          "Driving car"
        end
      end

      class Bike
        def ride
          "Riding bike"
        end
      end
    end

    # Access with ::
    car = Transportation::Car.new
    bike = Transportation::Bike.new

    puts car.drive   # => "Driving car"
    puts bike.ride   # => "Riding bike"
    ```

    ## Module Constants

    ```ruby
    module Math
      PI = 3.14159
      E = 2.71828

      def self.circle_area(radius)
        PI * radius ** 2
      end
    end

    puts Math::PI  # => 3.14159
    puts Math.circle_area(5)  # => 78.53975
    ```

    ## Differences: Module vs Class

    | Module | Class |
    |--------|-------|
    | Cannot be instantiated | Can create objects with `.new` |
    | Can be mixed into classes | Can inherit from one parent |
    | Multiple modules per class | Single inheritance only |
    | Used for code reuse | Used for object creation |

    ## Extend vs Include

    ```ruby
    module Greetings
      def hello
        "Hello!"
      end
    end

    # include - adds instance methods
    class Person
      include Greetings
    end

    person = Person.new
    person.hello  # => "Hello!"

    # extend - adds class methods
    class Company
      extend Greetings
    end

    Company.hello  # => "Hello!"
    ```

    ## Real-World Example

    ```ruby
    module Timestampable
      def created_at
        @created_at ||= Time.now
      end

      def updated_at
        @updated_at ||= Time.now
      end
    end

    class BlogPost
      include Timestampable
      attr_accessor :title, :content
    end

    class Comment
      include Timestampable
      attr_accessor :text, :author
    end

    post = BlogPost.new
    puts post.created_at  # => 2025-10-01 ...
    ```

    ## The Math Module

    Ruby's built-in Math module:

    ```ruby
    Math::PI              # => 3.141592653589793
    Math::E               # => 2.718281828459045

    Math.sqrt(16)         # => 4.0
    Math.sin(0)           # => 0.0
    Math.cos(0)           # => 1.0
    Math.log(10)          # => 2.302585092994046
    ```

    ## Key Takeaways

    - Modules group related methods/constants
    - Cannot create instances: no `.new`
    - Use `include` to add instance methods
    - Use `extend` to add class methods
    - Classes can include multiple modules
    - Modules enable code reuse (mixins)
    - Use `::` to access module contents
    - Modules provide namespacing
  CONTENT

  'file_io.md' => <<~CONTENT,
    # File Input/Output

    Ruby makes it easy to read from and write to files. File I/O is essential for saving data, loading configuration, and processing text files.

    ## Reading Files

    ### Read Entire File

    ```ruby
    # Read entire file into string
    content = File.read("example.txt")
    puts content

    # Read into array of lines
    lines = File.readlines("example.txt")
    lines.each { |line| puts line }
    ```

    ### Read Line by Line

    ```ruby
    File.open("example.txt", "r") do |file|
      file.each_line do |line|
        puts line
      end
    end
    ```

    ## Writing Files

    ### Write (Overwrite)

    ```ruby
    # Write string to file (overwrites existing)
    File.write("output.txt", "Hello, World!")

    # Write multiple lines
    content = "Line 1\nLine 2\nLine 3"
    File.write("output.txt", content)
    ```

    ### Append to File

    ```ruby
    # Append without overwriting
    File.open("log.txt", "a") do |file|
      file.puts "New log entry"
      file.puts "Another entry"
    end
    ```

    ## File Modes

    Common file open modes:

    - `"r"` - Read only (default)
    - `"w"` - Write only (overwrites)
    - `"a"` - Append only
    - `"r+"` - Read and write
    - `"w+"` - Read and write (overwrites)
    - `"a+"` - Read and append

    ```ruby
    # Read and write
    File.open("data.txt", "r+") do |file|
      content = file.read
      file.write("\nNew line")
    end
    ```

    ## File Existence

    ```ruby
    if File.exist?("config.txt")
      puts "File exists!"
    else
      puts "File not found"
    end

    # Alternative
    File.file?("config.txt")  # true if regular file
    File.directory?("data")   # true if directory
    ```

    ## File Information

    ```ruby
    file_path = "example.txt"

    File.size(file_path)        # Size in bytes
    File.basename(file_path)    # => "example.txt"
    File.dirname(file_path)     # Directory path
    File.extname(file_path)     # => ".txt"

    # Modification time
    File.mtime(file_path)       # => Time object
    ```

    ## Safe File Handling

    Using blocks ensures files are closed automatically:

    ```ruby
    # File closes automatically after block
    File.open("data.txt", "r") do |file|
      content = file.read
      # Process content
    end
    # File is closed here

    # Manual close (not recommended)
    file = File.open("data.txt", "r")
    content = file.read
    file.close  # Don't forget!
    ```

    ## CSV Files

    ```ruby
    require 'csv'

    # Write CSV
    CSV.open("users.csv", "w") do |csv|
      csv << ["Name", "Age", "City"]
      csv << ["Alice", 30, "NYC"]
      csv << ["Bob", 25, "LA"]
    end

    # Read CSV
    CSV.foreach("users.csv", headers: true) do |row|
      puts "Name: \#{row['Name']}, Age: \#{row['Age']}"
    end
    ```

    ## JSON Files

    ```ruby
    require 'json'

    # Write JSON
    data = { name: "Alice", age: 30, city: "NYC" }
    File.write("user.json", JSON.pretty_generate(data))

    # Read JSON
    json_string = File.read("user.json")
    user = JSON.parse(json_string)
    puts user["name"]  # => "Alice"
    ```

    ## Practical Examples

    ### Count lines in file

    ```ruby
    line_count = File.readlines("example.txt").size
    puts "File has \#{line_count} lines"
    ```

    ### Find and replace in file

    ```ruby
    content = File.read("config.txt")
    updated = content.gsub("old_value", "new_value")
    File.write("config.txt", updated)
    ```

    ### Log to file

    ```ruby
    def log_message(message)
      timestamp = Time.now.strftime("%Y-%m-%d %H:%M:%S")
      File.open("app.log", "a") do |file|
        file.puts "[\#{timestamp}] \#{message}"
      end
    end

    log_message("Application started")
    log_message("User logged in")
    ```

    ## Key Takeaways

    - `File.read()` - read entire file
    - `File.readlines()` - read as array of lines
    - `File.write()` - write (overwrites)
    - `File.open() { |f| }` - safe file handling
    - Use `"r"` for read, `"w"` for write, `"a"` for append
    - `File.exist?()` - check if file exists
    - Blocks auto-close files
    - Use CSV and JSON libraries for structured data
  CONTENT

  'error_handling.md' => <<~CONTENT,
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
      puts "Error: \#{e.message}"
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
      puts "Something else went wrong: \#{e.message}"
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
      puts "Error: \#{e.message}"
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
      puts "Success! Result: \#{result}"
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
        puts "Retrying... (attempt \#{attempts})"
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
          raise InsufficientFundsError, "Tried to withdraw \#{amount}, but only have \#{@balance}"
        end
        @balance -= amount
      end
    end

    # Usage
    account = BankAccount.new(100)
    begin
      account.withdraw(150)
    rescue InsufficientFundsError => e
      puts "Error: \#{e.message}"
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
      puts "Error reading config: \#{e.message}"
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
  CONTENT
}

# Write each lesson
lessons.each do |filename, content|
  filepath = File.join(lessons_dir, filename)
  File.write(filepath, content)
  puts "✓ Created #{filename}"
end

puts "\n✓ All advanced lessons generated successfully!"
puts "Total lessons: #{lessons.size}"
