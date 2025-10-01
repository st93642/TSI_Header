#!/usr/bin/env ruby
# Generate complete lesson content from FreeCodeCamp Ruby tutorial

require 'fileutils'

lessons_dir = File.expand_path('../learn/curriculum/ruby/lessons', __dir__)

# Lesson content adapted from FreeCodeCamp "Learning Ruby: From Zero to Hero"
lessons = {
  'strings.md' => <<~CONTENT,
    # Strings in Ruby

    Strings are one of the most fundamental data types in Ruby. They represent sequences of characters and are used extensively in every Ruby program.

    ## What is a String?

    A string is a sequence of characters enclosed in either single quotes (`'`) or double quotes (`"`). Strings can contain letters, numbers, symbols, and whitespace.

    ```ruby
    # Single quotes
    name = 'Alice'

    # Double quotes
    greeting = "Hello, World!"

    # Strings with numbers and symbols
    code = "Ruby 3.0 is amazing!"
    ```

    ## Single vs Double Quotes

    The main difference between single and double quotes is that double quotes allow:
    - **String interpolation** (embedding variables and expressions)
    - **Escape sequences** (like `\\n` for newline, `\\t` for tab)

    ```ruby
    name = "Bob"

    # String interpolation (only works with double quotes)
    message = "Hello, \#{name}!"  # => "Hello, Bob!"

    # Escape sequences
    multi_line = "Line 1\\nLine 2\\nLine 3"
    ```

    Single quotes treat everything literally:

    ```ruby
    name = "Alice"
    message = 'Hello, \#{name}!'  # => "Hello, \#{name}!" (literal)
    ```

    ## Common String Methods

    ### Changing Case
    ```ruby
    text = "Ruby Programming"
    text.upcase      # => "RUBY PROGRAMMING"
    text.downcase    # => "ruby programming"
    text.capitalize  # => "Ruby programming"
    ```

    ### String Concatenation
    ```ruby
    first_name = "John"
    last_name = "Doe"
    full_name = first_name + " " + last_name  # => "John Doe"
    ```

    ### Accessing Characters
    ```ruby
    word = "Ruby"
    word[0]      # => "R"
    word[-1]     # => "y"
    word[0..2]   # => "Rub"
    ```

    ## Key Takeaways

    - Strings can use single (`'`) or double (`"`) quotes
    - Double quotes enable interpolation with `\#{}`
    - Ruby has extensive built-in string methods
    - Use `.upcase`, `.downcase`, `.capitalize` for case changes
    - String interpolation is cleaner than concatenation
  CONTENT

  'numbers.md' => <<~CONTENT,
    # Numbers and Math in Ruby

    Ruby provides powerful support for numeric operations with integers, floating-point numbers, and mathematical functions.

    ## Numeric Types

    ### Integers
    Whole numbers without decimal points:

    ```ruby
    age = 25
    big_number = 1_000_000  # Underscores for readability
    ```

    ### Floats
    Numbers with decimal points:

    ```ruby
    price = 19.99
    pi = 3.14159
    ```

    ## Basic Arithmetic

    ```ruby
    10 + 5   # => 15 (addition)
    10 - 5   # => 5  (subtraction)
    10 * 5   # => 50 (multiplication)
    10 / 5   # => 2  (division)
    10 % 3   # => 1  (modulo/remainder)
    2 ** 3   # => 8  (exponentiation)
    ```

    ## Division Gotcha

    Integer division truncates decimals:

    ```ruby
    10 / 3   # => 3 (integer division)
    10.0 / 3 # => 3.333... (float division)
    10.to_f / 3  # => 3.333... (convert to float first)
    ```

    ## Type Conversion

    ```ruby
    42.to_f        # => 42.0 (integer to float)
    3.99.to_i      # => 3 (float to integer)
    "42".to_i      # => 42 (string to integer)
    "3.14".to_f    # => 3.14 (string to float)
    ```

    ## Numeric Methods

    ```ruby
    -5.abs         # => 5 (absolute value)
    3.7.round      # => 4 (round to nearest)
    4.even?        # => true
    5.odd?         # => true
    ```

    ## Math Module

    ```ruby
    Math::PI       # => 3.14159...
    Math.sqrt(16)  # => 4.0
    Math.sqrt(2)   # => 1.414...
    ```

    ## Key Takeaways

    - Ruby has Integer and Float numeric types
    - Integer division truncates (use `.to_f` for decimals)
    - Numbers are objects with methods like `.even?`, `.abs`
    - Math module provides advanced functions
    - Use underscores in large numbers: `1_000_000`
  CONTENT

  'conditionals.md' => <<~CONTENT,
    # Conditionals and Control Flow

    Control flow statements allow your program to make decisions based on conditions. Ruby provides elegant syntax for conditional logic.

    ## The `if` Statement

    ```ruby
    age = 18
    if age >= 18
      puts "You are an adult"
    end
    ```

    ## `if-else` Statement

    ```ruby
    temperature = 25

    if temperature > 30
      puts "It's hot!"
    else
      puts "Weather is pleasant"
    end
    ```

    ## `if-elsif-else` Statement

    ```ruby
    score = 85

    if score >= 90
      puts "Grade: A"
    elsif score >= 80
      puts "Grade: B"
    elsif score >= 70
      puts "Grade: C"
    else
      puts "Grade: F"
    end
    ```

    ## Inline `if` Modifier

    Ruby allows concise one-line conditions:

    ```ruby
    puts "You can vote!" if age >= 18
    send_email if user_subscribed
    ```

    ## Comparison Operators

    ```ruby
    5 == 5      # => true  (equal to)
    5 != 3      # => true  (not equal to)
    5 > 3       # => true  (greater than)
    5 < 3       # => false (less than)
    5 >= 5      # => true  (greater or equal)
    5 <= 3      # => false (less or equal)
    ```

    ## Logical Operators

    ```ruby
    # AND - both must be true
    if age >= 18 && has_license
      puts "You can drive"
    end

    # OR - at least one must be true
    if is_weekend || is_holiday
      puts "No work today!"
    end

    # NOT - negates condition
    if !raining
      puts "Let's go outside"
    end
    ```

    ## Truthiness in Ruby

    Only `false` and `nil` are falsy - everything else is truthy:

    ```ruby
    if 0
      puts "0 is truthy!"  # This executes
    end

    if ""
      puts "Empty string is truthy!"  # This executes
    end
    ```

    ## Ternary Operator

    ```ruby
    age = 20
    status = age >= 18 ? "Adult" : "Minor"
    ```

    ## Key Takeaways

    - `if`, `elsif`, `else` control program flow
    - Inline modifiers: `puts "Hi" if condition`
    - Only `false` and `nil` are falsy
    - Ternary: `condition ? true_value : false_value`
    - Logical operators: `&&` (AND), `||` (OR), `!` (NOT)
    - Comparison: `==`, `!=`, `>`, `<`, `>=`, `<=`
  CONTENT

  'arrays.md' => <<~CONTENT,
    # Arrays: Collections in Ruby

    Arrays are ordered collections that can store multiple values. They are one of the most commonly used data structures in Ruby.

    ## Creating Arrays

    ```ruby
    # Empty array
    my_array = []

    # Array with integers
    numbers = [1, 2, 3, 4, 5]

    # Array with strings
    names = ["Alice", "Bob", "Charlie"]

    # Mixed types (Ruby allows this)
    mixed = [1, "hello", 3.14, true]
    ```

    ## Accessing Elements

    Arrays use zero-based indexing:

    ```ruby
    numbers = [5, 7, 1, 3, 4]

    numbers[0]   # => 5 (first element)
    numbers[1]   # => 7 (second element)
    numbers[-1]  # => 4 (last element)
    numbers[-2]  # => 3 (second to last)
    ```

    ## Adding Elements

    ```ruby
    books = []

    # Using push
    books.push("The Effective Engineer")
    books.push("Zero to One")

    # Using << operator
    books << "Lean Startup"
    books << "Hooked"

    puts books
    # => ["The Effective Engineer", "Zero to One", "Lean Startup", "Hooked"]
    ```

    ## Common Array Methods

    ### Size and Length
    ```ruby
    numbers = [1, 2, 3, 4, 5]
    numbers.length  # => 5
    numbers.size    # => 5 (same as length)
    numbers.empty?  # => false
    ```

    ### First and Last
    ```ruby
    numbers.first   # => 1
    numbers.last    # => 5
    ```

    ### Include?
    ```ruby
    numbers.include?(3)  # => true
    numbers.include?(10) # => false
    ```

    ### Removing Elements
    ```ruby
    numbers = [1, 2, 3, 4, 5]

    numbers.pop    # => 5 (removes and returns last)
    numbers.shift  # => 1 (removes and returns first)
    numbers.delete(3)  # Removes element with value 3
    ```

    ## Iterating Over Arrays

    ```ruby
    fruits = ["apple", "banana", "orange"]

    # Each iterator
    fruits.each do |fruit|
      puts fruit
    end

    # With index
    fruits.each_with_index do |fruit, index|
      puts "\#{index}: \#{fruit}"
    end
    ```

    ## Array Operations

    ```ruby
    arr1 = [1, 2, 3]
    arr2 = [3, 4, 5]

    arr1 + arr2    # => [1, 2, 3, 3, 4, 5] (concatenation)
    arr1 - arr2    # => [1, 2] (difference)
    arr1 & arr2    # => [3] (intersection)
    arr1 | arr2    # => [1, 2, 3, 4, 5] (union)
    ```

    ## Useful Methods

    ```ruby
    numbers = [5, 2, 8, 1, 9]

    numbers.sort      # => [1, 2, 5, 8, 9]
    numbers.reverse   # => [9, 1, 8, 2, 5]
    numbers.uniq      # Removes duplicates
    numbers.max       # => 9
    numbers.min       # => 1
    numbers.sum       # => 25
    ```

    ## Key Takeaways

    - Arrays store ordered collections: `[1, 2, 3]`
    - Zero-based indexing: first element is `[0]`
    - Negative indices count from end: `[-1]` is last
    - Add with `.push()` or `<<` operator
    - Remove with `.pop`, `.shift`, `.delete`
    - Iterate with `.each` method
    - Many built-in methods: `.sort`, `.reverse`, `.uniq`
  CONTENT

  'hashes.md' => <<~CONTENT,
    # Hashes: Key-Value Collections

    Hashes are collections of key-value pairs, similar to dictionaries in other languages. They provide fast lookup by key.

    ## Creating Hashes

    ```ruby
    # Empty hash
    person = {}

    # Hash with string keys
    person = {
      "name" => "Leandro",
      "nickname" => "TK",
      "nationality" => "Brazilian"
    }

    # Modern symbol syntax (preferred)
    person = {
      name: "Leandro",
      nickname: "TK",
      nationality: "Brazilian",
      age: 24
    }
    ```

    ## Accessing Values

    ```ruby
    person = {
      name: "Alice",
      age: 30,
      city: "New York"
    }

    person[:name]   # => "Alice"
    person[:age]    # => 30
    person[:city]   # => "New York"
    ```

    ## Adding and Updating

    ```ruby
    person = { name: "Bob" }

    # Add new key-value
    person[:age] = 25
    person[:city] = "Boston"

    # Update existing value
    person[:age] = 26

    puts person
    # => {:name=>"Bob", :age=>26, :city=>"Boston"}
    ```

    ## Hash Methods

    ```ruby
    person = { name: "Alice", age: 30, city: "NYC" }

    person.keys     # => [:name, :age, :city]
    person.values   # => ["Alice", 30, "NYC"]
    person.length   # => 3
    person.empty?   # => false
    person.has_key?(:name)  # => true
    ```

    ## Iterating Over Hashes

    ```ruby
    person = {
      name: "Alice",
      age: 30,
      city: "NYC"
    }

    # Iterate with key and value
    person.each do |key, value|
      puts "\#{key}: \#{value}"
    end

    # Output:
    # name: Alice
    # age: 30
    # city: NYC
    ```

    ## Symbols vs Strings as Keys

    **Symbols** (`:name`) are preferred over strings (`"name"`) as keys because:
    - More memory efficient
    - Faster lookup
    - Conventional in Ruby

    ```ruby
    # Using symbols (preferred)
    user = { name: "Alice", age: 30 }

    # Using strings (less common)
    user = { "name" => "Alice", "age" => 30 }
    ```

    ## Default Values

    ```ruby
    # Returns nil for missing keys
    hash = {}
    hash[:missing]  # => nil

    # Set default value
    hash = Hash.new(0)
    hash[:missing]  # => 0
    ```

    ## Key Takeaways

    - Hashes store key-value pairs: `{ key: value }`
    - Use symbols as keys: `:name`, `:age`
    - Access with square brackets: `hash[:key]`
    - Add/update: `hash[:key] = value`
    - Iterate with `.each do |key, value|`
    - Methods: `.keys`, `.values`, `.has_key?`
  CONTENT

  'defining_methods.md' => <<~CONTENT,
    # Defining Methods in Ruby

    Methods are reusable blocks of code that perform specific tasks. They help organize your code and avoid repetition.

    ## Basic Method Definition

    ```ruby
    # Simple method
    def greet
      puts "Hello, World!"
    end

    # Call the method
    greet  # => Hello, World!
    ```

    ## Methods with Parameters

    ```ruby
    def greet(name)
      puts "Hello, \#{name}!"
    end

    greet("Alice")  # => Hello, Alice!
    greet("Bob")    # => Hello, Bob!
    ```

    ## Methods with Multiple Parameters

    ```ruby
    def add(a, b)
      a + b
    end

    result = add(5, 3)
    puts result  # => 8
    ```

    ## Return Values

    Ruby methods automatically return the last evaluated expression:

    ```ruby
    def multiply(a, b)
      a * b
    end

    result = multiply(4, 5)
    puts result  # => 20

    # Explicit return (optional)
    def divide(a, b)
      return 0 if b == 0  # Guard clause
      a / b
    end
    ```

    ## Default Parameters

    ```ruby
    def greet(name = "Guest")
      "Hello, \#{name}!"
    end

    greet         # => "Hello, Guest!"
    greet("Alice") # => "Hello, Alice!"
    ```

    ## Methods with Multiple Return Values

    ```ruby
    def min_max(array)
      [array.min, array.max]
    end

    numbers = [5, 2, 8, 1, 9]
    minimum, maximum = min_max(numbers)
    puts "Min: \#{minimum}, Max: \#{maximum}"
    # => Min: 1, Max: 9
    ```

    ## Method Naming Conventions

    ```ruby
    # Use snake_case
    def calculate_average(numbers)
      numbers.sum.to_f / numbers.size
    end

    # Question mark for boolean methods
    def even?(number)
      number % 2 == 0
    end

    # Exclamation mark for dangerous methods
    def reverse!
      # Modifies object in place
    end
    ```

    ## Variable Arguments

    ```ruby
    def sum(*numbers)
      numbers.sum
    end

    sum(1, 2, 3)        # => 6
    sum(1, 2, 3, 4, 5)  # => 15
    ```

    ## Keyword Arguments

    ```ruby
    def create_user(name:, age:, city: "Unknown")
      {
        name: name,
        age: age,
        city: city
      }
    end

    user = create_user(name: "Alice", age: 30)
    # => {:name=>"Alice", :age=>30, :city=>"Unknown"}
    ```

    ## Key Takeaways

    - Define methods with `def method_name`
    - Methods automatically return last expression
    - Use parameters: `def greet(name)`
    - Default parameters: `def greet(name = "Guest")`
    - Use snake_case for method names
    - `?` suffix for boolean methods
    - `*args` for variable number of arguments
    - Keyword arguments: `def method(key: value)`
  CONTENT

  'classes_objects.md' => <<~CONTENT,
    # Classes and Objects

    Ruby is an object-oriented language. Everything in Ruby is an object, and classes are blueprints for creating objects.

    ## Defining a Class

    ```ruby
    class Vehicle
      # Class code goes here
    end

    # Create an instance (object)
    car = Vehicle.new
    ```

    ## Initialize Method (Constructor)

    ```ruby
    class Vehicle
      def initialize(wheels, tank_type, capacity, max_speed)
        @wheels = wheels
        @tank_type = tank_type
        @capacity = capacity
        @max_speed = max_speed
      end
    end

    tesla = Vehicle.new(4, 'electric', 5, 250)
    ```

    ## Instance Variables

    Instance variables (prefixed with `@`) store object state:

    ```ruby
    class Person
      def initialize(name, age)
        @name = name  # Instance variable
        @age = age    # Instance variable
      end
    end
    ```

    ## Instance Methods

    ```ruby
    class Vehicle
      def initialize(max_speed)
        @max_speed = max_speed
      end

      def make_noise
        "VRRRRUUUUM"
      end

      def accelerate
        puts "Accelerating to \#{@max_speed} km/h!"
      end
    end

    car = Vehicle.new(180)
    car.make_noise     # => "VRRRRUUUUM"
    car.accelerate     # => Accelerating to 180 km/h!
    ```

    ## Getter and Setter Methods

    ```ruby
    class Person
      def initialize(name)
        @name = name
      end

      # Getter method
      def name
        @name
      end

      # Setter method
      def name=(new_name)
        @name = new_name
      end
    end

    person = Person.new("Alice")
    puts person.name      # => Alice
    person.name = "Bob"
    puts person.name      # => Bob
    ```

    ## attr_accessor, attr_reader, attr_writer

    Ruby provides shortcuts for getter/setter methods:

    ```ruby
    class Person
      attr_reader :name    # Only getter
      attr_writer :email   # Only setter
      attr_accessor :age   # Both getter and setter

      def initialize(name, age, email)
        @name = name
        @age = age
        @email = email
      end
    end

    person = Person.new("Alice", 30, "alice@example.com")
    person.name           # Works (reader)
    # person.name = "Bob" # Error (no writer)

    person.age            # Works (accessor)
    person.age = 31       # Works (accessor)
    ```

    ## Class Methods

    ```ruby
    class MathHelper
      def self.square(number)
        number * number
      end
    end

    result = MathHelper.square(5)
    puts result  # => 25
    ```

    ## to_s Method

    Customize how objects are printed:

    ```ruby
    class Person
      attr_accessor :name, :age

      def initialize(name, age)
        @name = name
        @age = age
      end

      def to_s
        "\#{@name}, \#{@age} years old"
      end
    end

    person = Person.new("Alice", 30)
    puts person  # => Alice, 30 years old
    ```

    ## Key Takeaways

    - Classes are blueprints: `class ClassName`
    - Create objects with `.new`
    - `initialize` method is the constructor
    - Instance variables start with `@`
    - Instance methods define object behavior
    - `attr_reader` - getter only
    - `attr_writer` - setter only
    - `attr_accessor` - both getter and setter
    - Class methods use `self.method_name`
  CONTENT

  'inheritance.md' => <<~CONTENT,
    # Inheritance in Ruby

    Inheritance allows classes to inherit characteristics and behavior from parent classes, promoting code reuse.

    ## Basic Inheritance

    ```ruby
    class Car
      attr_accessor :wheels, :capacity, :max_speed

      def initialize(wheels, capacity, max_speed)
        @wheels = wheels
        @capacity = capacity
        @max_speed = max_speed
      end
    end

    # ElectricCar inherits from Car
    class ElectricCar < Car
    end

    tesla = ElectricCar.new(4, 5, 250)
    puts tesla.wheels      # => 4
    puts tesla.max_speed   # => 250
    ```

    ## The `<` Operator

    The `<` operator indicates inheritance:

    ```ruby
    class Parent
      def greet
        "Hello from parent"
      end
    end

    class Child < Parent
      # Inherits greet method
    end

    child = Child.new
    puts child.greet  # => "Hello from parent"
    ```

    ## Overriding Methods

    Child classes can override parent methods:

    ```ruby
    class Animal
      def speak
        "Some sound"
      end
    end

    class Dog < Animal
      def speak
        "Woof!"
      end
    end

    class Cat < Animal
      def speak
        "Meow!"
      end
    end

    dog = Dog.new
    cat = Cat.new
    puts dog.speak  # => "Woof!"
    puts cat.speak  # => "Meow!"
    ```

    ## Using `super`

    Call the parent class's version of a method:

    ```ruby
    class Vehicle
      def initialize(wheels)
        @wheels = wheels
      end

      def info
        "This vehicle has \#{@wheels} wheels"
      end
    end

    class Car < Vehicle
      def initialize(wheels, brand)
        super(wheels)  # Call parent initialize
        @brand = brand
      end

      def info
        super + " and is a \#{@brand}"
      end
    end

    car = Car.new(4, "Tesla")
    puts car.info
    # => "This vehicle has 4 wheels and is a Tesla"
    ```

    ## Inheritance Chain

    ```ruby
    class GrandParent
      def method1
        "GrandParent method"
      end
    end

    class Parent < GrandParent
      def method2
        "Parent method"
      end
    end

    class Child < Parent
      def method3
        "Child method"
      end
    end

    child = Child.new
    child.method1  # From GrandParent
    child.method2  # From Parent
    child.method3  # From Child
    ```

    ## Checking Class Relationships

    ```ruby
    dog = Dog.new

    dog.instance_of?(Dog)     # => true
    dog.instance_of?(Animal)  # => false

    dog.is_a?(Dog)            # => true
    dog.is_a?(Animal)         # => true
    dog.kind_of?(Animal)      # => true (same as is_a?)
    ```

    ## Key Takeaways

    - Inheritance uses `<` operator: `class Child < Parent`
    - Child classes inherit parent methods and attributes
    - Override methods by redefining them in child class
    - Use `super` to call parent class methods
    - Ruby supports single inheritance only
    - Check relationships with `.is_a?` and `.instance_of?`
    - Inheritance promotes code reuse and organization
  CONTENT
}

# Write each lesson
lessons.each do |filename, content|
  filepath = File.join(lessons_dir, filename)
  File.write(filepath, content)
  puts "✓ Created #{filename}"
end

puts "\n✓ All FreeCodeCamp lessons generated successfully!"
puts "Total lessons: #{lessons.size}"
