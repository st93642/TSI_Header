# Attribute Methods Exercise
# Practice with attr_reader, attr_writer, and attr_accessor

# Instructions:
# Ruby provides convenient methods to automatically create getters and setters
# for instance variables, promoting encapsulation and clean code.

# Your task is to implement the classes that the tests expect.
# Look at the test failures to understand what each class should do.

# 1. Basic Attribute Methods
class Person
  # Use attr_reader for read-only name
  # Use attr_accessor for read-write age
  # Use attr_writer for write-only password
  
  def initialize(name, age)
    # Set name and age
  end
  
  def adult?
    # Return true if age >= 18
  end
end

# 2. Custom Accessors with Validation
class Product
  # Use attr_reader for name and price
  
  def initialize(name, price)
    # Set name and validate/set price
  end
  
  def price=(new_price)
    # Custom setter: validate price > 0
  end
  
  def formatted_price
    # Return price formatted as "$X.XX"
  end
end

# 3. Private Attributes
class BankAccount
  # Use attr_reader for account_number (public)
  # Balance should be private with custom methods
  
  def initialize(account_number, initial_balance = 0)
    # Set account number and balance
  end
  
  def deposit(amount)
    # Add amount to balance, validate amount > 0
  end
  
  def withdraw(amount)
    # Subtract amount from balance, validate sufficient funds
  end
  
  def balance
    # Return current balance (read-only access)
  end
  
  private
  
  def sufficient_funds?(amount)
    # Check if balance >= amount
  end
end

# 4. Class with Multiple Attribute Types
class Student
  # Use appropriate attr_ methods for:
  # - name (read-only after creation)
  # - grade (read-write with validation)
  # - student_id (read-only)
  # - email (read-write)
  
  def initialize(name, student_id)
    # Set name and student_id
  end
  
  def grade=(new_grade)
    # Validate grade is A, B, C, D, or F
  end
  
  def email=(new_email)
    # Validate email contains @ symbol
  end
  
  def info
    # Return formatted string with student information
  end
end

# 5. Advanced Attribute Patterns
class Configuration
  # Use attr_accessor for multiple config options
  
  def initialize
    # Set default values
  end
  
  def self.load_from_hash(config_hash)
    # Create instance and set attributes from hash
  end
  
  def to_hash
    # Return hash of all configuration values
  end
  
  def update(options = {})
    # Update multiple attributes from hash
  end
end

# Example usage (uncomment these to test your code):
# person = Person.new("Alice", 25)
# puts person.name                     # "Alice"
# puts person.age                      # 25
# person.age = 26
# puts person.adult?                   # true

# product = Product.new("Widget", 19.99)
# puts product.name                    # "Widget"
# puts product.formatted_price         # "$19.99"

# account = BankAccount.new("12345", 100)
# account.deposit(50)
# puts account.balance                 # 150
# account.withdraw(25)
# puts account.balance                 # 125

# student = Student.new("Bob", "S001")
# student.grade = "A"
# student.email = "bob@example.com"
# puts student.info

# config = Configuration.new
# config.update(debug: true, port: 3000)
# puts config.to_hash