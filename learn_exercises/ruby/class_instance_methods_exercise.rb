# Class vs Instance Methods Exercise
# Understand the difference between class and instance methods

# Instructions:
# Class methods belong to the class itself and are called on the class.
# Instance methods belong to instances and are called on objects.

# Your task is to implement the classes that the tests expect.
# Look at the test failures to understand what each method should do.

# 1. Basic Class vs Instance Methods
class Counter
  # Class variable to track total instances
  
  def initialize
    # Initialize instance counter
    # Increment class counter
  end
  
  def increment
    # Increment instance counter
  end
  
  def count
    # Return instance counter value
  end
  
  def self.total_instances
    # Return total number of Counter instances created
  end
  
  def self.reset_total
    # Reset total instances counter to 0
  end
end

# 2. Factory Pattern with Class Methods
class User
  def initialize(name, email)
    # Set name and email
  end
  
  def self.create_admin(name, email)
    # Create user with admin privileges
  end
  
  def self.create_guest
    # Create guest user with default values
  end
  
  def self.from_csv(csv_line)
    # Parse "name,email" and create user
  end
  
  def admin?
    # Check if user is admin
  end
  
  def display_info
    # Return formatted user information
  end
end

# 3. Utility Class Methods
class MathUtils
  # All methods should be class methods (no instances needed)
  
  def self.fibonacci(n)
    # Return nth Fibonacci number
  end
  
  def self.prime?(number)
    # Check if number is prime
  end
  
  def self.factorial(n)
    # Calculate factorial of n
  end
  
  def self.gcd(a, b)
    # Find greatest common divisor
  end
end

# 4. Configuration with Class Methods
class DatabaseConfig
  # Class variables for configuration
  
  def self.host=(hostname)
    # Set database host
  end
  
  def self.host
    # Get database host
  end
  
  def self.port=(port_number)
    # Set database port
  end
  
  def self.port
    # Get database port
  end
  
  def self.connection_string
    # Return formatted connection string
  end
  
  def self.reset_to_defaults
    # Reset all config to default values
  end
end

# 5. Mixed Class and Instance Methods
class Logger
  def initialize(name)
    # Set logger name
  end
  
  def log(message)
    # Log message with timestamp and logger name
  end
  
  def self.global_log(message)
    # Log to global logger
  end
  
  def self.set_global_level(level)
    # Set global logging level
  end
  
  def self.create_file_logger(filename)
    # Create logger that writes to file
  end
  
  private
  
  def timestamp
    # Return current timestamp
  end
  
  def self.format_message(level, message)
    # Format log message with level
  end
end

# Example usage (uncomment these to test your code):
# counter1 = Counter.new
# counter2 = Counter.new
# counter1.increment
# puts counter1.count                    # 1
# puts Counter.total_instances           # 2

# admin = User.create_admin("Alice", "alice@admin.com")
# guest = User.create_guest
# puts admin.admin?                      # true
# puts guest.admin?                      # false

# puts MathUtils.fibonacci(10)           # 55
# puts MathUtils.prime?(17)              # true
# puts MathUtils.factorial(5)            # 120

# DatabaseConfig.host = "localhost"
# DatabaseConfig.port = 5432
# puts DatabaseConfig.connection_string  # "localhost:5432"

# logger = Logger.new("AppLogger")
# logger.log("Application started")
# Logger.global_log("Global message")