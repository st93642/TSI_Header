# Testing System Demo Exercise
# Advanced testing concepts and demonstration of test-driven development

# Instructions:
# This exercise demonstrates advanced testing concepts including
# mocking, stubbing, and test-driven development practices.

require 'minitest/autorun'

# Your task is to implement the methods that the tests expect.
# Look at the test failures to understand what each method should do.

# 1. Weather Service (for testing external dependencies)
class WeatherService
  def get_temperature(city)
    # Simulate API call - return temperature for city
    # In real app, this would make HTTP request
  end
  
  def get_forecast(city, days)
    # Return array of temperatures for next N days
  end
end

# 2. User Authentication System
class UserAuth
  def initialize(database = nil)
    @database = database || UserDatabase.new
  end
  
  def authenticate(username, password)
    # Check username/password against database
  end
  
  def create_user(username, password, email)
    # Create new user, return success/failure
  end
  
  def reset_password(username, new_password)
    # Reset user password
  end
end

# 3. Mock Database for Testing
class UserDatabase
  def initialize
    @users = {}
  end
  
  def find_user(username)
    # Find user by username
  end
  
  def save_user(user_data)
    # Save user data
  end
  
  def user_exists?(username)
    # Check if user exists
  end
end

# 4. Email Service (for testing side effects)
class EmailService
  def send_welcome_email(user_email)
    # Send welcome email to user
  end
  
  def send_password_reset(user_email, reset_token)
    # Send password reset email
  end
end

# 5. Integration Service (combines multiple services)
class UserRegistration
  def initialize(auth_service, email_service)
    @auth = auth_service
    @email = email_service
  end
  
  def register_user(username, password, email)
    # Register user and send welcome email
  end
  
  def password_reset_flow(username)
    # Handle password reset workflow
  end
end

# 6. Test Helper Methods
def create_mock_database
  # Create a mock database for testing
end

def create_test_user
  # Create a test user object
end

def simulate_network_error
  # Simulate network failure for testing error handling
end

# Example Test Class (for reference)
class TestUserAuth < Minitest::Test
  def setup
    @auth = UserAuth.new
  end
  
  def test_successful_authentication
    # Test successful login
  end
  
  def test_failed_authentication
    # Test failed login
  end
  
  def test_user_creation
    # Test user creation
  end
end

# Example usage (uncomment these to test your code):
# weather = WeatherService.new
# puts weather.get_temperature("New York")     # Should return temperature
# puts weather.get_forecast("Boston", 3)       # Should return 3-day forecast

# auth = UserAuth.new
# puts auth.create_user("alice", "pass123", "alice@example.com")
# puts auth.authenticate("alice", "pass123")   # Should return true

# email = EmailService.new
# email.send_welcome_email("alice@example.com")

# registration = UserRegistration.new(auth, email)
# registration.register_user("bob", "secret", "bob@example.com")