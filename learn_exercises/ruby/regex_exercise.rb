# Regular Expressions Exercise
# Practice pattern matching and text processing with Ruby regex

# Instructions:
# Regular expressions (regex) are powerful tools for pattern matching
# and text manipulation. Ruby has built-in regex support with the // syntax.

# Your task is to implement the methods that the tests expect.
# Look at the test failures to understand what each method should do.

# 1. Basic Pattern Matching
# Create a method that checks if a string contains digits
def contains_digits?(text)
  # Use regex to check if text contains any digits 0-9
end

# Create a method that checks if string is a valid email format
def valid_email?(email)
  # Basic email validation: word@word.word
end

# 2. Pattern Extraction
# Create a method that extracts all numbers from a string
def extract_numbers(text)
  # Return array of all number sequences found in text
end

# Create a method that extracts phone number digits
def extract_phone_digits(phone)
  # Extract only digits from phone number string
end

# 3. Text Substitution
# Create a method that replaces all vowels with asterisks
def hide_vowels(text)
  # Replace a,e,i,o,u with * (case insensitive)
end

# Create a method that formats phone numbers
def format_phone(digits)
  # Format 10-digit string as (XXX) XXX-XXXX
end

# 4. Validation Patterns
# Create a method that validates password strength
def strong_password?(password)
  # Password must have: 8+ chars, 1 uppercase, 1 lowercase, 1 digit
end

# Create a method that validates username format
def valid_username?(username)
  # Username: 3-20 chars, letters/numbers/underscore only, start with letter
end

# Example usage (uncomment these to test your code):
# puts contains_digits?("hello123")        # Should return true
# puts valid_email?("user@example.com")    # Should return true
# puts extract_numbers("I have 5 cats and 3 dogs")  # Should return ["5", "3"]
# puts extract_phone_digits("(555) 123-4567")        # Should return "5551234567"
# puts hide_vowels("Hello World")          # Should return "H*ll* W*rld"
# puts format_phone("5551234567")          # Should return "(555) 123-4567"
# puts strong_password?("MyPass123")       # Should return true
# puts valid_username?("user_123")         # Should return true