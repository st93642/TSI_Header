# Testing Basics with Minitest Exercise
# Learn to write unit tests for Ruby code using Minitest

# Instructions:
# Testing is crucial for reliable software. Ruby's Minitest provides
# a simple framework for writing and running tests.

require 'minitest/autorun'

# Your task is to implement the methods that the tests expect.
# Look at the test failures to understand what each method should do.

# 1. Basic Calculator for Testing
class Calculator
  def add(a, b)
    # Return sum of a and b
  end
  
  def subtract(a, b)
    # Return difference of a and b
  end
  
  def multiply(a, b)
    # Return product of a and b
  end
  
  def divide(a, b)
    # Return quotient of a and b, handle division by zero
  end
end

# 2. String Utilities for Testing
class StringUtils
  def self.reverse_words(sentence)
    # Reverse the order of words in sentence
  end
  
  def self.count_vowels(text)
    # Count vowels (a,e,i,o,u) in text
  end
  
  def self.palindrome?(text)
    # Check if text reads same forwards and backwards
  end
end

# 3. Array Utilities for Testing
class ArrayUtils
  def self.find_max(numbers)
    # Find maximum number in array
  end
  
  def self.remove_duplicates(array)
    # Return array with duplicates removed
  end
  
  def self.average(numbers)
    # Calculate average of numbers array
  end
end

# 4. Sample Test Class (for reference)
class TestCalculator < Minitest::Test
  def setup
    @calc = Calculator.new
  end
  
  def test_addition
    assert_equal 4, @calc.add(2, 2)
    assert_equal 0, @calc.add(-1, 1)
  end
  
  def test_subtraction
    assert_equal 0, @calc.subtract(2, 2)
    assert_equal 5, @calc.subtract(10, 5)
  end
  
  # Add more test methods for multiply and divide
end

# Example usage (uncomment these to test your code):
# calc = Calculator.new
# puts calc.add(5, 3)                    # Should return 8
# puts StringUtils.reverse_words("hello world")  # Should return "world hello"
# puts StringUtils.count_vowels("hello")          # Should return 2
# puts StringUtils.palindrome?("radar")           # Should return true
# puts ArrayUtils.find_max([1, 5, 3, 9, 2])     # Should return 9
# puts ArrayUtils.remove_duplicates([1, 2, 2, 3, 1])  # Should return [1, 2, 3]
# puts ArrayUtils.average([1, 2, 3, 4, 5])            # Should return 3.0