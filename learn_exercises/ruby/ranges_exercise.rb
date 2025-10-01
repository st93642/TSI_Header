# Range Objects Exercise  
# Practice working with Ruby ranges and their various applications

# Instructions:
# Ranges represent sequences of values with a beginning and end.
# They're useful for iteration, case statements, and array slicing.

# Your task is to implement the methods that the tests expect.
# Look at the test failures to understand what each method should do.

# 1. Basic Range Creation
# Create a method that returns a range from 1 to 10 (inclusive)
def one_to_ten_range
  # Return range 1..10
end

# Create a method that returns a range from 1 to 10 (exclusive of 10)
def one_to_ten_exclusive
  # Return range 1...10 (three dots excludes end)
end

# 2. Range Membership
# Create a method that checks if a number is in a range
def in_range?(number, start_num, end_num)
  # Check if number is within the range start_num..end_num
end

# 3. Range Iteration
# Create a method that returns an array of even numbers in a range
def even_numbers_in_range(start_num, end_num)
  # Return array of even numbers from start_num to end_num
end

# 4. Character Ranges
# Create a method that returns letters from 'a' to a given letter
def letters_up_to(end_letter)
  # Return range 'a'..end_letter as array
end

# 5. Range Applications
# Create a method that uses range in case statement
def grade_letter(score)
  # Return letter grade based on score:
  # 90-100: 'A', 80-89: 'B', 70-79: 'C', 60-69: 'D', below 60: 'F'
end

# Example usage (uncomment these to test your code):
# puts one_to_ten_range.to_a           # Should print [1,2,3,4,5,6,7,8,9,10]
# puts one_to_ten_exclusive.to_a       # Should print [1,2,3,4,5,6,7,8,9]
# puts in_range?(5, 1, 10)             # Should return true
# puts even_numbers_in_range(1, 10)    # Should return [2,4,6,8,10]
# puts letters_up_to('e')              # Should return ['a','b','c','d','e']
# puts grade_letter(85)                # Should return 'B'