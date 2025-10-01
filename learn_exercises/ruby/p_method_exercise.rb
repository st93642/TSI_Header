# Using P for Debugging Exercise
# Learn to use the p method for debugging Ruby code

# Instructions:
# The p method is essential for debugging in Ruby. It prints the raw
# representation of objects, making it perfect for debugging.

# Your task is to implement methods that use p for debugging output.
# Look at the test failures to understand what each method should do.

# 1. Basic p usage
# Create a method that returns the current year as a number
def get_year
  # Use p to debug - see what Time.now looks like
  # p Time.now
  # Return just the year as a number
end

# 2. Debugging with p
# Create a method that debugs variable values
def debug_calculation(a, b)
  # Use p to show the values of a and b
  # Calculate and return a + b * 2
  # Use p to show the result before returning
end

# 3. P vs puts comparison  
# Create a method that shows the difference between p and puts
def demonstrate_p_vs_puts(value)
  # This method doesn't return anything, just demonstrates
  # Use puts to show the value
  # Use p to show the value
  # Notice the difference in output
end

# Example usage (uncomment these to test your debugging):
# puts get_year()                        # Should return 2025
# puts debug_calculation(5, 3)           # Should return 11, with debug output
# demonstrate_p_vs_puts("Hello World")   # Shows difference between p and puts
