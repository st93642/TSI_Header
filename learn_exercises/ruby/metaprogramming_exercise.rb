# Metaprogramming Basics Exercise

# Create a Calculator class with basic methods

class Calculator
  def add(a, b)
    a + b
  end
  
  def multiply(a, b)
    a * b
  end
  
  def subtract(a, b)
    a - b
  end
end

# Create a method that calls Calculator methods dynamically
def calculate(operation, a, b)
  # Use send to call the operation method on a Calculator instance
  # Return the result

end
