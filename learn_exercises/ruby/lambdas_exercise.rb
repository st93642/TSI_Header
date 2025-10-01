# Lambdas Exercise

# 1. Create a method make_multiplier(n) that returns lambda multiplying by n
#    Example: mult = make_multiplier(3); mult.call(4) => 12
#    Example: mult = make_multiplier(5); mult.call(2) => 10
#    Hint: ->(x) { x * n } or lambda { |x| x * n }

def make_multiplier(n)
  # Return a lambda that multiplies its argument by n

end

# 2. Create a method transform(arr, operation) that applies lambda to each element
#    Example: transform([1,2,3], ->(x) { x * 2 }) => [2, 4, 6]
#    Example: transform([1,2,3], ->(x) { x + 10 }) => [11, 12, 13]
#    Hint: arr.map { |x| operation.call(x) }

def transform(arr, operation)
  # Apply the operation lambda to each array element

end

# 3. Create a method positive_checker that returns lambda checking if positive
#    Example: checker = positive_checker; checker.call(5) => true
#    Example: checker = positive_checker; checker.call(-3) => false
#    Hint: ->(x) { x > 0 }

def positive_checker
  # Return a lambda that returns true if number > 0

end
