# Task: Error handling

def safe_divide(a, b)
  # Return result or "Error" if b is zero
  return "Error" if b == 0
  a / b
end

def check_positive(num)
  # Raise error if negative, else return num
  raise "Negative!" if num < 0
  num
end

def rescue_example
  begin
    1 / 0
  rescue => e
    # Return "Caught: ZeroDivisionError"
  end
end
