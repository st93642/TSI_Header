# Conditional Statements Exercise

# 1. Create a method check_number(num) that returns:
#    - "positive" if num > 0
#    - "negative" if num < 0
#    - "zero" if num == 0
#    Example: check_number(5) => "positive"
#    Example: check_number(-3) => "negative"
#    Example: check_number(0) => "zero"

def check_number(num)
  # Use if/elsif/else to check the number
  if num > 0
    "positive"
  elsif num < 0
    "negative"
  else
    "zero"
  end
end

# 2. Create a method age_group(age) that returns:
#    - "child" if age < 13
#    - "teen" if age >= 13 and age <= 17
#    - "adult" if age >= 18
#    Example: age_group(10) => "child"
#    Example: age_group(15) => "teen"
#    Example: age_group(25) => "adult"

def age_group(age)
  # Use if/elsif/else to determine age group
  if age < 13
    "child"
  elsif age >= 13 && age <= 17
    "teen"
  else
    "adult"
  end
end

# 3. Create a method passing?(score) that returns:
#    - true if score >= 60
#    - false if score < 60
#    Example: passing?(75) => true
#    Example: passing?(45) => false

def passing?(score)
  # Use a simple if statement or comparison
  score >= 60
end
