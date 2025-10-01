# TSI Student Portal Authentication System - Boolean Logic Exercise
# Practice with boolean values, comparison operators, and logical operators in Ruby

# Instructions:
# You're building an authentication system for the TSI Student Portal.
# Boolean logic is essential for security, permissions, and user validation.
# Implement the methods below to handle real-world authentication scenarios.

# ===== PART 1: Basic Authentication Logic =====

# Create a method valid_login? that checks if both username AND password match
# Parameters: username, password, stored_username, stored_password
# Returns: true only if both match exactly

def valid_login?(username, password, stored_username, stored_password)
  # Your code here

end

# Create a method account_active? that checks if an account can be used
# Parameters: is_active (boolean), is_suspended (boolean), payment_current (boolean)
# Returns: true only if active AND not suspended AND payment is current
def account_active?(is_active, is_suspended, payment_current)
  # Your code here

end

# ===== PART 2: Permission System =====

# Create a method admin_access? that determines if user has admin privileges
# Parameters: user_type (string), account_age_days (number)
# Returns: true if user is 'admin' OR 'faculty' OR ('student' with account > 365 days)
def admin_access?(user_type, account_age_days)
  # Your code here

end

# Create a method library_access? that checks library permissions
# Parameters: is_student (boolean), is_faculty (boolean), library_fees_paid (boolean)
# Returns: true if (student OR faculty) AND fees are paid
def library_access?(is_student, is_faculty, library_fees_paid)
  # Your code here

end

# ===== PART 3: Security Validation =====

# Create a method strong_password? that validates password strength
# Parameters: password (string)
# Returns: true if password is 8+ characters AND contains letters AND numbers
def strong_password?(password)
  # Your code here

end

# Create a method suspicious_login? that flags risky login attempts
# Parameters: failed_attempts (number), login_hour (0-23), is_weekend (boolean)
# Returns: true if failed_attempts >= 3 OR login_hour < 6 OR login_hour > 22 OR is_weekend
def suspicious_login?(failed_attempts, login_hour, is_weekend)
  # Your code here

end

# ===== PART 4: Course Enrollment Logic =====

# Create a method can_enroll? that checks if student can enroll in a course
# Parameters: prerequisite_completed, has_space, registration_open, tuition_paid (all boolean)
# Returns: true only if ALL conditions are met
def can_enroll?(prerequisite_completed, has_space, registration_open, tuition_paid)
  # Your code here

end

# Create a method scholarship_eligible? that determines scholarship qualification
# Parameters: gpa (number), financial_need (boolean), is_citizen (boolean), extracurricular_hours (number)
# Returns: true if gpa >= 3.5 AND (financial_need OR is_citizen) AND extracurricular_hours >= 50
def scholarship_eligible?(gpa, financial_need, is_citizen, extracurricular_hours)
  # Your code here

end

# ===== PART 5: Ruby Truthiness Understanding =====

# Create a method truthy? that checks if a value is truthy in Ruby
# Parameters: value (any type)
# Returns: boolean representation of the value (only false and nil are falsy)
def truthy?(value)
  # Your code here

end

# Create a method valid_input? that checks for nil or empty strings
# Parameters: input (any type)
# Returns: false if input is nil or empty/whitespace string, true otherwise
def valid_input?(input)
  # Your code here

end

# Example usage (uncomment these to test your code):
# puts valid_login?('student1', 'correct123', 'student1', 'correct123')  # Should print: true
# puts account_active?(true, false, true)                                # Should print: true
# puts admin_access?('student', 400)                                    # Should print: true
# puts library_access?(true, false, true)                              # Should print: true
# puts strong_password?('mypass123')                                     # Should print: true
# puts suspicious_login?(5, 14, false)                                  # Should print: true
# puts can_enroll?(true, true, true, true)                             # Should print: true
# puts scholarship_eligible?(3.8, true, false, 60)                     # Should print: true
# puts truthy?(0)                                                       # Should print: true
# puts valid_input?('hello')                                            # Should print: true
