# E-Commerce Application Constants
# Build a realistic e-commerce application configuration system

# You're building an online store called "TSI Market" and need to configure
# various constants for the application. This mirrors real-world scenarios
# where constants are used for configuration, limits, and calculations.

# ===== PART 1: Application Identity Constants =====
# Define the core application identity constants

# STORE_NAME should be "TSI Market"


# STORE_VERSION should be "2.1.0"


# STORE_MOTTO should be "Quality Tech for Students"


# ===== PART 2: Business Configuration Constants =====
# Define business rules and limits

# FREE_SHIPPING_THRESHOLD should be 50 (minimum order for free shipping)


# MAX_CART_ITEMS should be 20 (maximum items per cart)


# DISCOUNT_CODE should be "STUDENT25" (25% discount for students)


# ===== PART 3: Mathematical Constants for Calculations =====
# Define constants used in business calculations

# TAX_RATE should be 0.08 (8% sales tax)


# SHIPPING_RATE should be 5.99 (flat shipping rate)


# ===== PART 4: Security Constants =====
# Define security-related constants

# MIN_PASSWORD_LENGTH should be 8


# MAX_LOGIN_ATTEMPTS should be 3


# SESSION_TIMEOUT should be 1800 (30 minutes in seconds)


# ===== PART 5: Business Logic Methods =====
# Create methods that use your constants

# Calculate total cost including tax

# calculate_total(subtotal) should return subtotal + (subtotal * TAX_RATE)
def calculate_total(subtotal)
  # Your code here
end

# Check if order qualifies for free shipping
# free_shipping?(order_total) should return true if >= FREE_SHIPPING_THRESHOLD
def free_shipping?(order_total)
  # Your code here
end

# Validate password meets minimum requirements
# valid_password?(password) should check length against MIN_PASSWORD_LENGTH
def valid_password?(password)
  # Your code here
end

# Apply student discount
# apply_student_discount(price) should reduce price by 25%
def apply_student_discount(price)
  # Your code here
end

# ===== PART 6: Configuration Module =====
# Group related constants in a module for better organization

# Create a Database module with:
# - HOST constant = "localhost"
# - PORT constant = 5432
# - NAME constant = "tsi_market_production"

module Database
  # Your constants here
end

# Example usage - these should work when you're done:
# puts "Welcome to #{STORE_NAME} v#{STORE_VERSION}!"
# puts "#{STORE_MOTTO}"
# puts "Order total: $#{calculate_total(100.00)}"
# puts "Free shipping: #{free_shipping?(75.00)}"
# puts "Password valid: #{valid_password?('mypassword123')}"
# puts "Student price: $#{apply_student_discount(100.00)}"
# puts "Database: #{Database::HOST}:#{Database::PORT}/#{Database::NAME}"


# puts "App: #{APP_NAME} v#{VERSION}"
# puts "Circle area (r=5): #{calculate_area(5)}"
# puts "Valid password 'secret': #{valid_password?('secret')}"
# puts "Valid password 'mypassword': #{valid_password?('mypassword')}"
# puts "Database host: #{Config::DATABASE_HOST}"