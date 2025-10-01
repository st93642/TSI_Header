# Input/Output Operations Exercise
# Practice reading user input and displaying output in various formats

# Instructions:
# Input/Output (I/O) operations allow programs to interact with users
# and external data sources. Ruby provides several methods for I/O.

# Your task is to implement the methods that the tests expect.
# Look at the test failures to understand what each method should do.

# 1. Basic Output Formatting
# Create a method that formats a greeting message
def format_greeting(name, time_of_day)
  # Return "Good [time_of_day], [name]!"
end

# Create a method that formats numbers as currency
def format_currency(amount)
  # Return amount formatted as "$X.XX"
end

# 2. String Interpolation and Formatting
# Create a method that creates a user profile summary
def user_profile(name, age, city, occupation)
  # Return formatted string with all info
end

# Create a method that formats a table row
def format_table_row(id, name, score)
  # Return formatted string: "ID: XX | Name: XXXX | Score: XX"
end

# 3. Multi-line Output
# Create a method that generates a simple report
def generate_report(title, items)
  # Return multi-line string with title and bulleted items
end

# Create a method that creates an ASCII box around text
def create_text_box(text)
  # Return text surrounded by ASCII borders
end

# 4. Data Formatting
# Create a method that formats file size
def format_file_size(bytes)
  # Convert bytes to KB, MB, GB with appropriate unit
end

# Create a method that formats time duration
def format_duration(seconds)
  # Convert seconds to "Xh Ym Zs" format
end

# 5. Simulated Input Processing
# Create a method that processes comma-separated values
def process_csv_line(csv_line)
  # Split CSV line and return array of trimmed values
end

# Create a method that parses key=value pairs
def parse_config_line(config_line)
  # Parse "key=value" format, return hash
end

# Example usage (uncomment these to test your code):
# puts format_greeting("Alice", "morning")     # "Good morning, Alice!"
# puts format_currency(12.99)                 # "$12.99"
# puts user_profile("Bob", 25, "NYC", "Developer")
# puts format_table_row(1, "Alice", 95)
# puts generate_report("Sales", ["Item A", "Item B"])
# puts create_text_box("Hello")
# puts format_file_size(1048576)              # "1.0 MB"
# puts format_duration(3661)                  # "1h 1m 1s"
# puts process_csv_line("a, b, c ")           # ["a", "b", "c"]
# puts parse_config_line("debug=true")        # {"debug" => "true"}