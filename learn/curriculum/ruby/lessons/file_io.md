# File Input/Output

Ruby makes it easy to read from and write to files. File I/O is essential for saving data, loading configuration, and processing text files.

## Reading Files

### Read Entire File

```ruby
# Read entire file into string
content = File.read("example.txt")
puts content

# Read into array of lines
lines = File.readlines("example.txt")
lines.each { |line| puts line }
```

### Read Line by Line

```ruby
File.open("example.txt", "r") do |file|
  file.each_line do |line|
    puts line
  end
end
```

## Writing Files

### Write (Overwrite)

```ruby
# Write string to file (overwrites existing)
File.write("output.txt", "Hello, World!")

# Write multiple lines
content = "Line 1
Line 2
Line 3"
File.write("output.txt", content)
```

### Append to File

```ruby
# Append without overwriting
File.open("log.txt", "a") do |file|
  file.puts "New log entry"
  file.puts "Another entry"
end
```

## File Modes

Common file open modes:

- `"r"` - Read only (default)
- `"w"` - Write only (overwrites)
- `"a"` - Append only
- `"r+"` - Read and write
- `"w+"` - Read and write (overwrites)
- `"a+"` - Read and append

```ruby
# Read and write
File.open("data.txt", "r+") do |file|
  content = file.read
  file.write("
New line")
end
```

## File Existence

```ruby
if File.exist?("config.txt")
  puts "File exists!"
else
  puts "File not found"
end

# Alternative
File.file?("config.txt")  # true if regular file
File.directory?("data")   # true if directory
```

## File Information

```ruby
file_path = "example.txt"

File.size(file_path)        # Size in bytes
File.basename(file_path)    # => "example.txt"
File.dirname(file_path)     # Directory path
File.extname(file_path)     # => ".txt"

# Modification time
File.mtime(file_path)       # => Time object
```

## Safe File Handling

Using blocks ensures files are closed automatically:

```ruby
# File closes automatically after block
File.open("data.txt", "r") do |file|
  content = file.read
  # Process content
end
# File is closed here

# Manual close (not recommended)
file = File.open("data.txt", "r")
content = file.read
file.close  # Don't forget!
```

## CSV Files

```ruby
require 'csv'

# Write CSV
CSV.open("users.csv", "w") do |csv|
  csv << ["Name", "Age", "City"]
  csv << ["Alice", 30, "NYC"]
  csv << ["Bob", 25, "LA"]
end

# Read CSV
CSV.foreach("users.csv", headers: true) do |row|
  puts "Name: #{row['Name']}, Age: #{row['Age']}"
end
```

## JSON Files

```ruby
require 'json'

# Write JSON
data = { name: "Alice", age: 30, city: "NYC" }
File.write("user.json", JSON.pretty_generate(data))

# Read JSON
json_string = File.read("user.json")
user = JSON.parse(json_string)
puts user["name"]  # => "Alice"
```

## Practical Examples

### Count lines in file

```ruby
line_count = File.readlines("example.txt").size
puts "File has #{line_count} lines"
```

### Find and replace in file

```ruby
content = File.read("config.txt")
updated = content.gsub("old_value", "new_value")
File.write("config.txt", updated)
```

### Log to file

```ruby
def log_message(message)
  timestamp = Time.now.strftime("%Y-%m-%d %H:%M:%S")
  File.open("app.log", "a") do |file|
    file.puts "[#{timestamp}] #{message}"
  end
end

log_message("Application started")
log_message("User logged in")
```

## Key Takeaways

- `File.read()` - read entire file
- `File.readlines()` - read as array of lines
- `File.write()` - write (overwrites)
- `File.open() { |f| }` - safe file handling
- Use `"r"` for read, `"w"` for write, `"a"` for append
- `File.exist?()` - check if file exists
- Blocks auto-close files
- Use CSV and JSON libraries for structured data
