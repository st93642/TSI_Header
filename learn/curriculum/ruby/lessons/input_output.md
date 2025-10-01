# Input/Output Operations

## Overview

Input/Output (I/O) operations allow your Ruby programs to interact with the outside world. This includes reading from and writing to files, handling command-line arguments, and working with standard input/output streams.

## File Operations

Ruby's `IO` class provides methods for working with files. The most common way to work with files is using the `File` class, which inherits from `IO`.

### Reading Files

```ruby
# Read entire file at once
content = File.read('example.txt')

# Read file line by line
File.foreach('example.txt') do |line|
  puts line
end

# Read all lines into an array
lines = File.readlines('example.txt')

# Open and automatically close file
File.open('example.txt', 'r') do |file|
  content = file.read
end
```

### Writing Files

```ruby
# Write to file (overwrites existing content)
File.write('output.txt', 'Hello, World!')

# Append to file
File.write('output.txt', '\\nAppended text', mode: 'a')

# Open file for writing
File.open('output.txt', 'w') do |file|
  file.puts 'Line 1'
  file.puts 'Line 2'
end
```

### File Modes

- `'r'`: Read-only (default)
- `'w'`: Write-only (truncates existing file)
- `'a'`: Write-only (appends to existing file)
- `'r+'`: Read and write
- `'w+'`: Read and write (truncates file)
- `'a+'`: Read and write (appends)

## Command-Line Arguments

Command-line arguments are available through the `ARGV` array.

```ruby
# script.rb
puts \"Number of arguments: #{ARGV.length}\"
puts \"Arguments: #{ARGV.inspect}\"

# Run with: ruby script.rb arg1 arg2 arg3
```

## Standard Input/Output

### Standard Output (STDOUT)

```ruby
# Print to standard output
puts 'Hello, World!'  # Adds newline
print 'Hello, World!' # No newline

# Using STDOUT explicitly
STDOUT.puts 'Direct to STDOUT'
```

### Standard Input (STDIN)

```ruby
# Read from standard input
name = gets.chomp  # Read line and remove newline
puts \"Hello, #{name}!\"

# Read all input
input = STDIN.read
```

## File Paths and Directories

```ruby
# Check if file exists
File.exist?('example.txt')

# Check if directory exists
Dir.exist?('my_directory')

# Get current directory
current_dir = Dir.pwd

# Create directory
Dir.mkdir('new_directory')

# List files in directory
files = Dir.entries('.')
```

## Error Handling

Always handle potential I/O errors:

```ruby
begin
  File.open('example.txt', 'r') do |file|
    content = file.read
    puts content
  end
rescue Errno::ENOENT
  puts 'File not found'
rescue Errno::EACCES
  puts 'Permission denied'
end
```

## Best Practices

1. Always close files when done (use blocks)
2. Handle exceptions for file operations
3. Use appropriate file modes
4. Consider file encodings for text files
5. Use relative paths carefully
