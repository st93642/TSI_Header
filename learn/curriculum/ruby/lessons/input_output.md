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

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Robust I/O Patterns

This appendix gives reusable patterns for file IO, interactive prompts, and test-friendly IO in Ruby.

### Safe file reading

```ruby
begin
  content = File.read('data.txt')
rescue Errno::ENOENT
  warn 'Missing data.txt; please add sample input.'
  content = ''
end
```

### Stream processing pattern

```ruby
File.open('data.txt') do |f|
  f.each_line do |line|
    process_line(line.chomp)
  end
end
```

### CI snippets and HTML table for expected environments

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Environment</th><th>Ruby</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Ubuntu</td><td>3.0+</td><td>Use rbenv or system Ruby in CI</td></tr>
    <tr><td>macOS</td><td>3.0+</td><td>Prefer Ruby via Homebrew for reproducibility</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Redirecting IO in tests

```ruby
def with_captured_io
  old_stdout = $stdout
  old_stdin = $stdin
  $stdout = StringIO.new
  $stdin = StringIO.new("input\n")
  yield
  $stdout.string
ensure
  $stdout = old_stdout
  $stdin = old_stdin
end
```

### Exercises

1. Implement a script that reads CSV from stdin and prints column totals.
2. Create a test that simulates interactive input for a prompt-based script.
3. Build a small CLI that accepts flags via `ARGV` and responds appropriately.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: CLI & Input Patterns

This appendix shows OptionParser usage, safe STDIN handling, and an HTML table summarizing common helpers.

```ruby
require 'optparse'
options = {}
OptionParser.new do |opts|
  opts.on('-v','--verbose') { options[:verbose] = true }
end.parse!
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Helper</th><th>Purpose</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>ARGV</td><td>Raw args</td><td>Simplest</td></tr>
    <tr><td>OptionParser</td><td>Structured parsing</td><td>Best for flags and options</td></tr>
    <tr><td>STDIN.read</td><td>Stream input</td><td>Works with pipes</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (Practical Appendix)

1. Write a CLI that accepts input files or stdin and normalizes line endings.
2. Add a `--format=json` flag and output normalized JSON when requested.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: IO Patterns — Resources (Appendix — External Links)

References and safe patterns for file and stream IO in Ruby.

- Ruby IO docs: [IO class reference](https://ruby-doc.org/core/IO.html)
- Tempfile usage: [Tempfile docs](https://ruby-doc.org/stdlib-3.2.0/libdoc/tempfile/rdoc/Tempfile.html)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Use</th><th>Doc</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>File</td><td><a href="https://ruby-doc.org/core/File.html">File</a></td><td>Prefer block form to auto-close</td></tr>
    <tr><td>Tempfile</td><td><a href="https://ruby-doc.org/stdlib-3.2.0/libdoc/tempfile/rdoc/Tempfile.html">Tempfile</a></td><td>Safe temporary files during tests</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises (External Resources)

1. Rework a sample to use Tempfile in tests and ensure cleanup runs.
2. Demonstrate reading a binary file safely and add related unit assertions.

<!-- markdownlint-enable MD010 -->
