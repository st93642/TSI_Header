# Standard Library: JSON and CSV

## Overview

Ruby's standard library includes powerful tools for working with common data formats. The JSON and CSV libraries allow you to easily parse, generate, and manipulate structured data.

## JSON (JavaScript Object Notation)

JSON is a lightweight data interchange format that's easy for humans to read and write, and easy for machines to parse and generate.

### Parsing JSON

```ruby
require 'json'

# Parse JSON string to Ruby objects
json_string = '{"name": "Alice", "age": 30, "active": true}'
data = JSON.parse(json_string)
# => {"name"=>"Alice", "age"=>30, "active"=>true}

# Parse JSON array
json_array = '[1, 2, "three", true, null]'
data = JSON.parse(json_array)
# => [1, 2, "three", true, nil]
```

### Generating JSON

```ruby
require 'json'

# Convert Ruby objects to JSON strings
ruby_hash = {name: "Bob", age: 25, skills: ["Ruby", "JavaScript"]}
json_string = JSON.generate(ruby_hash)
# => "{\"name\":\"Bob\",\"age\":25,\"skills\":[\"Ruby\",\"JavaScript\"]}"

# Pretty-print JSON
pretty_json = JSON.pretty_generate(ruby_hash)
puts pretty_json
# {
#   "name": "Bob",
#   "age": 25,
#   "skills": [
#     "Ruby",
#     "JavaScript"
#   ]
# }
```

### JSON Options

```ruby
# Symbolize keys instead of strings
data = JSON.parse(json_string, symbolize_names: true)
# => {:name=>"Alice", :age=>30, :active=>true}

# Allow NaN and Infinity values
data = JSON.parse(json_string, allow_nan: true)

# Custom object class
data = JSON.parse(json_string, object_class: OpenStruct)
```

## CSV (Comma-Separated Values)

CSV is a common format for tabular data. Ruby's CSV library handles various CSV dialects and edge cases.

### Reading CSV Data

```ruby
require 'csv'

# Read CSV from string
csv_data = "Name,Age,City\nAlice,30,New York\nBob,25,London\n"
data = CSV.parse(csv_data)
# => [["Name", "Age", "City"], ["Alice", "30", "New York"], ["Bob", "25", "London"]]

# Read CSV with headers
csv_with_headers = "Name,Age,City\nAlice,30,New York\nBob,25,London\n"
data = CSV.parse(csv_with_headers, headers: true)
data.each do |row|
  puts "#{row['Name']} is #{row['Age']} years old"
end
```

### Writing CSV Data

```ruby
require 'csv'

# Generate CSV string
data = [
  ['Name', 'Age', 'City'],
  ['Alice', 30, 'New York'],
  ['Bob', 25, 'London']
]
csv_string = CSV.generate do |csv|
  data.each { |row| csv << row }
end
puts csv_string

# Write to file
CSV.open('people.csv', 'w') do |csv|
  csv << ['Name', 'Age', 'City']
  csv << ['Alice', 30, 'New York']
  csv << ['Bob', 25, 'London']
end
```

### CSV Options

```ruby
# Custom separators
CSV.parse(csv_data, col_sep: ';')  # Semicolon-separated
CSV.parse(csv_data, col_sep: "\t") # Tab-separated

# Different quote characters
CSV.parse(csv_data, quote_char: "'")  # Single quotes

# Skip blank lines
CSV.parse(csv_data, skip_blanks: true)

# Convert data types automatically
CSV.parse(csv_data, converters: :numeric)
```

## Working with Files

### JSON Files

```ruby
require 'json'

# Read JSON from file
data = JSON.parse(File.read('data.json'))

# Write JSON to file
File.write('output.json', JSON.pretty_generate(data))

# Load JSON file directly
require 'json'
data = JSON.load_file('data.json')
```

### CSV Files

```ruby
require 'csv'

# Read CSV file
CSV.foreach('data.csv') do |row|
  puts row.inspect
end

# Read CSV file with headers
CSV.foreach('data.csv', headers: true) do |row|
  puts "#{row['Name']}: #{row['Value']}"
end

# Read entire CSV file
all_data = CSV.read('data.csv')

# Write CSV file
CSV.open('output.csv', 'w') do |csv|
  csv << ['Column1', 'Column2']
  csv << ['Value1', 'Value2']
end
```

## Advanced Features

### JSON Additions

Ruby can automatically serialize/deserialize certain objects:

```ruby
require 'json/add/time'

time = Time.now
json = JSON.generate(time)
# => "{\"json_class\":\"Time\",\"s\":1672531200,\"n\":0}"

parsed_time = JSON.parse(json, create_additions: true)
# => 2023-01-01 00:00:00 +0000
parsed_time.class # => Time
```

### CSV Tables

```ruby
require 'csv'

# Create a CSV table
table = CSV.table('data.csv')

# Access data like a spreadsheet
table.each do |row|
  puts row[:name]
end

# Filter and manipulate data
adults = table.select { |row| row[:age] >= 18 }
```

## Error Handling

```ruby
require 'json'
require 'csv'

# JSON parsing errors
begin
  data = JSON.parse(invalid_json)
rescue JSON::ParserError => e
  puts "Invalid JSON: #{e.message}"
end

# CSV parsing errors
begin
  data = CSV.parse(malformed_csv)
rescue CSV::MalformedCSVError => e
  puts "Invalid CSV: #{e.message}"
end
```

## Best Practices

1. **Use headers**: When working with CSV data that has column names, use `headers: true`
2. **Handle encodings**: Specify encoding when reading files with special characters
3. **Validate data**: Always check data types after parsing
4. **Use blocks for files**: Automatically close files when done
5. **Pretty-print for debugging**: Use `JSON.pretty_generate` when inspecting data
6. **Consider performance**: For large files, process line-by-line rather than loading everything into memory

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: JSON & CSV Patterns

This appendix shows quick tips for streaming JSON/CSV, safe parsing, and an HTML table of converters.

```ruby
require 'json'
File.open('data.json') do |f|
  f.each_line do |line|
    record = JSON.parse(line)
    process(record)
  end
end
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Library</th><th>Purpose</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>JSON</td><td>Serialization</td><td>Use pretty_generate for debug</td></tr>
    <tr><td>CSV</td><td>Tabular I/O</td><td>Headers: true when available</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises

1. Write a streaming CSV aggregator that merges multiple CSV files into one normalized file.
2. Add a `--pretty` flag to pretty-print JSON output for debugging.

<!-- markdownlint-enable MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Standard Library — Useful Modules & Patterns (Appendix — standard_library-ruby2)

A compact reference to frequently used stdlib modules and pragmatic examples for real projects.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Module</th><th>Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>JSON</td><td>Parse/Generate JSON</td><td>Prefer `JSON.parse` with symbolize_names when useful</td></tr>
    <tr><td>CSV</td><td>CSV file processing</td><td>Use streaming for large files</td></tr>
    <tr><td>Net::HTTP</td><td>HTTP clients</td><td>Consider `uri` and error handling for timeouts</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Quick examples

```ruby
require 'json'
obj = JSON.parse('{"a":1}', symbolize_names: true)

require 'csv'
CSV.foreach('data.csv', headers: true) do |row|
  process(row)
end
```

### Testing stdlib interactions

- Stub network calls with libraries (e.g., `WebMock` or `VCR`) to keep tests deterministic.
- Use temporary files or `Tempfile` for files in tests.

```ruby
require 'minitest/autorun'
require 'tempfile'

class TestStdlib < Minitest::Test
  def test_tempfile
    t = Tempfile.new('x')
    t.write('hello')
    t.rewind
    assert_equal 'hello', t.read
  ensure
    t.close!
  end
end
```

### Exercises (Appendix — standard_library-ruby2)

1. Read a CSV file with headers and convert it into an array of hashes; test with a small fixture.
2. Implement a wrapper over `Net::HTTP` that retries on transient failures and write tests using a stubbed server.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: Standard Library Highlights — net/http, JSON & open-uri (Appendix — standard_library-ruby-appendix-20251005)

Small recipes showing common standard library usages and safe patterns for network I/O and JSON parsing.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Library</th><th>Use</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>net/http</td><td>HTTP clients</td><td>Use timeouts and handle errors</td></tr>
    <tr><td>JSON</td><td>Parsing</td><td>Rescue parse errors</td></tr>
    <tr><td>open-uri</td><td>Quick fetch</td><td>Beware untrusted URLs</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: safe JSON parse

```ruby
require 'json'
begin
  obj = JSON.parse(text)
rescue JSON::ParserError
  # handle bad JSON
end
```

### Exercises (Appendix — standard_library-ruby-appendix-20251005)

1. Fetch JSON from a URL with a timeout and parse it safely, adding tests for timeouts.
2. Use `open-uri` with caution and write a wrapper that validates the URL first.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
