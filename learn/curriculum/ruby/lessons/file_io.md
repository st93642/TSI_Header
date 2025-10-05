# File input and output

Persistent data lives in files—configuration, logs, exports, and more. Ruby’s IO classes make reading and writing straightforward, whether you’re streaming gigabytes or editing a tiny JSON snippet. Mastering these tools helps you process data efficiently and safely.

## Learning goals

- Read and write text and binary files using the `File` API and IO enumerators.
- Choose appropriate modes (`"r"`, `"w"`, `"a"`, binary flags) and manage encoding explicitly.
- Work with structured formats (CSV, JSON, YAML) via the standard library.
- Use helpers like `Pathname`, `Dir`, `Tempfile`, and file locking to build robust scripts.
- Avoid resource leaks and race conditions by embracing block form and defensive coding.

## Reading files

```ruby
# Entire file as string (watch memory usage for large files)
content = File.read("example.txt")

# Array of lines (includes trailing newlines)
lines = File.readlines("example.txt", chomp: true)

# Streaming line by line
File.open("example.txt", "r") do |file|
  file.each_line.with_index do |line, index|
    puts "#{index + 1}: #{line.chomp}"
  end
end

# Enumerator for lazy processing
File.foreach("example.txt") do |line|
  # handles file opening/closing automatically
end
```

Prefer streaming methods (`foreach`, `each_line`) for large files to avoid loading everything into memory.

## Writing files

```ruby
File.write("output.txt", "Hello, world!\n")         # overwrites

File.open("log.txt", "a") do |file|                   # append mode
  file.puts "[#{Time.now.iso8601}] Started"
end

File.open("data.bin", "wb") do |file|                 # binary write
  file.write([0xFF, 0x00, 0x12].pack("C*"))
end
```

Opening files with a block ensures they close automatically—even if an exception occurs. Modes:

- `"r"`: read-only (default)
- `"w"`: write-only, truncates existing file
- `"a"`: append write
- `"r+"`: read/write without truncation
- Add `b` for binary (`"rb"`, `"wb"`) and `:encoding` option for text: `File.open("notes.txt", "r", encoding: "UTF-8")`.

## File metadata and existence checks

```ruby
path = "example.txt"

File.exist?(path)        # => true/false
File.file?(path)         # true if regular file
File.directory?(path)    # true if directory

File.size(path)          # bytes
File.mtime(path)         # last modified Time
File.basename(path)      # file name
File.dirname(path)       # directory portion
File.extname(path)       # ".txt"
```

`FileUtils` adds copying, moving, and removal helpers; require it when doing maintenance tasks.

## Handling encodings

Specify encodings to avoid surprises:

```ruby
File.open("input.txt", "r", encoding: "ISO-8859-1") do |file|
  text = file.read.encode("UTF-8")
end
```

When reading unknown input, use `Encoding::Converter` or `String#scrub` to handle invalid bytes gracefully.

## Temporary files and directories

```ruby
require "tempfile"

Tempfile.create("snapshot") do |tmp|
  tmp.write("intermediate data")
  tmp.flush
  puts "Stored at #{tmp.path}"
end  # temp file removed automatically
```

`Dir.mktmpdir` yields a temporary directory you can populate and clean up automatically.

## File locking

Prevent concurrent writers from corrupting data by locking.

```ruby
File.open("app.log", "a") do |file|
  file.flock(File::LOCK_EX)
  file.puts "[#{Process.pid}] #{message}"
  file.flush
  file.flock(File::LOCK_UN)
end
```

Use shared locks (`LOCK_SH`) for readers and exclusive locks (`LOCK_EX`) for writers.

## Working with directories

```ruby
Dir.entries("logs")             # => [".", "..", "today.log", ...]
Dir.glob("logs/**/*.log")       # recursive glob

Dir.mkdir("exports") unless Dir.exist?("exports")
```

`Pathname` (from the standard library) offers object-oriented path manipulation:

```ruby
require "pathname"

root = Pathname.new(__dir__)
log_path = root.join("logs", "app.log")
log_path.exist?
```

## Structured data: CSV, JSON, YAML

```ruby
require "csv"
CSV.open("users.csv", "w") do |csv|
  csv << %w[id email]
  users.each { |u| csv << [u.id, u.email] }
end

CSV.foreach("users.csv", headers: true) do |row|
  puts row["email"]
end
```

```ruby
require "json"

payload = { name: "Ada", skills: %w[ruby c] }
File.write("user.json", JSON.pretty_generate(payload))

data = JSON.parse(File.read("user.json"))
```

```ruby
require "yaml"
settings = { retries: 3, timeout: 5 }
File.write("config.yml", settings.to_yaml)

YAML.safe_load(File.read("config.yml"), symbolize_names: true)
```

Use `JSON.parse(..., symbolize_names: true)` or `YAML.safe_load` for controlled conversion. Avoid `YAML.load` on untrusted input—it's capable of instantiating arbitrary objects.

## Large files and streaming transforms

When processing huge files, combine IO streaming with enumerators:

```ruby
File.open("data.log", "r") do |file|
  file.lazy
      .reject { |line| line.start_with?("#") }
      .map(&:strip)
      .take(1000)
      .each { |entry| process(entry) }
end
```

`IO#each_byte`, `#readpartial`, and `#gets` give fine-grained control over buffering when implementing custom protocols.

## Error handling

Wrap file operations in begin/rescue blocks to handle missing files, permission issues, or encoding errors.

```ruby
begin
  File.open("config.yml", "r") { |f| load_config(f) }
rescue Errno::ENOENT
  warn "Config file missing; using defaults"
rescue Errno::EACCES
  warn "Insufficient permissions"
end
```

`Errno::EINVAL`, `IOError`, and `EOFError` cover other edge cases.

## Guided practice

1. **Log rotator**
   - Write a script that checks `app.log` size and moves it to `app-YYYYMMDD.log` when it exceeds 5 MB.
   - Use `FileUtils.mv` and ensure the new log file is created atomically.

2. **CSV aggregator**
   - Merge multiple CSV files in a directory into a single normalized file.
   - Preserve headers, skip duplicates, and stream line by line to avoid memory blow-ups.

3. **Binary inspector**
   - Read a binary file in chunks (`readpartial(1024)`) and print hex dumps using `String#bytes`.
   - Detect unexpected null bytes or control characters.

4. **Config loader**
   - Load settings from YAML with defaults. Provide `settings.fetch(:timeout, 5)` style access and raise friendly errors for missing required keys.

5. **Concurrent logger**
   - Implement a logging helper that acquires a lock, appends a timestamped message, and safely truncates the file when it exceeds a configurable limit.

## Self-check questions

1. When should you use `File.foreach` instead of `File.read`, and what trade-offs does each entail?
2. How do file modes (`"w"`, `"a"`, `"r+"`) differ, and what happens if you open a file in the wrong mode?
3. Why is it important to specify encodings when reading and writing text files, and how can you handle invalid byte sequences?
4. Which standard-library helpers (`Tempfile`, `FileUtils`, `Pathname`) simplify working with files and directories in larger scripts?
5. How does file locking prevent race conditions, and what are the differences between shared and exclusive locks?

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: File IO — Atomic Writes & Streaming (Appendix — file_io-ruby2)

Recipes for safely reading and writing files, streaming large inputs, and testing IO helpers.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Operation</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Atomic write</td><td>temp file + rename</td><td>reduces partial-write risk</td></tr>
    <tr><td>Streaming</td><td>File.foreach</td><td>memory efficient for large files</td></tr>
    <tr><td>Binary IO</td><td>rb mode</td><td>use for non-text files</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Atomic write example

```ruby
temp = "#{path}.tmp"
File.write(temp, data)
File.rename(temp, path)
```

### Streaming example

```ruby
File.foreach('large.csv') do |line|
  process(line)
end
```

### Exercises (Appendix — file_io-ruby2)

1. Implement an atomic writer helper and test that it replaces the target file only after a successful write (use `StringIO` and temp dirs in tests).
2. Build a streaming CSV reader that yields rows lazily and test it with a generated large string.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

With these tools, your Ruby scripts can ingest logs, export reports, and manage configuration safely. Combine streaming IO with structured data helpers, and you’ll be ready to automate real-world workflows with confidence.

<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

## Practical Appendix: File IO — Locking, Permissions & Robust Patterns (Appendix — file_io-ruby3)

Advanced patterns for file operations: advisory locking, correct permission handling, and robust error handling for production code.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Concern</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Locking</td><td>File#flock</td><td>Advisory locks; coordinate processes carefully</td></tr>
    <tr><td>Permissions</td><td>File.chmod/umask</td><td>Set explicit perms when creating sensitive files</td></tr>
    <tr><td>Retries</td><td>Exponential backoff</td><td>Useful for transient IO failures</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: advisory lock

```ruby
File.open('data.db', 'r+') do |f|
  f.flock(File::LOCK_EX)
  begin
    # read, modify, write
  ensure
    f.flock(File::LOCK_UN)
  end
end
```

### Safe permissions

```ruby
old_umask = File.umask(0)
File.open('secret.txt', File::CREAT|File::WRONLY, 0600) { |f| f.write(secret) }
ensure
  File.umask(old_umask)
end
```

### Robust streaming

- For large files, stream and process line-by-line with `File.foreach` or `IO#readpartial` for sockets.
- Use `Tempfile` for atomic constructs and ensure cleanup in `ensure`.

### Exercises (Appendix — file_io-ruby3)

1. Implement a small file-backed counter that uses `flock` to coordinate increments across processes; add tests using parallel processes or threads.
2. Write a helper that creates files with secure permissions (0600) and test that the created file has the expected mode.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

## Practical Appendix: File I/O — Streaming, Encoding & Atomic Writes (Appendix — file_io-ruby-appendix-20251005)

Practical guidance for safe file writes, streaming large files, and handling encodings in Ruby applications.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Task</th><th>Approach</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Stream read</td><td>File.foreach</td><td>Memory-safe</td></tr>
    <tr><td>Atomic write</td><td>Write temp + rename</td><td>Prevents partial writes</td></tr>
    <tr><td>Encoding</td><td>File.open('r:UTF-8')</td><td>Specify encodings explicitly</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: atomic write

```ruby
File.open('out.tmp','w') do |f|
  f.write(data)
end
File.rename('out.tmp', 'out.txt')
```

### Exercises (Appendix — file_io-ruby-appendix-20251005)

1. Implement atomic file writes and add tests that simulate partial-failure by raising during write.
2. Read a UTF-16 file safely and convert it to UTF-8.
