# File Streams and Serialization

## Lesson 10.1: File Streams and Serialization

Welcome to the final module of our C++ curriculum! By now, you've mastered variables, control flow, functions, classes, inheritance, polymorphism, templates, and exception handling. In this lesson, we'll explore **file input/output (I/O)** using C++ streams - the modern, type-safe way to read from and write to files.

File I/O is essential for real-world applications. Programs need to persist data between runs, read configuration files, process large datasets, and generate reports. C++ provides powerful stream-based I/O that handles different data types automatically and integrates seamlessly with your existing code.

### Why File I/O Matters

Consider these scenarios:

- A word processor saving documents (without file I/O, you'd lose your work every time you close the program)
- A game storing high scores and player progress (games need to remember your achievements between sessions)
- A scientific application analyzing experimental data (researchers need to save measurements for later analysis)
- A configuration system reading settings from disk (applications need to remember user preferences)

Without file I/O, your programs would be "amnesiac" - losing all data when they terminate. File operations bridge the gap between temporary memory (RAM) and permanent storage (hard drives, SSDs). This persistence is what makes software useful for real-world tasks.

**Key Insight**: Memory is volatile - it gets wiped when your program ends. Files provide permanent storage that survives program restarts, power outages, and system reboots.

### The Stream Concept

C++ uses the **stream abstraction** for I/O. A stream is a sequence of bytes that can be read from or written to. Think of a stream like a conveyor belt: data flows in one direction, and you can either put things on the belt (output) or take things off the belt (input).

Streams handle the complexity of different data sources and destinations:

- **Input streams** (`istream`) provide data **to** your program - like reading from a file or keyboard
- **Output streams** (`ostream`) receive data **from** your program - like writing to a file or screen
- **Bidirectional streams** (`iostream`) can do both - like a file you can read and write

**Why streams are powerful**: The same code works for files, console, strings, or even network connections. You write to a stream using `<<` or read from it using `>>`, and the stream handles all the details of where the data actually goes or comes from. This abstraction makes I/O code portable and reusable.

**Example**: `std::cout << "Hello"` and `file << "Hello"` use the same syntax, but one goes to the screen and one goes to a file. The stream abstraction hides these differences from you.

### File Streams Overview

C++ provides three main file stream classes in the `<fstream>` header:

```cpp
#include <fstream>

// For reading files
std::ifstream input_file;

// For writing files
std::ofstream output_file;

// For both reading and writing
std::fstream bidirectional_file;
```

These classes inherit from `std::istream`, `std::ostream`, and `std::iostream` respectively, so they work with all the standard I/O operations you're familiar with.

### Opening Files

Before using a file stream, you must **open** it. Opening a file establishes a connection between your program and the file on disk. The constructor can open the file directly:

```cpp
// Open for reading - file must exist
std::ifstream input("data.txt");

// Open for writing - creates file if it doesn't exist, overwrites if it does
std::ofstream output("results.txt");

// Open for both reading and writing - preserves existing file content
std::fstream both("config.dat", std::ios::in | std::ios::out);
```

You can also open files after construction:

```cpp
std::ofstream log_file;
log_file.open("application.log");  // Opens the file for writing
```

**Why check if opening succeeded?** File operations can fail for many reasons:

- File doesn't exist (when reading)
- No permission to access the file
- Disk is full (when writing)
- Path is invalid
- File is locked by another program

Always check if the file opened successfully:

```cpp
std::ifstream input("data.txt");
if (!input) {
    std::cerr << "Error: Could not open data.txt" << std::endl;
    std::cerr << "Possible reasons: file doesn't exist, no permission, invalid path" << std::endl;
    return 1;
}
```

**Key Point**: Never assume file operations will succeed. Always check for errors and handle them gracefully.

### File Modes

When opening files, you can specify **mode flags** to control behavior. These flags determine how the file is opened and what operations are allowed:

```cpp
// Common modes
std::ios::in     // Open for reading (default for ifstream)
std::ios::out    // Open for writing (default for ofstream)
std::ios::app    // Append to end of file (writes always go to the end)
std::ios::trunc  // Truncate file to zero length (default for out mode)
std::ios::binary // Binary mode (no text transformations)

// Combined modes using bitwise OR
std::ofstream file("data.bin", std::ios::out | std::ios::binary);
```

**Understanding the modes**:

- `in`: Allows reading. File must exist.
- `out`: Allows writing. Creates file if it doesn't exist.
- `app`: All writes go to the end of the file (useful for log files)
- `trunc`: Erases existing content when opening (dangerous!)
- `binary`: Prevents automatic newline conversions (Windows: CR+LF ↔ LF)

**Common combinations**:

- `std::ios::out | std::ios::trunc` (default for ofstream) - overwrite existing file
- `std::ios::out | std::ios::app` - append to existing file
- `std::ios::in | std::ios::out` - read and write existing file
- `std::ios::in | std::ios::out | std::ios::trunc` - read and write, but erase existing content

**Key Insight**: Mode flags are combined with `|` (bitwise OR). The wrong combination can destroy your data!

### Text File Operations

Let's start with text files, which store human-readable data. Text files are great for configuration files, logs, and data that humans need to read or edit.

#### Writing Text Files

```cpp
#include <iostream>
#include <fstream>
#include <string>

int main() {
    // Open file for writing
    std::ofstream output("shopping_list.txt");
    
    if (!output) {
        std::cerr << "Error opening file!" << std::endl;
        return 1;
    }
    
    // Write data using << operator (same as cout!)
    output << "Shopping List" << std::endl;
    output << "=============" << std::endl;
    output << "- Milk" << std::endl;
    output << "- Bread" << std::endl;
    output << "- Eggs" << std::endl;
    
    // File closes automatically when output goes out of scope
    // This is RAII in action!
    return 0;
}
```

**How it works**: The `<<` operator is overloaded for file streams, just like it is for `cout`. Each `<<` sends data to the file. `std::endl` adds a newline and flushes the buffer (ensures data is actually written to disk).

#### Reading Text Files

```cpp
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream input("shopping_list.txt");
    
    if (!input) {
        std::cerr << "Error opening file!" << std::endl;
        return 1;
    }
    
    std::string line;
    
    // Read line by line using getline
    while (std::getline(input, line)) {
        std::cout << "Read: " << line << std::endl;
    }
    
    return 0;
}
```

**Why getline?** `getline` reads an entire line (until newline character) into a string. This is safer than reading word-by-word with `>>` because it handles spaces in text properly. The loop continues until `getline` fails (end of file), which is the standard C++ idiom for reading files.

### Working with Different Data Types

Streams automatically handle type conversion, which is incredibly powerful. You can mix different data types in the same file:

```cpp
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

int main() {
    // Writing mixed data types
    std::ofstream out("data.txt");
    
    int age = 25;
    double height = 175.5;
    std::string name = "Alice";
    
    // Streams convert each type to its string representation
    out << name << " " << age << " " << height << std::endl;
    
    // Reading mixed data types
    std::ifstream in("data.txt");
    
    std::string read_name;
    int read_age;
    double read_height;
    
    // Streams automatically parse and convert back to the correct types
    in >> read_name >> read_age >> read_height;
    
    std::cout << "Name: " << read_name << std::endl;
    std::cout << "Age: " << read_age << std::endl;
    std::cout << "Height: " << read_height << std::endl;
    
    return 0;
}
```

**How type conversion works**:

- **Output (`<<`)**: Converts numbers to strings, writes strings as-is
- **Input (`>>`)**: Skips whitespace, parses strings into numbers
- **Formatting**: Uses default C++ formatting (no localization issues)

**Key Insight**: Streams handle the messy details of converting between binary data (how computers store numbers) and human-readable text. This makes file I/O much simpler than in languages like C.### Error Handling in File Operations

File operations can fail. Always check for errors:

```cpp
#include <iostream>
#include <fstream>

int main() {
    std::ofstream file("output.txt");
    
    if (!file) {
        std::cerr << "Failed to open output.txt" << std::endl;
        std::cerr << "Possible causes: no write permission, disk full, invalid path" << std::endl;
        return 1;
    }
    
    // Check for write errors after each operation
    file << "Hello, World!" << std::endl;
    
    if (!file) {
        std::cerr << "Write operation failed - disk might be full" << std::endl;
        return 1;
    }
    
    // Check if file is still in a good state
    if (file.good()) {
        std::cout << "File operations successful" << std::endl;
    }
    
    return 0;
}
```

**Why multiple error checks?** File operations can fail at different points:

- **Opening**: File doesn't exist, no permissions, path invalid
- **During I/O**: Disk full, network disconnect, file locked by another process
- **Closing**: May fail if buffer can't be flushed

**Stream state flags**:

- `good()`: All operations successful
- `bad()`: Irrecoverable error (file corrupted, etc.)
- `fail()`: Logical error (invalid format, etc.)
- `eof()`: End of file reached

**Key Insight**: Never assume file operations succeed. Robust programs check for errors and handle them gracefully.

### Binary File Operations

Text files are convenient but inefficient for large data or complex structures. Binary files store data exactly as it appears in memory.

#### Writing Binary Data

```cpp
#include <iostream>
#include <fstream>
#include <vector>

int main() {
    std::vector<int> numbers = {1, 2, 3, 4, 5, 100, 200, 300};
    
    // Open in binary mode - no text transformations
    std::ofstream file("numbers.bin", std::ios::binary);
    
    if (!file) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }
    
    // Write vector size first (metadata for reading)
    size_t size = numbers.size();
    file.write(reinterpret_cast<const char*>(&size), sizeof(size));
    
    // Write vector data as raw bytes
    file.write(reinterpret_cast<const char*>(numbers.data()), 
               size * sizeof(int));
    
    return 0;
}
```

**Why write size first?** When reading binary data, you need to know how much data to read. The size acts as metadata.

**What is `reinterpret_cast`?** Binary I/O requires treating data as raw bytes (`char*`). `reinterpret_cast` tells the compiler "trust me, I know what I'm doing" when converting between types.

#### Reading Binary Data

```cpp
#include <iostream>
#include <fstream>
#include <vector>

int main() {
    std::vector<int> numbers;
    
    std::ifstream file("numbers.bin", std::ios::binary);
    
    if (!file) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }
    
    // Read vector size first
    size_t size;
    file.read(reinterpret_cast<char*>(&size), sizeof(size));
    
    // Resize vector to hold the data
    numbers.resize(size);
    
    // Read vector data as raw bytes
    file.read(reinterpret_cast<char*>(numbers.data()), 
              size * sizeof(int));
    
    // Display results
    for (int num : numbers) {
        std::cout << num << " ";
    }
    std::cout << std::endl;
    
    return 0;
}
```

**Binary vs Text**:

- **Binary**: Exact memory representation, fast, compact, but platform-dependent
- **Text**: Human-readable, portable, slower due to conversion, larger files

**Key Insight**: Binary files are like photographs (exact reproduction), text files are like descriptions (can be translated).

### Serialization: Saving Objects to Files

**Serialization** is the process of converting objects to a format that can be stored and later reconstructed. C++ doesn't have built-in serialization, but we can implement it.

#### Simple Object Serialization

```cpp
#include <iostream>
#include <fstream>
#include <string>

class Person {
private:
    std::string name_;
    int age_;
    double salary_;
    
public:
    Person() = default;
    Person(const std::string& name, int age, double salary)
        : name_(name), age_(age), salary_(salary) {}
    
    // Serialization methods
    void serialize(std::ofstream& out) const {
        // Write string length first, then string data
        size_t name_len = name_.size();
        out.write(reinterpret_cast<const char*>(&name_len), sizeof(name_len));
        out.write(name_.c_str(), name_len);
        
        // Write primitive data
        out.write(reinterpret_cast<const char*>(&age_), sizeof(age_));
        out.write(reinterpret_cast<const char*>(&salary_), sizeof(salary_));
    }
    
    void deserialize(std::ifstream& in) {
        // Read string length, then string data
        size_t name_len;
        in.read(reinterpret_cast<char*>(&name_len), sizeof(name_len));
        
        name_.resize(name_len);
        in.read(&name_[0], name_len);
        
        // Read primitive data
        in.read(reinterpret_cast<char*>(&age_), sizeof(age_));
        in.read(reinterpret_cast<char*>(&salary_), sizeof(salary_));
    }
    
    void print() const {
        std::cout << "Name: " << name_ << std::endl;
        std::cout << "Age: " << age_ << std::endl;
        std::cout << "Salary: $" << salary_ << std::endl;
    }
};

int main() {
    // Create and serialize a person
    Person alice("Alice Johnson", 30, 75000.50);
    
    {
        std::ofstream file("person.dat", std::ios::binary);
        alice.serialize(file);
    }
    
    // Deserialize from file
    Person loaded_person;
    {
        std::ifstream file("person.dat", std::ios::binary);
        loaded_person.deserialize(file);
    }
    
    loaded_person.print();
    
    return 0;
}
```

**Why serialize manually?** C++ doesn't have built-in serialization like some languages (Java, Python). Manual control ensures efficiency and handles complex object relationships.

**String serialization pattern**: Always write length first, then data. This prevents buffer overflows when reading - you know exactly how much data to read.

**Binary vs Text Serialization**:

- **Binary**: Compact, fast, preserves exact data types, but platform-dependent and not human-readable
- **Text**: Human-readable, debuggable, works across platforms, but larger and slower due to parsing

**Real-world tip**: For production code, use libraries like Boost.Serialization, cereal, or nlohmann/json instead of manual implementation.

For more portable serialization, use text formats:

```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

class Person {
private:
    std::string name_;
    int age_;
    double salary_;
    
public:
    Person() = default;
    Person(const std::string& name, int age, double salary)
        : name_(name), age_(age), salary_(salary) {}
    
    // Text serialization
    std::string serialize_text() const {
        std::ostringstream oss;
        oss << "Person{" << std::endl;
        oss << "  name: \"" << name_ << "\"," << std::endl;
        oss << "  age: " << age_ << "," << std::endl;
        oss << "  salary: " << salary_ << std::endl;
        oss << "}" << std::endl;
        return oss.str();
    }
    
    void deserialize_text(std::istream& in) {
        std::string line;
        
        // Skip "Person{"
        std::getline(in, line);
        
        // Read name
        std::getline(in, line);
        // Parse "  name: "Alice Johnson","
        size_t start = line.find('"');
        size_t end = line.rfind('"');
        if (start != std::string::npos && end != std::string::npos) {
            name_ = line.substr(start + 1, end - start - 1);
        }
        
        // Read age
        std::getline(in, line);
        // Parse "  age: 30,"
        size_t colon_pos = line.find(':');
        if (colon_pos != std::string::npos) {
            std::string age_str = line.substr(colon_pos + 1);
            // Remove comma
            if (!age_str.empty() && age_str.back() == ',') {
                age_str.pop_back();
            }
            age_ = std::stoi(age_str);
        }
        
        // Read salary
        std::getline(in, line);
        // Parse "  salary: 75000.5"
        colon_pos = line.find(':');
        if (colon_pos != std::string::npos) {
            std::string salary_str = line.substr(colon_pos + 1);
            salary_ = std::stod(salary_str);
        }
        
        // Skip closing brace
        std::getline(in, line);
    }
    
    void print() const {
        std::cout << "Name: " << name_ << std::endl;
        std::cout << "Age: " << age_ << std::endl;
        std::cout << "Salary: $" << salary_ << std::endl;
    }
};

int main() {
    Person alice("Alice Johnson", 30, 75000.50);
    
    // Serialize to text file
    {
        std::ofstream file("person.txt");
        file << alice.serialize_text();
    }
    
    // Deserialize from text file
    Person loaded_person;
    {
        std::ifstream file("person.txt");
        loaded_person.deserialize_text(file);
    }
    
    loaded_person.print();
    
    return 0;
}
```

**Text serialization advantages**: Human-readable format makes debugging easier. You can open the file in any text editor to see what's stored. Also works across different platforms and programming languages.

**Parsing challenges**: Manual text parsing is error-prone. The example above is simplified - real JSON parsing requires handling escaped characters, nested structures, etc.

**When to use text vs binary**:

- **Use text for**: Configuration files, human-editable data, cross-platform compatibility, debugging
- **Use binary for**: Performance-critical applications, large datasets, internal application data

For large files, you might need to jump to specific positions:

```cpp
#include <iostream>
#include <fstream>

int main() {
    // Create a file with numbered lines
    {
        std::ofstream out("lines.txt");
        for (int i = 1; i <= 10; ++i) {
            out << "Line " << i << ": This is line number " << i << std::endl;
        }
    }
    
    std::fstream file("lines.txt");
    
    if (!file) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }
    
    // Move to position 100 (approximately line 5)
    file.seekg(100);
    
    std::string line;
    std::getline(file, line);
    std::cout << "Line at position 100: " << line << std::endl;
    
    // Get current position
    std::streampos current_pos = file.tellg();
    std::cout << "Current position: " << current_pos << std::endl;
    
    // Move to beginning
    file.seekg(0, std::ios::beg);
    
    // Move to end
    file.seekg(0, std::ios::end);
    std::streampos file_size = file.tellg();
    std::cout << "File size: " << file_size << " bytes" << std::endl;
    
    return 0;
}
```

### Working with Directories

C++17 introduced filesystem operations:

```cpp
#include <iostream>
#include <filesystem>
#include <fstream>

namespace fs = std::filesystem;

int main() {
    // Create directory
    fs::create_directory("data");
    
    // Check if path exists
    if (fs::exists("data/numbers.txt")) {
        std::cout << "File exists" << std::endl;
    }
    
    // Get file size
    if (fs::exists("data/numbers.txt")) {
        std::uintmax_t size = fs::file_size("data/numbers.txt");
        std::cout << "File size: " << size << " bytes" << std::endl;
    }
    
    // List directory contents
    for (const auto& entry : fs::directory_iterator("data")) {
        std::cout << entry.path() << std::endl;
    }
    
    return 0;
}
```

### Best Practices for File I/O

1. **Always check file operations**: Test `if (!file)` after opening and operations.

2. **Use RAII**: Let file streams close automatically when they go out of scope.

3. **Handle exceptions**: File operations can throw exceptions in some cases.

4. **Prefer text for portability**: Text files work across platforms; binary files may not.

5. **Consider file locking**: For multi-process applications, prevent concurrent access issues.

6. **Validate data**: When reading, verify data integrity and handle corrupted files.

7. **Use appropriate modes**: Choose text/binary and read/write modes carefully.

### Common File I/O Patterns

#### Configuration Files

```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>

class Config {
private:
    std::unordered_map<std::string, std::string> settings_;
    
public:
    bool load(const std::string& filename) {
        std::ifstream file(filename);
        if (!file) return false;
        
        std::string line;
        while (std::getline(file, line)) {
            // Skip comments and empty lines
            if (line.empty() || line[0] == '#') continue;
            
            size_t equals_pos = line.find('=');
            if (equals_pos != std::string::npos) {
                std::string key = line.substr(0, equals_pos);
                std::string value = line.substr(equals_pos + 1);
                
                // Trim whitespace
                key.erase(0, key.find_first_not_of(" \t"));
                key.erase(key.find_last_not_of(" \t") + 1);
                value.erase(0, value.find_first_not_of(" \t"));
                value.erase(value.find_last_not_of(" \t") + 1);
                
                settings_[key] = value;
            }
        }
        
        return true;
    }
    
    std::string get(const std::string& key, const std::string& default_value = "") const {
        auto it = settings_.find(key);
        return it != settings_.end() ? it->second : default_value;
    }
};

int main() {
    Config config;
    if (config.load("app.config")) {
        std::string theme = config.get("theme", "default");
        int font_size = std::stoi(config.get("font_size", "12"));
        
        std::cout << "Theme: " << theme << std::endl;
        std::cout << "Font size: " << font_size << std::endl;
    }
    
    return 0;
}
```

#### Data Logging

```cpp
#include <iostream>
#include <fstream>
#include <chrono>
#include <iomanip>

class Logger {
private:
    std::ofstream log_file_;
    
public:
    Logger(const std::string& filename) {
        log_file_.open(filename, std::ios::app);
        if (!log_file_) {
            throw std::runtime_error("Could not open log file");
        }
    }
    
    void log(const std::string& message) {
        auto now = std::chrono::system_clock::now();
        auto time_t = std::chrono::system_clock::to_time_t(now);
        
        log_file_ << std::put_time(std::localtime(&time_t), "%Y-%m-%d %H:%M:%S")
                  << " - " << message << std::endl;
    }
};

int main() {
    Logger logger("app.log");
    
    logger.log("Application started");
    logger.log("Processing data...");
    logger.log("Operation completed successfully");
    
    return 0;
}
```

### Practice Time

Now it's your turn to practice file I/O! Complete the following exercises:

1. **Basic File Writer**: Create a program that writes user input to a file until "quit" is entered.

2. **File Statistics**: Write a program that reads a text file and reports:
   - Number of lines
   - Number of words
   - Number of characters

3. **Student Database**: Create a program that can:
   - Add student records (name, ID, grades) to a file
   - Read and display all student records
   - Search for a student by ID

4. **Binary Data Storage**: Implement a simple database that stores Person objects in binary format with CRUD operations.

5. **File Merger**: Create a program that merges multiple text files into one, removing duplicate lines.

Remember to handle errors gracefully and close files properly. Use the techniques you've learned about streams, serialization, and file positioning.

### Self-Check Questions

1. What are the three main file stream classes in C++?
2. How do you check if a file opened successfully?
3. What's the difference between text and binary file modes?
4. Why is serialization important for object persistence?
5. How can you move to a specific position in a file?
6. What are the advantages of text-based serialization over binary?
7. How does RAII apply to file operations?
8. What file modes would you use for appending to an existing file?
9. How can you determine the size of a file before reading it?
10. Why should you validate data when reading from files?

### Key Takeaways

- **Streams** provide a unified interface for I/O operations
- **File streams** (`ifstream`, `ofstream`, `fstream`) handle file operations
- **Text mode** is portable but less efficient; **binary mode** is efficient but platform-dependent
- **Serialization** converts objects to storable formats
- **Error handling** is crucial for robust file operations
- **RAII** ensures files are properly closed
- **File positioning** enables random access to large files

Congratulations! You've completed the C++ curriculum. You now have the skills to write professional C++ applications that can handle complex data structures, object-oriented design, generic programming, error management, and persistent storage. Keep practicing and building projects to reinforce these concepts!
