# Ruby Curriculum Gap Analysis

## Current Coverage (22 lessons)

### ✅ Basics (7 lessons)

- Hello World with Puts
- String Concatenation  
- Using P for Debugging
- Hello World and Output
- Variables and Data Types
- Working with Strings
- Numbers and Math
# Create methods that return greetings

def say_hello
  # Return "Hello, World!"

end

def greet_person(name)
  # Return "Hello, {name}!"

end

def greet_with_time(name, time)
  # Return "Good {time}, {name}!"
  # Example: greet_with_time("Alice", "morning") => "Good morning, Alice!"

end

### ✅ Control Flow (3 lessons)

- If, Elsif, and Unless
- Loops and Iteration
- Case/When Statements

### ✅ Collections (3 lessons)

- Arrays and Methods
- Hashes and Symbols
- Advanced Iteration

### ✅ Methods and Blocks (3 lessons)

- Defining Methods
- Blocks and Procs
- Lambdas and Closures

### ✅ OOP (3 lessons)

- Classes and Objects
- Inheritance and Modules
- Mixins and Composition

### ✅ Advanced (3 lessons)

- Metaprogramming Basics
- File I/O and Serialization
- Exception Handling

## 🔴 Major Gaps to Fill

### Missing Core Topics

1. **Regular Expressions (Regex)** - Pattern matching, string validation
2. **Symbols vs Strings** - When to use each, memory implications
3. **Range Objects** - Creating and using ranges
4. **Time and Date** - Working with temporal data
5. **Enumerables Deep Dive** - each_with_index, reduce, select, reject, etc.
6. **Method Chaining** - Fluent interfaces

### Missing Intermediate Topics

7. **Attr Accessor/Reader/Writer** - Getter/setter shortcuts
8. **Class Methods vs Instance Methods** - self, @@, @
9. **Module Functions** - extend vs include
10. **Namespacing** - Organizing code with modules
11. **Constants** - CONSTANT naming and usage
12. **Operator Overloading** - Defining +, -, ==, etc.

### Missing Advanced Topics

13. **Testing with Minitest/RSpec** - TDD fundamentals
14. **Gems and Bundler** - Dependency management
15. **Ruby Standard Library** - JSON, CSV, Net::HTTP
16. **Threads and Concurrency** - Basic threading
17. **DSLs (Domain Specific Languages)** - Creating mini-languages
18. **Refinements** - Monkey patching safely

### Missing Practical Topics

19. **Input/Output Streams** - STDIN, STDOUT, STDERR
20. **Environment Variables** - ENV usage
21. **Command Line Arguments** - ARGV
22. **String Encoding** - UTF-8, ASCII
23. **Debugging Techniques** - binding.pry, byebug

## 🎯 Priority Additions (Next 10 lessons)

### High Priority (Must Have)

1. **Regular Expressions** - Essential for text processing
2. **Symbols** - Fundamental Ruby concept
3. **Range Objects** - Used everywhere
4. **Enumerables Advanced** - Core Ruby skill
5. **Attr Methods** - Clean OOP code

### Medium Priority (Should Have)

6. **Class vs Instance Methods** - Important OOP distinction  
7. **Time and Date** - Practical applications
8. **Testing Basics** - Professional development
9. **Input/Output** - User interaction
10. **Standard Library Intro** - JSON, CSV basics

### Low Priority (Nice to Have)

- Threads, DSLs, Refinements (more advanced)
- Gems/Bundler (environment-specific)
- Encoding (edge cases)

## 📝 Testing System Enhancement

### Current Limitation

- Tests only check return values
- Cannot test puts/print output
- Cannot verify side effects

### Solution: Capture Output

```ruby
# Wrap code execution and capture STDOUT
captured_output = StringIO.new
$stdout = captured_output
user_code_here
$stdout = STDOUT
output_text = captured_output.string
```

### New Test Types Needed

1. **Output Tests** - Check printed text
2. **Exception Tests** - Verify errors raised
3. **Side Effect Tests** - File creation, modifications
4. **Performance Tests** - Basic efficiency checks

## 🚀 Implementation Plan

### Phase 1: Core Gaps (Lessons 23-27)

- [ ] Regex lesson + exercise
- [ ] Symbols lesson + exercise
- [ ] Ranges lesson + exercise
- [ ] Enumerables Advanced lesson + exercise
- [ ] Attr Methods lesson + exercise

### Phase 2: OOP Enhancement (Lessons 28-30)

- [ ] Class vs Instance Methods lesson + exercise
- [ ] Operator Overloading lesson + exercise
- [ ] Module Namespacing lesson + exercise

### Phase 3: Practical Skills (Lessons 31-33)

- [ ] Time and Date lesson + exercise
- [ ] Input/Output lesson + exercise
- [ ] Testing Basics lesson + exercise

### Phase 4: Standard Library (Lessons 34-35)

- [ ] JSON Processing lesson + exercise
- [ ] CSV Handling lesson + exercise

### Phase 5: Testing Enhancement

- [ ] Implement output capture in ExerciseRunner
- [ ] Add output tests to relevant exercises
- [ ] Update exercise JSON schema for output tests

## 📊 Final Curriculum Stats

**Target:** 35 lessons across 8 modules
**Current:** 22 lessons across 6 modules
**To Add:** 13 lessons + 2 new modules

### New Modules Proposed

- **Module 7: Practical Ruby** (5 lessons)
  - Regular Expressions
  - Symbols and Constants
  - Range Objects
  - Time and Date
  - Input/Output

- **Module 8: Professional Ruby** (6 lessons)
  - Enumerables Advanced
  - Class and Instance Methods
  - Attr Accessors
  - Module Namespacing
  - Testing Basics
  - Standard Library (JSON/CSV)
