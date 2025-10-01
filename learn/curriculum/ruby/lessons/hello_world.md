# Hello World and Output

Welcome to your first Ruby lesson! In this lesson, you'll learn how to write your first Ruby program and understand the basics of displaying output.

## What is Ruby?

Ruby is a dynamic, object-oriented programming language created by Yukihiro Matsumoto (Matz) in the mid-1990s. It's designed to be simple and productive with an elegant syntax that is natural to read and easy to write.

> **Ruby Philosophy**: "Optimized for programmer happiness" - Matz

## Your First Ruby Program

The traditional first program in any language is "Hello, World!" - a simple program that displays this message on the screen.

In Ruby, it's incredibly simple:

```ruby
puts "Hello, World!"
```

That's it! Just one line of code.

## The `puts` Method

`puts` is short for "put string" - it outputs text to the terminal followed by a newline character.

### Basic Usage

```ruby
puts "Welcome to Ruby!"
puts "This is a new line"
```

**Output:**
```
Welcome to Ruby!
This is a new line
```

## The `print` Method

Ruby also has `print`, which works similarly to `puts` but **doesn't** add a newline:

```ruby
print "Hello"
print " World"
puts "!"
```

**Output:**
```
Hello World!
```

## The `p` Method

The `p` method is useful for debugging - it shows the "raw" form of objects:

```ruby
p "Hello"
p 42
p [1, 2, 3]
```

**Output:**
```
"Hello"
42
[1, 2, 3]
```

Notice how `p` shows the quotes around strings!

## Comments in Ruby

Comments help you document your code. Ruby ignores comments when running your program.

### Single-line comments

```ruby
# This is a single-line comment
puts "This will run"  # Comment after code
```

### Multi-line comments

```ruby
=begin
This is a multi-line comment.
You can write as much as you want here.
It won't be executed.
=end

puts "This will run"
```

## String Literals

You can use single quotes or double quotes for strings:

```ruby
puts 'Single quotes work'
puts "Double quotes work too"
```

**Difference**: Double quotes allow string interpolation (we'll cover this in the next lesson!).

## Key Takeaways

- `puts` displays output with a newline
- `print` displays output without a newline  
- `p` displays the raw representation of objects
- Use `#` for single-line comments
- Use `=begin` and `=end` for multi-line comments
- Strings can use single `'` or double `"` quotes

## Practice Time!

Now it's time to practice what you've learned. Click the button below to start the exercise.

### Exercise Goals

1. Display your name using `puts`
2. Use `print` to display multiple words on one line
3. Add comments to explain your code
4. Experiment with `p` to see how it differs from `puts`

> **Tip**: Don't be afraid to experiment! Ruby is forgiving and you can't break anything.

## What's Next?

In the next lesson, you'll learn about variables and different data types in Ruby. You'll discover how to store and manipulate data in your programs.

---

**Remember**: The best way to learn programming is by writing code. Complete the exercise and you'll be on your way to Ruby mastery! 🚀
