# Create methods that use puts to display output

def say_hello
  # Use puts to print "Hello, World!"
  puts "Hello, World!"
end

def greet_person(name)
  # Use puts to print "Hello, {name}!"
  # Example: greet_person("Alice") should print "Hello, Alice!"
  puts "Hello, #{name}!"
end

def greet_with_time(name, time)
  # Use puts to print "Good {time}, {name}!"
  # Example: greet_with_time("Alice", "morning") should print "Good morning, Alice!"
  puts "Good #{time}, #{name}!"
end
