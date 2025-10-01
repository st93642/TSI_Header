# Define a Dog class with the following:
# - initialize method that takes name and breed
# - bark method that returns "Woof! I'm {name}!"
# - description method that returns "{name} is a {breed}"

class Dog
  # Your code here
  def initialize(name, breed)
    @name = name
    @breed = breed
  end
  def bark
    "Woof! I'm #{@name}!"
  end
  def description
    "#{@name} is a #{@breed}"
  end
end
