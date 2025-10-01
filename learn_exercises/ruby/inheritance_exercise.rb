# Task: Inheritance

class Animal
  attr_reader :name
  def initialize(name)
    @name = name
  end
end

class Cat < Animal
  def meow
    # Return "Meow! I'm {name}"
  end
end
