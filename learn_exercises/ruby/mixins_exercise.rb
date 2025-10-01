# Task: Mixins

module Greetable
  def greet
    "Hello from this class"
  end
end

class Person
  include Greetable
end

class Robot
  include Greetable
end
