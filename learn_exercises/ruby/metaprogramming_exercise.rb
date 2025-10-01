# Task: Dynamic methods

class DynamicGreeter
  [:hello, :hi, :hey].each do |method_name|
    define_method(method_name) do
      # Return greeting + " there!"
    end
  end
end
