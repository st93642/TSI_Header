# Blocks and Procs Exercise

# 1. Create a method apply_to_five(&block) that calls block with 5
#    Example: apply_to_five { |x| x * 2 } => 10
#    Example: apply_to_five { |x| x + 10 } => 15
#    Hint: Use block.call(5) or yield 5

def apply_to_five(&block)
  # Call the block with number 5 as argument

end

# 2. Create a method make_adder(n) that returns a Proc adding n
#    Example: adder = make_adder(5); adder.call(3) => 8
#    Example: adder = make_adder(10); adder.call(5) => 15
#    Hint: Proc.new { |x| x + n }

def make_adder(n)
  # Return a Proc that adds n to its argument

end

# 3. Create a method custom_select(arr, &block) that filters array
#    Example: custom_select([1,2,3,4]) { |n| n > 2 } => [3, 4]
#    Example: custom_select([1,2,3,4]) { |n| n.even? } => [2, 4]
#    Hint: Use arr.select(&block)

def custom_select(arr, &block)
  # Filter array using the provided block

end
