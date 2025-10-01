# Hashes: Key-Value Collections Exercise

# 1. Create a method create_person(name, age) that returns a hash
#    Example: create_person('Alice', 25) => {name: 'Alice', age: 25}
#    Example: create_person('Bob', 30) => {name: 'Bob', age: 30}
#    Hint: Return {name: name, age: age}

def create_person(name, age)
  # Return a hash with name and age keys

end

# 2. Create a method get_age(person) that returns age from hash
#    Example: get_age({name: 'Alice', age: 25}) => 25
#    Example: get_age({name: 'Bob', age: 30}) => 30
#    Hint: Access with person[:age]

def get_age(person)
  # Return the age value from the person hash

end

# 3. Create a method add_email(person, email) that adds email key
#    Example: add_email({name: 'Alice'}, 'a@test.com') => {name: 'Alice', email: 'a@test.com'}
#    Hint: Set person[:email] = email, then return person

def add_email(person, email)
  # Add email key to person hash and return it

end
