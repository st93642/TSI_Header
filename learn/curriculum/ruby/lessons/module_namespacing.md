# Lesson 8.5: Module Namespacing

## Overview

Modules in Ruby serve two main purposes: as **mixins** (adding behavior to classes) and as **namespaces** (organizing code and preventing name conflicts). Namespacing allows you to group related classes, constants, and methods under a common module name.

## Basic Namespacing

```ruby
module Graphics
  class Circle
    def draw
      "Drawing a circle"
    end
  end

  class Square
    def draw
      "Drawing a square"
    end
  end
end

# Access classes through the module namespace
circle = Graphics::Circle.new
square = Graphics::Square.new

puts circle.draw  # => "Drawing a circle"
puts square.draw  # => "Drawing a square"
```

## Constants in Namespaces

```ruby
module MathConstants
  PI = 3.14159
  E = 2.71828

  module Trigonometry
    def self.sin(x)
      # Simplified implementation
      x - (x**3)/6
    end
  end
end

puts MathConstants::PI        # => 3.14159
puts MathConstants::E         # => 2.71828
puts MathConstants::Trigonometry.sin(0.5)  # => 0.479166
```

## Nested Modules

```ruby
module Company
  module HR
    class Employee
      attr_reader :name, :department

      def initialize(name, department)
        @name = name
        @department = department
      end
    end

    module Payroll
      def self.calculate_salary(employee)
        base_salary = 50000
        department_bonus = employee.department == "Engineering" ? 10000 : 0
        base_salary + department_bonus
      end
    end
  end

  module IT
    class Server
      def initialize(name)
        @name = name
      end
    end
  end
end

employee = Company::HR::Employee.new("Alice", "Engineering")
puts Company::HR::Payroll.calculate_salary(employee)  # => 60000

server = Company::IT::Server.new("web-server-01")
```

## Avoiding Name Conflicts

Namespacing prevents conflicts between similarly named classes:

```ruby
module Blog
  class Post
    def initialize(title)
      @title = title
    end

    def publish
      "Published blog post: #{@title}"
    end
  end
end

module SocialMedia
  class Post
    def initialize(content)
      @content = content
    end

    def publish
      "Posted to social media: #{@content}"
    end
  end
end

# No conflict between these classes
blog_post = Blog::Post.new("Ruby Namespacing")
social_post = SocialMedia::Post.new("Check out this Ruby tip!")

puts blog_post.publish
puts social_post.publish
```

## Module Functions as Namespaces

```ruby
module StringUtils
  def self.capitalize_words(text)
    text.split.map(&:capitalize).join(' ')
  end

  def self.truncate(text, length)
    text.length > length ? text[0...length] + "..." : text
  end

  def self.slugify(text)
    text.downcase.gsub(/[^a-z0-9]+/, '-').gsub(/^-|-$/, '')
  end
end

puts StringUtils.capitalize_words("hello world")  # => "Hello World"
puts StringUtils.truncate("This is a long text", 10)  # => "This is a ..."
puts StringUtils.slugify("Hello, World!")  # => "hello-world"
```

## Including Modules for Mixins

Modules can be both namespaces and mixins:

```ruby
module Database
  module Connection
    def connect(url)
      @connection = "Connected to #{url}"
    end

    def disconnect
      @connection = nil
    end

    def connected?
      !@connection.nil?
    end
  end

  class PostgreSQL
    include Connection

    def initialize(url)
      connect(url)
    end
  end

  class MySQL
    include Connection

    def initialize(url)
      connect(url)
    end
  end
end

pg = Database::PostgreSQL.new("postgresql://localhost/mydb")
mysql = Database::MySQL.new("mysql://localhost/mydb")

puts pg.connected?     # => true
puts mysql.connected?  # => true
```

## Organizing Large Codebases

```ruby
module Ecommerce
  module Models
    class Product
      attr_reader :name, :price

      def initialize(name, price)
        @name = name
        @price = price
      end
    end

    class Order
      def initialize
        @items = []
      end

      def add_item(product, quantity = 1)
        @items << { product: product, quantity: quantity }
      end
    end
  end

  module Services
    class PaymentProcessor
      def process(order)
        "Processing payment for order with #{order.items.size} items"
      end
    end

    class ShippingCalculator
      def calculate(order)
        base_cost = 5.99
        item_cost = order.items.size * 2.50
        base_cost + item_cost
      end
    end
  end

  module Controllers
    class OrdersController
      def create
        product = Models::Product.new("Laptop", 999.99)
        order = Models::Order.new
        order.add_item(product)

        payment_result = Services::PaymentProcessor.new.process(order)
        shipping_cost = Services::ShippingCalculator.new.calculate(order)

        "Order created: #{payment_result}, Shipping: $#{shipping_cost}"
      end
    end
  end
end

controller = Ecommerce::Controllers::OrdersController.new
puts controller.create
```

## Requiring Namespaced Code

```ruby
# In file: lib/my_gem.rb
module MyGem
  VERSION = "1.0.0"

  class Client
    def initialize(api_key)
      @api_key = api_key
    end

    def fetch_data
      "Data fetched with API key: #{@api_key}"
    end
  end
end

# Usage
require 'my_gem'

client = MyGem::Client.new("secret-key")
puts client.fetch_data
```

## Best Practices

1. **Use consistent naming**: Module names should be descriptive and follow Ruby conventions
2. **Avoid deep nesting**: Don't nest modules more than 2-3 levels deep
3. **Group related functionality**: Keep related classes and modules together
4. **Use :: for access**: Prefer `Module::Class` over including everything
5. **Document your namespaces**: Make module purposes clear

```ruby
# Good: Clear namespace structure
module API
  module V1
    class UsersController; end
    class PostsController; end
  end

  module V2
    class UsersController; end
    class PostsController; end
  end
end

# Access specific versions
users_v1 = API::V1::UsersController.new
users_v2 = API::V2::UsersController.new
```

## Common Patterns

### Service Objects Pattern

```ruby
module Services
  class UserRegistration
    def self.call(user_params)
      user = Models::User.new(user_params)
      if user.valid?
        Repositories::UserRepository.save(user)
        Notifications::EmailService.send_welcome(user)
        { success: true, user: user }
      else
        { success: false, errors: user.errors }
      end
    end
  end
end

# Usage
result = Services::UserRegistration.call(name: "Alice", email: "alice@example.com")
```

### Configuration Pattern

```ruby
module MyApp
  module Config
    DATABASE_URL = ENV.fetch('DATABASE_URL', 'sqlite://db.sqlite3')
    REDIS_URL = ENV.fetch('REDIS_URL', 'redis://localhost:6379')

    module Development
      DEBUG = true
      LOG_LEVEL = :debug
    end

    module Production
      DEBUG = false
      LOG_LEVEL = :warn
    end
  end
end

# Usage
puts MyApp::Config::DATABASE_URL
puts MyApp::Config::Development::DEBUG
```

Modules provide powerful namespacing capabilities that help organize large Ruby applications and prevent naming conflicts. They create clear boundaries and make code more maintainable and understandable.
