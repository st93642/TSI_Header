# Student Event Management System

Time and date handling are fundamental concepts in programming. Ruby provides powerful built-in classes for working with temporal data. The `Time` class handles moments in time with microsecond precision, while the `Date` class focuses on calendar dates without time components.

## Getting the Current Time

The most common way to work with time is to get the current moment:

```ruby
# Current time
now = Time.now
puts now  # => 2025-10-01 14:30:45 +0200

# Alternative syntax
current_time = Time.new
puts current_time  # => 2025-10-01 14:30:45 +0200
```

## Creating Specific Times

You can create Time objects for specific moments:

```ruby
# Create a time for a specific date and time
birthday = Time.new(1990, 12, 25, 8, 30, 0)  # Christmas morning 1990
puts birthday  # => 1990-12-25 08:30:00 +0100

# Just specify year and month
new_year = Time.new(2025, 1, 1)  # New Year's Day 2025
puts new_year  # => 2025-01-01 00:00:00 +0100

# Local time (uses system timezone)
meeting = Time.local(2025, 10, 15, 14, 0)  # 2 PM local time
puts meeting  # => 2025-10-15 14:00:00 +0200

# UTC time
utc_time = Time.utc(2025, 10, 15, 12, 0)  # Noon UTC
puts utc_time  # => 2025-10-15 12:00:00 UTC
```

## Time Components

Time objects have many methods to access individual components:

```ruby
t = Time.new(2025, 10, 1, 15, 30, 45)

puts t.year    # => 2025
puts t.month   # => 10
puts t.day     # => 1
puts t.hour    # => 15
puts t.min     # => 30
puts t.sec     # => 45

puts t.wday    # => 2 (0 = Sunday, 1 = Monday, ..., 6 = Saturday)
puts t.yday    # => 274 (day of year, 1-366)
puts t.zone    # => "CEST" (timezone abbreviation)
```

## Time Arithmetic

You can add and subtract seconds from Time objects:

```ruby
now = Time.now
puts "Now: #{now}"

# Add seconds
one_hour_later = now + 3600  # 3600 seconds = 1 hour
puts "In 1 hour: #{one_hour_later}"

# Subtract seconds
one_hour_ago = now - 3600
puts "1 hour ago: #{one_hour_ago}"

# Time difference
future_time = Time.new(2026, 1, 1)
seconds_until_then = future_time - now
days_until_then = seconds_until_then / (24 * 60 * 60)
puts "Days until 2026: #{days_until_then.to_i}"
```

## Formatting Time

The `strftime` method formats time as strings:

```ruby
t = Time.new(2025, 10, 1, 15, 30, 45)

# Common formats
puts t.strftime("%Y-%m-%d")     # => "2025-10-01"
puts t.strftime("%H:%M:%S")     # => "15:30:45"
puts t.strftime("%A, %B %d")    # => "Tuesday, October 01"

# Complete date and time
puts t.strftime("%Y-%m-%d %H:%M:%S")  # => "2025-10-01 15:30:45"

# 12-hour format
puts t.strftime("%I:%M %p")     # => "03:30 PM"
```

Common `strftime` directives:

- `%Y` - Year with century (2025)
- `%m` - Month (01-12)
- `%d` - Day of month (01-31)
- `%H` - Hour (00-23)
- `%M` - Minute (00-59)
- `%S` - Second (00-60)
- `%A` - Full weekday name
- `%B` - Full month name
- `%I` - Hour (01-12, 12-hour clock)
- `%p` - AM/PM

## The Date Class

The `Date` class works with calendar dates without time:

```ruby
require 'date'

# Current date
today = Date.today
puts today  # => 2025-10-01

# Specific date
independence_day = Date.new(1776, 7, 4)
puts independence_day  # => 1776-07-04

# Date components
puts today.year   # => 2025
puts today.month  # => 10
puts today.day    # => 1
puts today.wday   # => 2 (0 = Sunday)
```

## Converting Between Time and Date

```ruby
require 'date'

# Time to Date
now = Time.now
today = now.to_date
puts today  # => 2025-10-01

# Date to Time (midnight in local timezone)
some_date = Date.new(2025, 12, 25)
christmas_time = some_date.to_time
puts christmas_time  # => 2025-12-25 00:00:00 +0100
```

## Date Arithmetic

```ruby
require 'date'

today = Date.today
puts "Today: #{today}"

# Add days
tomorrow = today + 1
puts "Tomorrow: #{tomorrow}"

next_week = today + 7
puts "Next week: #{next_week}"

# Add months
next_month = today >> 1  # >> shifts by months
puts "Next month: #{next_month}"

# Add years
next_year = today >> 12
puts "Next year: #{next_year}"

# Date differences
birthday = Date.new(1990, 5, 15)
age_in_days = today - birthday
puts "Age in days: #{age_in_days.to_i}"
```

## Practical Examples

### Age Calculator

```ruby
require 'date'

def calculate_age(birth_year, birth_month, birth_day)
  today = Date.today
  birthday = Date.new(birth_year, birth_month, birth_day)

  age = today.year - birthday.year
  age -= 1 if today < birthday + age  # Haven't had birthday yet this year

  age
end

puts calculate_age(1990, 5, 15)  # => 35 (in 2025)
```

### Time Until Event

```ruby
def time_until(target_year, target_month, target_day)
  now = Time.now
  target = Time.new(target_year, target_month, target_day)

  if target > now
    seconds = target - now
    days = (seconds / (24 * 60 * 60)).to_i
    hours = (seconds / (60 * 60) % 24).to_i
    minutes = (seconds / 60 % 60).to_i

    "#{days} days, #{hours} hours, #{minutes} minutes"
  else
    "Event has already passed"
  end
end

puts time_until(2025, 12, 25)  # => "54 days, 9 hours, 29 minutes"
```

## Key Takeaways

- `Time.now` gets the current moment
- `Time.new(year, month, day, hour, min, sec)` creates specific times
- Use `strftime` to format times as strings
- `Date.today` gets today's date
- `Date.new(year, month, day)` creates specific dates
- Time arithmetic uses seconds, Date arithmetic uses days
- Convert between Time and Date with `to_date` and `to_time`

## Practice Time

Now it's time to practice working with time and date. Click the button below to start the exercise.

### Exercise Goals

1. Create and manipulate Time objects
2. Format times using strftime
3. Perform time and date arithmetic
4. Convert between Time and Date objects
5. Build practical time-based calculations

> **Tip**: Always consider timezones when working with times. Use UTC for storage and convert to local time for display.

## What's Next

In the next lesson, you'll learn about working with strings - Ruby's flexible text manipulation capabilities. You'll discover concatenation, interpolation, and powerful string methods.

---

**Remember**: Time flies when you're programming! 🕐✨
