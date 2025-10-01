# Student Event Management System
# Build a practical event management system for TSI students

# You're creating a system to manage student events at TSI (Transport and Telecommunication Institute).
# This system needs to handle event scheduling, age verification, time calculations, and more.

require 'date'

# ===== PART 1: Current Time and Basic Operations =====

# Get the current time and store it in a variable called 'now'
# This will be used throughout the system


# Create a specific time for the TSI Winter Graduation ceremony:
# December 15, 2025 at 10:00 AM
# Store this in a variable called 'graduation_time'


# ===== PART 2: Student Age Verification =====

# Create a method to calculate a student's age
# calculate_age(birth_year, birth_month, birth_day)
# Should return the student's age in years
def calculate_age(birth_year, birth_month, birth_day)
  # Hint: Compare today's date with the birth date
  # Remember to check if birthday has passed this year
end

# Create a method to check if student is eligible for alumni events (21+)
# eligible_for_alumni_events?(birth_year, birth_month, birth_day)
def eligible_for_alumni_events?(birth_year, birth_month, birth_day)
  # Use your calculate_age method

end

# ===== PART 3: Event Scheduling and Countdown =====

# Create a method to calculate days until an event
# days_until_event(event_year, event_month, event_day)
# Should return number of days until the event
def days_until_event(event_year, event_month, event_day)
  # Calculate difference between event date and today
end

# Create a method to format event time for announcements
# format_event_announcement(time_obj)
# Should return: "Monday, December 15, 2025 at 10:00 AM"
def format_event_announcement(time_obj)
  # Use strftime to format the time nicely
end

# ===== PART 4: Study Session Scheduler =====

# Create a method to schedule next study session (3 days from now at 2 PM)
# next_study_session()
# Should return a Time object 3 days from now at 14:00 (2 PM)
def next_study_session
  # Add 3 days to current time and set hour to 14
end

# Create a method to check if it's during study hours (9 AM to 8 PM)
# during_study_hours?(time_obj)
def during_study_hours?(time_obj)
  # Check if hour is between 9 and 20 (8 PM)
end

# ===== PART 5: Academic Calendar =====

# Create a method to find the start of current academic year
# (September 1st of current year, or previous year if we're before September)
# academic_year_start()
def academic_year_start
  # Logic: if current month < 9, use previous year
  # Otherwise use current year
end

# Create a method to calculate weeks into semester
# weeks_into_semester()
# Should return how many weeks since academic year started
def weeks_into_semester
  # Calculate difference between now and academic_year_start
  # Convert to weeks (days / 7)
end

# ===== PART 6: International Student Time Zones =====

# Create a method to show event time in different time zones
# event_time_in_utc(local_time)
# Convert local time to UTC for international students
def event_time_in_utc(local_time)
  # Convert to UTC
end

# ===== TESTING EXAMPLES =====
# Uncomment these to test your methods:

# Student age examples
# puts "Student age: #{calculate_age(2003, 5, 15)} years old"
# puts "Eligible for alumni events: #{eligible_for_alumni_events(2002, 1, 1)}"

# Event scheduling examples
# puts "Days until graduation: #{days_until_event(2025, 12, 15)}"
# puts "Graduation announcement: #{format_event_announcement(graduation_time)}"

# Study session examples
# next_session = next_study_session
# puts "Next study session: #{format_event_announcement(next_session)}"
# puts "Currently study hours: #{during_study_hours?(Time.now)}"

# Academic calendar examples
# puts "Academic year started: #{academic_year_start}"
# puts "Weeks into semester: #{weeks_into_semester}"

# Time zone examples
# puts "Graduation in UTC: #{event_time_in_utc(graduation_time)}"