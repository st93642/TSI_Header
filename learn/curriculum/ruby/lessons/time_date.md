# Time and date handling

Scheduling jobs, storing timestamps, and formatting reports all rely on robust temporal logic. Ruby’s core library supplies `Time`, `Date`, and `DateTime`, while common gems add time zones, durations, and human-friendly formatting. This lesson helps you reason about temporal data accurately and efficiently.

## Learning goals

- Create `Time`, `Date`, and `DateTime` objects for specific instants or calendar days.
- Perform arithmetic, comparisons, and conversions between temporal classes.
- Format and parse timestamps with `strftime`, `iso8601`, and flexible parsing helpers.
- Understand time zones, UTC vs local time, and strategies for storing and displaying timestamps safely.
- Build utilities that compute durations, countdowns, and recurring schedules.

## Getting the current moment

```ruby
now = Time.now         # system local time with zone offset
utc_now = Time.now.utc # convert to UTC
Time.new               # alias for Time.now
```

`Time` stores seconds since the Unix epoch with microsecond precision. `Time.now` honors the system time zone; `Time.now.utc` or `Time.now.getutc` normalizes to UTC.

## Constructing specific times

```ruby
launch = Time.new(2025, 10, 3, 14, 30, 0, "+02:00")
#            year  month day hour min  sec offset

Time.local(2025, 10, 3, 14, 30) # uses system zone
Time.utc(2025, 10, 3, 12, 30)    # explicit UTC
```

Time constructors accept fractional seconds (`Time.new(2025, 10, 3, 14, 30, 0.123456)`) and custom offsets. Use UTC for persistence; convert to local time when presenting to users.

## Components and comparisons

```ruby
Time.new(2025, 10, 3, 14, 30).tap do |t|
  t.year    # => 2025
  t.month   # => 10
  t.day     # => 3
  t.hour    # => 14
  t.zone    # => "CEST"
  t.wday    # => 5 (Friday)
  t.yday    # => 276 (day of year)
end

Time.new(2025, 10, 3) > Time.new(2024, 12, 31)  # => true
```

`Time` objects compare chronologically; subtracting two times yields a floating-point number of seconds.

## Time arithmetic

```ruby
now = Time.now
in_an_hour  = now + 3600      # seconds
one_day_ago = now - 24 * 3600

future = Time.new(2026, 1, 1)
seconds_until_future = future - now
minutes = (seconds_until_future / 60).floor
```

For readable intervals, consider the `active_support` duration helpers (`1.hour`, `3.days`) when Rails or ActiveSupport is available.

## Formatting and parsing

```ruby
time = Time.new(2025, 10, 3, 14, 30, 45)

time.strftime("%Y-%m-%d %H:%M:%S") # => "2025-10-03 14:30:45"

time.iso8601            # => "2025-10-03T14:30:45+02:00"
Time.iso8601("2025-10-03T14:30:45+02:00") # parsing
```

Common `strftime` directives:

- `%Y` year, `%m` month, `%d` day
- `%H` 24-hour, `%I` 12-hour, `%M` minute, `%S` second
- `%A` weekday name, `%B` month name

Avoid custom parsers when built-in formatters suffice; ISO 8601 is a safe default for interchange.

## Dates without time

```ruby
require "date"

today = Date.today
Date.new(2025, 10, 3)   # calendar date
today.year              # => 2025

# Arithmetic (days)
tomorrow = today + 1
next_month = today >> 1      # shift by months
prev_week = today - 7

# Difference returns Rational days
days_old = today - Date.new(1990, 5, 15)  # => #<Rational 12959/1>
days_old.to_i                           # => 12959
```

`Date` handles calendar arithmetic accurately across leap years and transitions. Use it when the time of day is irrelevant (birthdays, billing cycles, reporting periods).

## Bridging `Time` and `Date`

```ruby
require "date"

Time.now.to_date             # => Date instance at local midnight
Date.today.to_time           # => local midnight as Time
Date.today.to_time(:utc)     # => UTC midnight
DateTime.parse("2025-10-03T14:30:00+02:00")
DateTime.now.new_offset(0)   # change offset without altering UTC instant
```

`DateTime` supports arbitrary time zones and astronomical calendars; prefer `Time` for most application code and convert to `Date` when you only need days.

## Time zones and best practices

- Store timestamps in UTC (`Time.now.utc`, database `TIMESTAMP WITH TIME ZONE`).
- Convert to the user’s zone for display. In Rails, `ActiveSupport::TimeZone["Europe/Riga"].at(time)` or `time.in_time_zone("Europe/Riga")`.
- Beware of daylight-saving transitions when adding hours or days; shifting by calendar units is safer with `Date` + custom logic or `ActiveSupport::Duration`.

## Durations and countdowns

```ruby
def countdown(to_time)
  seconds = (to_time - Time.now).to_i
  return "Elapsed" if seconds.negative?

  days, rem = seconds.divmod(86_400)
  hours, rem = rem.divmod(3600)
  minutes, seconds = rem.divmod(60)

  format("%dd %02dh %02dm %02ds", days, hours, minutes, seconds)
end

countdown(Time.now + 48 * 3600) # => "2d 00h 00m 00s"
```

Break seconds into human-readable intervals with `Integer#divmod`. For relative phrases (“in 3 days”), consider gems like `rails-i18n` or `distance_of_time_in_words` (Rails).

## Parsing flexible input

```ruby
require "date"
Date.parse("October 3, 2025")          # => Date
Date.parse("2025-10-03")               # => Date
Time.parse("2025-10-03 14:30")         # => Time in local zone
Time.strptime("03-10-2025", "%d-%m-%Y")
```

`Date.parse` and `Time.parse` accept many formats but rely on locale assumptions; prefer explicit formats with `strptime` when possible.

## Testing temporal code

Time-dependent logic is easier to test with clock injection or time-travel helpers:

```ruby
Time.stub :now, Time.new(2025, 10, 3, 12, 0, 0) do
  assert_equal "noon", schedule.current_slot
end
```

Gems like `timecop`, `active_support/testing/time_helpers`, or `travel_to` (Rails) provide richer APIs for freezing or traveling in time during tests.

## Guided practice

1. **Reminder scheduler**
   - Implement `next_occurrence(start_time:, interval_seconds:)` that returns the next future `Time` after `Time.now`.
   - Write tests that stub `Time.now` to verify edge cases.

2. **Countdown CLI**
   - Build a script that accepts a target ISO 8601 timestamp and prints the remaining days/hours/minutes each second until the event.
   - Handle past timestamps gracefully.

3. **Business hours checker**
   - Given a time zone and an interval (e.g., 09:00–17:00 Monday–Friday), return whether a given `Time` falls within business hours.
   - Carefully handle daylight-saving transitions by converting to the zone first.

4. **Monthly billing cycle**
   - Write `billing_period(date)` returning the start/end dates of the cycle (e.g., 15th to 14th of next month).
   - Use date arithmetic (`>>`, `<<`) and ensure leap years don’t break it.

5. **ISO formatter utility**
   - Create `format_timestamp(time, zone:)` that converts a `Time` to a specific time zone and returns a string like `"2025-10-03T14:30:00+03:00"`.
   - Guard against `nil` inputs and allow injecting a default zone.

## Self-check questions

1. Why is storing timestamps in UTC considered best practice, and how do you safely present them to users in different time zones?
2. What’s the difference between `Time`, `Date`, and `DateTime`, and when would you choose each?
3. How does `strftime` differ from `iso8601` for formatting, and when would you favor one over the other?
4. What pitfalls arise when adding hours across daylight-saving boundaries, and how can you avoid them?
5. How can stubbing or freezing `Time.now` improve the reliability of tests that depend on the current moment?

Temporal logic is notoriously tricky—plan for time zones, daylight saving, and user locale early. With Ruby’s time and date toolkit (supplemented by well-chosen gems), you can model schedules, countdowns, and recurring events with confidence.
