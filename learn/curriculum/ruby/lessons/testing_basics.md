# Testing basics with Minitest

Minitest ships with Ruby, giving you a lightweight but capable testing framework. Whether you prefer the classic xUnit style or a spec-driven DSL, Minitest keeps feedback fast, integrates easily with CI, and plays nicely with gems like `minitest-reporters` and `mocha`. This lesson lays the foundation for reliable, maintainable test suites.

## Learning goals

- Write test classes inheriting from `Minitest::Test`, organizing setup/teardown and assertions.
- Use the most common assertions, including value checks, type checks, and exception expectations.
- Structure suites with helpers, test doubles, and focused naming conventions.
- Run tests from the command line, filter by name, and seed random order to surface order dependencies.
- Extend Minitest with reporters, spec-style syntax, and custom assertions.

## Anatomy of a test case

```ruby
require "minitest/autorun"
require_relative "calculator"

class CalculatorTest < Minitest::Test
  def setup
    @calculator = Calculator.new
  end

  def test_adds_two_numbers
    assert_equal 5, @calculator.add(2, 3)
  end

  def teardown
    # optional cleanup (e.g., disconnect from DB)
  end
end
```

- `setup` runs before each test; `teardown` runs after.
- Test method names must start with `test_`; keep them descriptive and behavior-focused.
- `require "minitest/autorun"` bootstraps the test runner automatically when the file executes.

## Core assertions

```ruby
assert_equal expected, actual
refute_equal unexpected, actual
assert_nil value
refute_nil value
assert_in_delta 3.14, value, 0.01  # approximate equality
assert_includes collection, element
assert_raises(ArgumentError) { subject.call }
assert_output(/done/) { task.run }
```

A few favorites:

- `assert` / `refute`: truthiness checks.
- `assert_kind_of(klass, object)` / `assert_instance_of`.
- `assert_predicate(object, :valid?)` ensures predicate returns truthy.
- `assert_changes` (Rails) or custom helper: compare before/after states.
- `assert_raises` and `assert_throws` for exceptions and `throw`/`catch` control flow.

## Custom assertions

Encapsulate repeated patterns inside helper modules.

```ruby
module CustomAssertions
  def assert_even(number)
    assert number.even?, "Expected #{number} to be even"
  end
end

class NumberTest < Minitest::Test
  include CustomAssertions

  def test_even_number
    assert_even 4
  end
end
```

Keep assertion messages clear; Minitest displays them on failure.

## Organizing helpers

Use plain Ruby methods inside the test class or include modules for reusable helpers. Avoid global state—pass data explicitly or reset it in `setup`.

```ruby
class UserRepositoryTest < Minitest::Test
  def setup
    @repo = UserRepository.new
    seed_user!
  end

  def test_find_by_email
    assert_equal "ada@example.com", @repo.find_by_email("ada@example.com").email
  end

  private

  def seed_user!
    @repo.create(email: "ada@example.com")
  end
end
```

## Stubs and mocks

Minitest provides basic stubs; use `mocha` or Minitest’s mock helpers for richer behavior.

```ruby
require "minitest/mock"

def test_fetches_remote_data
  client = Minitest::Mock.new
  client.expect(:get, "OK", ["/status"])

  service = StatusService.new(client:)
  assert_equal "OK", service.fetch

  client.verify
end
```

For quick stubs:

```ruby
Time.stub :now, Time.new(2025, 10, 3, 12, 0, 0) do
  assert_equal 12, Scheduler.current_hour
end
```

`stub` replaces the method temporarily and restores it afterward.

## Running tests

From the shell:

```bash
ruby test/calculator_test.rb
```

Filter by name using `-n` (regexp allowed):

```bash
ruby test/calculator_test.rb -n test_adds_two_numbers
ruby test/calculator_test.rb -n /adds/
```

Randomize order to detect inter-test dependencies:

```bash
ruby test/calculator_test.rb --seed 12345
```

Integrate with `rake test` or bundler’s `bundle exec rake test` for project-wide runs.

## Spec style (optional)

Minitest also supports `describe` / `it` syntax via `minitest/spec`.

```ruby
require "minitest/autorun"
require "minitest/spec"

describe Calculator do
  let(:calculator) { Calculator.new }

  it "adds numbers" do
    _(calculator.add(1, 2)).must_equal 3
  end
end
```

- `_()` wraps values for expectations.
- `must_equal`, `wont_be_nil`, `must_raise`, etc., mirror assertion helpers.
- `let`, `before`, and `after` provide lazily evaluated helpers similar to RSpec.

Pick one style per project to keep consistency.

## Reporting and output

Enhance readability with reporters (e.g., `minitest-reporters`).

```ruby
require "minitest/reporters"
Minitest::Reporters.use! Minitest::Reporters::SpecReporter.new
```

Place reporter configuration in `test/test_helper.rb` so every test file requires the helper and picks up the same settings.

## Fixtures and factories

Minitest doesn’t mandate a factory library, but you can:

- Roll your own helper methods to build objects.
- Use gems like `factory_bot` with Minitest (`require "factory_bot"` in test helper and `include FactoryBot::Syntax::Methods`).
- Leverage YAML fixtures (`ActiveSupport::TestCase`) if you’re in Rails.

Keep test data small and explicit; prefer factories that produce minimal objects and customize per test (`build(:user, admin: true)`).

## Keeping tests fast and isolated

- Reset global state in `teardown` or use `around` helpers.
- Avoid network calls; stub external services.
- Use temporary directories (`Dir.mktmpdir`) and in-memory stores (e.g., `sqlite3` with `:memory:`) for integration tests.
- Run tests in random order (`Minitest::Test.i_suck_and_my_tests_are_order_dependent!` as a humorous reminder not to rely on order).

## Guided practice

1. **Prime tester**
   - Implement a simple `PrimeChecker` and write tests covering prime, composite, negative, and edge cases.
   - Use `assert`/`refute` along with descriptive failure messages.

2. **HTTP client mock**
   - Build a `WeatherClient` that calls `HTTP.get("/forecast")`.
   - Stub `HTTP.get` to return canned JSON and assert parsing behavior without hitting the network.

3. **Custom assertion**
   - Create `assert_valid(record)` that fails with `record.errors.full_messages.join(", ")` when validation fails.
   - Use it in tests covering valid and invalid cases.

4. **Spec-style rewrite**
   - Convert an existing xUnit-style test to spec style.
   - Identify pros/cons of each approach in the test comments.

5. **Seed debugging**
   - Introduce a deliberate order dependency between two tests.
   - Run with different seeds to expose the issue, then refactor to remove the dependency.

## Self-check questions

1. What’s the difference between `assert_equal`, `assert_same`, and `assert_in_delta`, and when would you choose each?
2. How does `setup`/`teardown` help keep tests isolated, and what should you avoid doing inside them?
3. When mocking collaborators, how do you verify expectations were met and reset stubs afterward?
4. Why might you run tests with a random seed, and how do you reproduce a failure triggered by randomness?
5. What trade-offs exist between Minitest’s spec style and the traditional test class style?

Minitest’s low ceremony encourages fast feedback. Keep tests small, deterministic, and expressive, and they’ll safeguard your codebase as it grows.

<!-- markdownlint-disable MD033 MD010 -->

### Practical Appendix: Test Helpers & CI

This appendix adds common test helpers, a GitHub Actions snippet to run tests, and an HTML table comparing assertions.

```yaml
name: Ruby tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Setup Ruby
        uses: ruby/setup-ruby@v1
        with:
          ruby-version: '3.2'
      - name: Install
        run: bundle install
      - name: Run tests
        run: bundle exec rake test
```

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Assertion</th><th>Purpose</th><th>Example</th></tr>
  </thead>
  <tbody>
    <tr><td>assert_equal</td><td>Equality</td><td>assert_equal 3, sum(1,2)</td></tr>
    <tr><td>assert_raises</td><td>Error expectation</td><td>assert_raises(ArgumentError) { call }</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises

1. Add a `test_helper.rb` that sets up `minitest/reporters` and requires support files.
2. Add a CI badge to the README and demonstrate failing tests in CI when appropriate.

<!-- markdownlint-enable MD010 -->
