# Testing basics with Minitest

## Practical Appendix: Tests — CI & Fixtures (Appendix — testing-basics-playbook)

Compact CI tips and fixture/factory guidance to keep suites fast and reliable.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Pattern</th><th>Reason</th></tr>
  </thead>
  <tbody>
    <tr><td>CI</td><td>Run fast tests on PRs, slow/integration nightly</td><td>Keeps PR feedback quick</td></tr>
    <tr><td>Fixtures</td><td>Use factories for flexible data</td><td>Reduces brittle test data</td></tr>
    <tr><td>Mocks</td><td>Expectations over stubbing where interactions matter</td><td>Verifies behaviour contracts</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Exercises

1. Add a `test_helper.rb` that configures reporters and common helpers; update
   one test to require it.
2. Replace a static fixture with a small factory and show the differences in
   test readability.

### Self-check

1. How do you make tests that rely on time deterministic? (Hint: `Time.stub`)
2. When should you prefer mocks to real integration tests?

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

Minitest ships with Ruby, giving you a lightweight but capable testing framework. Whether you prefer the classic xUnit style or a spec-driven DSL, Minitest keeps feedback fast, integrates easily with CI, and plays nicely with gems like `minitest-reporters` and `mocha`. This lesson lays the foundation for reliable, maintainable test suites.

## Learning goals

- Write test classes inheriting from `Minitest::Test`, organizing setup/teardown
  and assertions.
- Use the most common assertions, including value checks, type checks, and
  exception expectations.
- Structure suites with helpers, test doubles, and focused naming conventions.
- Run tests from the command line, filter by name, and seed random order to
  surface order dependencies.
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
- Test method names must start with `test_`; keep them descriptive and
  behavior-focused.
- `require "minitest/autorun"` bootstraps the test runner automatically when the
  file executes.

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
- `assert_raises` and `assert_throws` for exceptions and `throw`/`catch` control
  flow.

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
- `let`, `before`, and `after` provide lazily evaluated helpers similar to
  RSpec.

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
- Use gems like `factory_bot` with Minitest (`require "factory_bot"` in test
  helper and `include FactoryBot::Syntax::Methods`).
- Leverage YAML fixtures (`ActiveSupport::TestCase`) if you’re in Rails.

Keep test data small and explicit; prefer factories that produce minimal objects and customize per test (`build(:user, admin: true)`).

## Keeping tests fast and isolated

- Reset global state in `teardown` or use `around` helpers.
- Avoid network calls; stub external services.
- Use temporary directories (`Dir.mktmpdir`) and in-memory stores (e.g.,
  `sqlite3` with `:memory:`) for integration tests.
- Run tests in random order
  (`Minitest::Test.i_suck_and_my_tests_are_order_dependent!` as a humorous
  reminder not to rely on order).

## Guided practice

1. **Prime tester**
   - Implement a simple `PrimeChecker` and write tests covering prime,
     composite, negative, and edge cases.
   - Use `assert`/`refute` along with descriptive failure messages.

2. **HTTP client mock**
   - Build a `WeatherClient` that calls `HTTP.get("/forecast")`.
   - Stub `HTTP.get` to return canned JSON and assert parsing behavior without
     hitting the network.

3. **Custom assertion**
   - Create `assert_valid(record)` that fails with
     `record.errors.full_messages.join(", ")` when validation fails.
   - Use it in tests covering valid and invalid cases.

4. **Spec-style rewrite**
   - Convert an existing xUnit-style test to spec style.
   - Identify pros/cons of each approach in the test comments.

5. **Seed debugging**
   - Introduce a deliberate order dependency between two tests.
   - Run with different seeds to expose the issue, then refactor to remove the
     dependency.

## Self-check questions

1. What’s the difference between `assert_equal`, `assert_same`, and
`assert_in_delta`, and when would you choose each?
2. How does `setup`/`teardown` help keep tests isolated, and what should you
   avoid doing inside them?
3. When mocking collaborators, how do you verify expectations were met and reset
   stubs afterward?
4. Why might you run tests with a random seed, and how do you reproduce a
   failure triggered by randomness?
5. What trade-offs exist between Minitest’s spec style and the traditional test
   class style?

Minitest’s low ceremony encourages fast feedback. Keep tests small,
deterministic, and expressive, and they’ll safeguard your codebase as it grows.

<!-- markdownlint-disable MD033 MD010 -->

## Practical Appendix: Testing Basics — Setup, Fixtures & Mocks (Appendix — testing_basics-ruby2)
<!-- markdownlint-disable MD033 MD034 MD040 MD010 -->

### Fixture patterns and shared helpers

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Strategy</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Factories</td><td>Reusable objects</td><td>Prefer small, focused factories</td></tr>
    <tr><td>Fixtures</td><td>Static sample data</td><td>Good for small projects; keep files small</td></tr>
    <tr><td>Helpers</td><td>DRY setup</td><td>Place in `test/test_helper.rb` and require everywhere</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Mocking checklist

- Prefer explicit expectations (`mock.expect`) when testing interaction
  patterns.
- Use `stub` for simple replacements (e.g., `Time.stub :now, fixed_time`).
- Always `verify` mocks if you set expectations.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->
Practical guidance for organizing tests, using setup/teardown, fixtures, and
when to use mocks or stubs.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Technique</th><th>When</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>setup/teardown</td><td>Common test state</td><td>Use to reduce duplication</td></tr>
    <tr><td>fixtures</td><td>Repeated data</td><td>Prefer factories for flexible tests</td></tr>
    <tr><td>mocks/stubs</td><td>Isolate external systems</td><td>Use sparingly to avoid brittle tests</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example

```ruby
require 'minitest/autorun'

class MyTest < Minitest::Test
  def setup
    @tmp = Tempfile.new('t')
  end

  def teardown
    @tmp.close!
  end
end
```

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — testing_basics-ruby2)

1. Write tests that use setup/teardown to create temporary directories and
   assert cleanup.
2. Replace a test that hits an external API with a mocked response and assert
   behavior remains correct.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-enable MD013 -->
## Practical Appendix: Testing Basics — CI, Parametrized Tests & Benchmarks (Appendix — testing_basics-ruby3)

Guidance for wiring tests into CI, using table-driven tests, and adding
lightweight benchmarks to monitor regressions.

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Topic</th><th>Pattern</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>CI integration</td><td>GitHub Actions / GitLab CI</td><td>Run tests on push and PR
</td></tr>
    <tr><td>Param tests</td><td>Loop over cases</td><td>Keep cases small and canonical</td></tr>
    <tr><td>Benchmarks</td><td>Benchmark/benchmark-ips</td><td>Use CI only for smoke benchmarks</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

### Example: table-driven test

```ruby
cases = [
  [1, 2, 3],
  [2, 2, 4]
]

cases.each do |a, b, expected|
  assert_equal expected, add(a, b)
end
```

### CI snippet (GitHub Actions)

<!-- markdownlint-disable MD033 -->
<table>
  <tr><td><pre><code class="language-yaml">name: Ruby CI
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: ruby/setup-ruby@v1
        with:
          ruby-version: 3.1
      - run: bundle install --jobs 4
      - run: bundle exec rake test
</code></pre></td></tr>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-disable MD013 -->
### Exercises (Appendix — testing_basics-ruby3)

1. Add a small CI workflow that runs tests on PRs and show the config file.
2. Convert repetitive test cases into a table-driven loop and add benchmark
   assertions.

<!-- markdownlint-enable MD033 MD034 MD040 MD010 -->

<!-- markdownlint-disable MD033 MD022 MD032 MD024 -->

## Practical Appendix: Testing Basics — Structure, Fixtures & Fast Feedback (Appendix — testing_basics-appendix)

<!-- markdownlint-disable MD033 -->
<table>
  <thead>
    <tr><th>Area</th><th>Practice</th><th>Why</th></tr>
  </thead>
  <tbody>
    <tr><td>Arrange-Act-Assert</td><td>One responsibility per test</td><td>Clear failures and maintainable suites</td></tr>
    <tr><td>Fixtures</td><td>Use factories or fixtures</td><td>Keep tests focused; avoid heavy setup where possible</td></tr>
    <tr><td>Fast feedback</td><td>Tag slow/integration tests</td><td>Run fast tests during dev, slow tests in CI</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 -->

<!-- markdownlint-enable MD013 -->
### Quick tips

- Run a single test file while developing (`rspec spec/foo_spec.rb`) to iterate
  quickly.
- Use `let` and `before` sparingly; prefer explicit setup for clarity.
- Mock external services to keep tests deterministic.

<!-- markdownlint-disable MD013 -->
### Appendix exercises

1. Tag one slow test as `:integration` and demonstrate running only fast tests
   locally.
2. Replace a fragile fixture with a factory and show improved isolation.

<!-- markdownlint-enable MD033 MD022 MD032 MD024 -->
