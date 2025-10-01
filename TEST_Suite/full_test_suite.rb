#!/usr/bin/env ruby
# frozen_string_literal: true

# TSI Header Extension - Full Test Suite
# Comprehensive Ruby-based testing framework for all extension functionality
# Replaces and enhances the JavaScript/Node.js test suite

require 'fileutils'
require 'pathname'
require 'yaml'
require 'json'
require 'time'

# Add the core library path
$LOAD_PATH.unshift(File.expand_path('../core/lib', __dir__))

# Test Suite Configuration
class TestSuiteConfig
  attr_reader :extension_root, :test_output_dir, :temp_dir, :languages, :verbose

  def initialize
    # Find the extension root by going up from this file's directory
    test_suite_dir = Pathname.new(__dir__)
    @extension_root = test_suite_dir.parent
    @test_output_dir = @extension_root.join('test_output')
    @temp_dir = @extension_root.join('temp_test_files')
    @verbose = ENV['VERBOSE'] == '1'
    @languages = load_supported_languages
  end

  private

  def load_supported_languages
    # Load languages from the existing generators
    languages_file = @extension_root.join('core/generators/languages')
    return [] unless languages_file.directory?

    languages_file.children.select(&:file?).map do |file|
      file.basename.to_s.sub('.js', '')
    end.sort
  end
end

# Test Result Collector
class TestResults
  attr_reader :total_tests, :passed_tests, :failed_tests, :results

  def initialize
    @total_tests = 0
    @passed_tests = 0
    @failed_tests = 0
    @results = []
    @start_time = Time.now
  end

  def add_result(test_name, passed, message = nil, duration = nil)
    @total_tests += 1
    if passed
      @passed_tests += 1
    else
      @failed_tests += 1
    end

    @results << {
      name: test_name,
      passed: passed,
      message: message,
      duration: duration,
      timestamp: Time.now.iso8601
    }
  end

  def success_rate
    return 0 if @total_tests.zero?
    (@passed_tests.to_f / @total_tests * 100).round(1)
  end

  def total_duration
    Time.now - @start_time
  end

  def summary
    {
      total_tests: @total_tests,
      passed_tests: @passed_tests,
      failed_tests: @failed_tests,
      success_rate: success_rate,
      total_duration: total_duration,
      results: @results
    }
  end
end

# Base Test Module
class TestModule
  attr_reader :config, :results

  def initialize(config, results)
    @config = config
    @results = results
    @progress_bar = nil
    @failed_tests = []
  end

  def run_test_with_progress(test_name, total_tests = nil, current_index = nil)
    start_time = Time.now
    begin
      result = yield
      duration = Time.now - start_time

      if result.is_a?(Hash) && result[:passed] != nil
        passed = result[:passed]
        message = result[:message]
        @results.add_result(test_name, passed, message, duration)
      else
        passed = !!result
        message = nil
        @results.add_result(test_name, passed, message, duration)
      end

      # Track failed tests for later display
      @failed_tests << { name: test_name, message: message } unless passed

      # Update progress bar if we have total and current
      if total_tests && current_index
        update_progress_bar(current_index, total_tests, test_name, passed)
      end

      result
    rescue => e
      duration = Time.now - start_time
      @results.add_result(test_name, false, e.message, duration)
      @failed_tests << { name: test_name, message: e.message }
      puts "❌ #{test_name}: #{e.message}" if @config.verbose
      false
    end
  end

  def start_progress_bar(total_tests, title = "Testing")
    @progress_bar = {
      total: total_tests,
      title: title,
      start_time: Time.now,
      last_update: 0
    }
    print_progress_bar(0, total_tests, "Starting...", nil)
  end

  def end_progress_bar
    if @progress_bar
      total_time = Time.now - @progress_bar[:start_time]
      success_rate = @results ? (@results.passed_tests.to_f / @results.total_tests * 100).round(1) : 0

      # Clear progress bar line and show final summary
      print "\r" + " " * 80 + "\r"
      puts "🎯 Completed: #{@results.total_tests} tests, #{success_rate}% success in #{format_duration(total_time)}"

      # Show failed tests if any
      if @failed_tests.any?
        puts "\n❌ Failed Tests:"
        @failed_tests.each do |failure|
          puts "  • #{failure[:name]}#{failure[:message] ? ": #{failure[:message]}" : ""}"
        end
      end
    end
  end

  private

  def update_progress_bar(current, total, current_test, passed)
    # Throttle updates to avoid too much output
    now = Time.now.to_f
    return if now - @progress_bar[:last_update] < 0.1 # Update max 10 times per second

    @progress_bar[:last_update] = now
    print_progress_bar(current, total, current_test, passed)
  end

  def print_progress_bar(current, total, current_test, passed)
    percentage = (current.to_f / total * 100).round(1)
    bar_width = 25
    filled = (current.to_f / total * bar_width).round
    bar = "█" * filled + "░" * (bar_width - filled)

    # Truncate test name to fit
    test_display = current_test.length > 20 ? current_test[0..17] + "..." : current_test

    # Status indicator
    status = passed.nil? ? "⏳" : (passed ? "✅" : "❌")

    # Time elapsed
    elapsed = Time.now - @progress_bar[:start_time]
    time_display = format_duration(elapsed)

    # Create the progress line (keep under 80 chars)
    progress_line = sprintf("%s [%s] %.1f%% %s %s %s",
                          @progress_bar[:title], bar, percentage, test_display, status, time_display)

    # Print with carriage return to overwrite line
    print "\r" + progress_line.ljust(80)
    $stdout.flush
  end

  def format_duration(seconds)
    if seconds < 60
      "#{seconds.round(1)}s"
    elsif seconds < 3600
      minutes = (seconds / 60).floor
      secs = (seconds % 60).round(1)
      "#{minutes}m#{secs}s"
    else
      hours = (seconds / 3600).floor
      minutes = ((seconds % 3600) / 60).floor
      "#{hours}h#{minutes}m"
    end
  end

  def run_test(test_name)
    start_time = Time.now
    begin
      puts "🧪 Running: #{test_name}" if @config.verbose
      result = yield
      duration = Time.now - start_time

      if result.is_a?(Hash) && result[:passed] != nil
        @results.add_result(test_name, result[:passed], result[:message], duration)
        puts result[:passed] ? "✅ #{test_name}" : "❌ #{test_name}: #{result[:message]}"
      else
        @results.add_result(test_name, !!result, nil, duration)
        puts result ? "✅ #{test_name}" : "❌ #{test_name}"
      end

      result
    rescue => e
      duration = Time.now - start_time
      @results.add_result(test_name, false, e.message, duration)
      puts "❌ #{test_name}: #{e.message}"
      puts e.backtrace.join("\n") if @config.verbose
      false
    end
  end

  def assert(condition, message = "Assertion failed")
    raise message unless condition
  end

  def assert_equal(expected, actual, message = nil)
    message ||= "Expected #{expected.inspect}, got #{actual.inspect}"
    assert(expected == actual, message)
  end

  def assert_file_exists(path, message = nil)
    message ||= "File does not exist: #{path}"
    assert(File.exist?(path), message)
  end

  def assert_file_contains(path, content, message = nil)
    assert_file_exists(path)
    file_content = File.read(path)
    message ||= "File #{path} does not contain expected content: #{content}"
    assert(file_content.include?(content), message)
  end

  def assert_directory_exists(path, message = nil)
    message ||= "Directory does not exist: #{path}"
    assert(Dir.exist?(path), message)
  end

  def create_temp_file(name, content = "")
    temp_path = @config.temp_dir.join(name)
    FileUtils.mkdir_p(temp_path.dirname)
    File.write(temp_path, content)
    temp_path
  end

  def create_temp_directory(name)
    temp_path = @config.temp_dir.join(name)
    FileUtils.mkdir_p(temp_path)
    temp_path
  end

  def cleanup_temp_files
    FileUtils.rm_rf(@config.temp_dir) if @config.temp_dir.exist?
  end
end

# Main Test Suite Runner
class FullTestSuite
  attr_reader :config, :results

  def initialize
    @config = TestSuiteConfig.new
    @results = TestResults.new
    @modules = []
  end

  def register_module(module_class)
    @modules << module_class
  end

  def run
    puts "🚀 TSI Header Extension Test Suite"
    puts "=" * 50

    # Ensure test output directory exists
    FileUtils.mkdir_p(@config.test_output_dir.to_s)

    # Load and run all test modules
    @modules.each do |module_class|
      begin
        module_instance = module_class.new(@config, @results)
        module_instance.run
      rescue => e
        puts "❌ Failed to load module #{module_class.name}: #{e.message}"
        @results.add_result("Module Load: #{module_class.name}", false, e.message)
      end
    end

    # Generate final report
    generate_report

    # Clean up temporary files and test output if all tests passed
    if @results.failed_tests == 0
      cleanup_after_success
    end

    # Show final summary
    success_rate = @results.success_rate
    duration = format_duration(@results.total_duration)
    status = @results.failed_tests == 0 ? "✅ PASSED" : "❌ FAILED"

    puts "\n🎯 Test Suite Complete: #{status}"
    puts "Total: #{@results.total_tests} | Passed: #{@results.passed_tests} | Failed: #{@results.failed_tests} | Success: #{success_rate}% | Time: #{duration}"

    # Exit with appropriate code
    exit(@results.failed_tests > 0 ? 1 : 0)
  end

  private

  def generate_report
    report_path = @config.test_output_dir.join('ruby_test_suite_report.json')
    report_data = {
      timestamp: Time.now.iso8601,
      suite_config: {
        extension_root: @config.extension_root.to_s,
        languages_tested: @config.languages.size,
        verbose: @config.verbose
      },
      summary: @results.summary
    }

    File.write(report_path, JSON.pretty_generate(report_data))
    puts "📊 Report saved: #{report_path}"
  end

  def cleanup_after_success
    puts "🧹 Cleaning up temporary files and test output..."

    # Clean up temporary test files
    if @config.temp_dir.exist?
      FileUtils.rm_rf(@config.temp_dir)
      puts "✅ Removed temporary files directory: #{@config.temp_dir}"
    end

    # Clean up test output directory
    if @config.test_output_dir.exist?
      FileUtils.rm_rf(@config.test_output_dir)
      puts "✅ Removed test output directory: #{@config.test_output_dir}"
    end
  end

  def format_duration(seconds)
    if seconds < 60
      "#{seconds.round(2)}s"
    elsif seconds < 3600
      minutes = (seconds / 60).floor
      secs = (seconds % 60).round(2)
      "#{minutes}m #{secs}s"
    else
      hours = (seconds / 3600).floor
      minutes = ((seconds % 3600) / 60).floor
      "#{hours}h #{minutes}m"
    end
  end
end

# Auto-load all test modules
Dir[File.join(__dir__, 'test_*.rb')].each do |file|
  require file
end

# Run the test suite if this file is executed directly
if __FILE__ == $0
  suite = FullTestSuite.new

  # Register all loaded test modules
  # This will be populated as we create more test modules
  if defined?(TestHeaderInsertion)
    suite.register_module(TestHeaderInsertion)
  end
  if defined?(TestCodebaseInsertion)
    suite.register_module(TestCodebaseInsertion)
  end
  if defined?(TestProjectCreation)
    suite.register_module(TestProjectCreation)
  end
  if defined?(TestClassCreation)
    suite.register_module(TestClassCreation)
  end
  if defined?(TestPomodoro)
    suite.register_module(TestPomodoro)
  end
  if defined?(TestLearnModule)
    suite.register_module(TestLearnModule)
  end

  suite.run
end