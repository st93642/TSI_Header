#!/usr/bin/env ruby
# frozen_string_literal: true

# Test Learn Module (C++)
# Validates that the C++ curriculum mirrors the expected structure

require 'json'
require 'pathname'

# Minimal TestModule for standalone execution
unless defined?(TestModule)
  class TestModule
    attr_reader :config, :results

    def initialize(config = nil, results = nil)
      @config = config || TestSuiteConfig.new
      @results = results || TestResults.new
      @progress_bar = nil
      @failed_tests = []
    end

    def run_test_with_progress(test_name, total_tests = nil, current_index = nil)
      start_time = Time.now
      begin
        result = yield
        duration = Time.now - start_time

        passed = if result.is_a?(Hash) && result[:passed] != nil
                   @results.add_result(test_name, result[:passed], result[:message], duration) if @results.respond_to?(:add_result)
                   !!result[:passed]
                 else
                   @results.add_result(test_name, !!result, nil, duration) if @results.respond_to?(:add_result)
                   !!result
                 end

        @failed_tests << { name: test_name, message: result.is_a?(Hash) ? result[:message] : nil } unless passed

        update_progress_bar(current_index, total_tests, test_name, passed) if total_tests && current_index

        result
      rescue => e
        duration = Time.now - start_time
        @results.add_result(test_name, false, e.message, duration) if @results.respond_to?(:add_result)
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
      return unless @progress_bar

      total_time = Time.now - @progress_bar[:start_time]
      success_rate = @results ? (@results.passed_tests.to_f / @results.total_tests * 100).round(1) : 0

      print "\r" + " " * 80 + "\r"
      puts "🎯 Completed: #{@results&.total_tests || 0} tests, #{success_rate}% success in #{format_duration(total_time)}"

      if @failed_tests.any?
        puts "\n❌ Failed Tests:"
        @failed_tests.each do |failure|
          puts "  • #{failure[:name]}#{failure[:message] ? ": #{failure[:message]}" : ''}"
        end
      end
    end

    private

    def update_progress_bar(current, total, current_test, passed)
      return unless @progress_bar

      now = Time.now.to_f
      return if now - @progress_bar[:last_update] < 0.05

      @progress_bar[:last_update] = now
      print_progress_bar(current, total, current_test, passed)
    end

    def print_progress_bar(current, total, current_test, passed)
      percentage = total.to_i.zero? ? 0 : (current.to_f / total * 100).round(1)
      bar_width = 25
      filled = total.to_i.zero? ? 0 : (current.to_f / total * bar_width).round
      bar = '█' * filled + '░' * (bar_width - filled)
      status = passed.nil? ? '⏳' : (passed ? '✅' : '❌')
      elapsed = Time.now - @progress_bar[:start_time]

      progress_line = format('%s [%s] %.1f%% %s %s %s',
                             @progress_bar[:title], bar, percentage,
                             current_test.to_s[0, 24].ljust(24), status, format_duration(elapsed))

      print "\r" + progress_line.ljust(80)
      $stdout.flush
    end

    def format_duration(seconds)
      if seconds < 60
        format('%.1fs', seconds)
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

    def assert(condition, message = 'Assertion failed')
      raise message unless condition
    end

    def assert_equal(expected, actual, message = nil)
      message ||= "Expected #{expected.inspect}, got #{actual.inspect}"
      assert(expected == actual, message)
    end

    def assert_file_exists(path, message = nil)
      message ||= "Missing file: #{path}"
      assert(File.exist?(path), message)
    end

    def assert_directory_exists(path, message = nil)
      message ||= "Missing directory: #{path}"
      assert(Dir.exist?(path), message)
    end
  end

  class TestSuiteConfig
    attr_reader :extension_root, :verbose

    def initialize
      @extension_root = Pathname.new(__dir__).parent.expand_path
      @verbose = ENV['VERBOSE'] == '1'
    end
  end

  class TestResults
    attr_reader :total_tests, :passed_tests, :failed_tests

    def initialize
      @total_tests = 0
      @passed_tests = 0
      @failed_tests = 0
    end

    def add_result(_test_name, passed, _message = nil, _duration = nil)
      @total_tests += 1
      if passed
        @passed_tests += 1
      else
        @failed_tests += 1
      end
    end
  end
end

class TestLearnCpp < TestModule
  EXPECTED_CURRICULUM = {
    modules: [
      {
        id: 'getting_started',
        lessons: %w[hello_world_cpp iostream_basics]
      },
      {
        id: 'variables_and_data',
        lessons: %w[variables_types_cpp arithmetic_input_cpp]
      },
      {
        id: 'control_flow',
        lessons: %w[conditionals_cpp loops_cpp]
      },
      {
        id: 'functions_and_structure',
        lessons: %w[functions_cpp header_basics_cpp]
      },
      {
        id: 'data_structures_cpp',
        lessons: %w[vectors_cpp structs_cpp classes_objects_cpp]
      },
      {
        id: 'algorithms_and_maps',
        lessons: %w[maps_cpp stl_algorithms_cpp]
      },
      {
        id: 'oop_cpp',
        lessons: %w[classes_encapsulation_cpp inheritance_cpp polymorphism_cpp]
      },
      {
        id: 'templates_cpp',
        lessons: %w[function_class_templates_cpp stl_internals_cpp]
      },
      {
        id: 'exception_handling_cpp',
        lessons: %w[basic_exceptions_cpp exception_safety_cpp]
      },
      {
        id: 'file_io_cpp',
        lessons: %w[file_streams_cpp]
      }
    ].freeze
  }.freeze

  def run
    @repo_root = Pathname.new(__dir__).parent.expand_path
    @cpp_root = @repo_root.join('learn', 'curriculum', 'cpp')

    total_tests = 4
    start_progress_bar(total_tests, 'C++ Curriculum')

    current = 0
    run_test_with_progress('curriculum.json structure', total_tests, current += 1) { validate_curriculum }
    run_test_with_progress('lesson markdown files', total_tests, current += 1) { validate_lessons }
    run_test_with_progress('exercise definitions', total_tests, current += 1) { validate_exercises }
    run_test_with_progress('solution guides', total_tests, current += 1) { validate_solutions }

    end_progress_bar

    @failed_tests.nil? || @failed_tests.empty?
  end

  private

  def validate_curriculum
    curriculum_path = @cpp_root.join('curriculum.json')
    assert_file_exists(curriculum_path, 'C++ curriculum.json is missing')

    data = JSON.parse(File.read(curriculum_path))
    assert_equal('C++', data['language'], 'language should be C++')

    modules = data['modules'] || []
    assert_equal(EXPECTED_CURRICULUM[:modules].size, modules.size, 'Unexpected module count in curriculum.json')

    EXPECTED_CURRICULUM[:modules].each_with_index do |expected_module, index|
      actual_module = modules[index] || {}
      assert_equal(expected_module[:id], actual_module['id'], "Module #{index + 1} should have id #{expected_module[:id]}")

      lesson_ids = (actual_module['lessons'] || []).map { |lesson| lesson['id'] }
      assert_equal(expected_module[:lessons], lesson_ids, "Module #{expected_module[:id]} should include lessons #{expected_module[:lessons].join(', ')}")
    end

    true
  end

  def validate_lessons
    lessons_dir = @cpp_root.join('lessons')
    assert_directory_exists(lessons_dir, 'C++ lessons directory missing')

    EXPECTED_CURRICULUM[:modules].flat_map { |mod| mod[:lessons] }.each do |lesson_id|
      lesson_path = lessons_dir.join("#{lesson_id}.md")
      assert_file_exists(lesson_path, "Lesson markdown missing for #{lesson_id}")

      content = File.read(lesson_path)
      assert(content.start_with?('# '), "Lesson #{lesson_id} should start with a level-one heading")
      assert(content.include?('Practice Time'), "Lesson #{lesson_id} should include a Practice Time section")
    end

    true
  end

  def validate_exercises
    exercises_dir = @cpp_root.join('exercises')
    assert_directory_exists(exercises_dir, 'C++ exercises directory missing')

    EXPECTED_CURRICULUM[:modules].flat_map { |mod| mod[:lessons] }.each do |lesson_id|
      exercise_path = exercises_dir.join("#{lesson_id}_exercise.json")
      assert_file_exists(exercise_path, "Exercise file missing for #{lesson_id}")

      data = JSON.parse(File.read(exercise_path))
      assert_equal("#{lesson_id}_exercise", data['id'], "Exercise id mismatch for #{lesson_id}")
      assert(data['starterCode'].is_a?(String) && data['starterCode'].include?('TODO'), "Starter code for #{lesson_id} should include TODO guidance")

      tests = data['tests'] || []
      assert(!tests.empty?, "Exercise #{lesson_id} must include automated tests")

      tests.each do |test|
        assert_equal('output', test['type'], 'C++ tests should use output comparisons')
        assert(test['expected'].is_a?(String), 'Expected output must be a string literal')
      end
    end

    true
  end

  def validate_solutions
    solutions_dir = @cpp_root.join('solutions')
    assert_directory_exists(solutions_dir, 'C++ solutions directory missing')

    EXPECTED_CURRICULUM[:modules].flat_map { |mod| mod[:lessons] }.each do |lesson_id|
      solution_path = solutions_dir.join("#{lesson_id}_exercise.json")
      assert_file_exists(solution_path, "Solution file missing for #{lesson_id}")

      data = JSON.parse(File.read(solution_path))
      assert_equal("#{lesson_id}_exercise", data['exerciseId'], "Solution exerciseId mismatch for #{lesson_id}")
      assert(data['code'].include?('#include'), "Solution code for #{lesson_id} should include a C++ header")
      assert(data['explanation'].is_a?(String) && !data['explanation'].strip.empty?, "Solution explanation missing for #{lesson_id}")
    end

    true
  end
end

# Run standalone
if $PROGRAM_NAME == __FILE__
  suite = TestLearnCpp.new
  exit(suite.run ? 0 : 1)
end