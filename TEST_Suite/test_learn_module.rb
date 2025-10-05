#!/usr/bin/env ruby
# frozen_string_literal: true

# Unified Learn Module Tests
# Validates that every supported language (C, C++, Ruby) ships a
# curriculum with matching lessons, exercises, starter code, and
# solutions that align with the ExerciseRunner pipeline.

require 'json'
require 'pathname'
require 'open3'

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

        passed, message = if result.is_a?(Hash) && !result[:passed].nil?
                             [!!result[:passed], result[:message]]
                           else
                             [!!result, nil]
                           end

        @results.add_result(test_name, passed, message, duration) if @results.respond_to?(:add_result)
        @failed_tests << { name: test_name, message: message } unless passed
        update_progress_bar(current_index, total_tests, test_name, passed) if total_tests && current_index
        result
      rescue => e
        duration = Time.now - start_time
        @results.add_result(test_name, false, e.message, duration) if @results.respond_to?(:add_result)
        @failed_tests << { name: test_name, message: e.message }
        puts "❌ #{test_name}: #{e.message}" if @config&.verbose
        false
      end
    end

    def start_progress_bar(total_tests, title = 'Testing')
      @progress_bar = {
        total: total_tests,
        title: title,
        start_time: Time.now,
        last_update: 0
      }
      print_progress_bar(0, total_tests, 'Starting...', nil)
    end

    def end_progress_bar
      return unless @progress_bar

      total_time = Time.now - @progress_bar[:start_time]
      success_rate = @results ? (@results.passed_tests.to_f / @results.total_tests * 100).round(1) : 0

      print "\r" + ' ' * 80 + "\r"
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

class TestLearnModule < TestModule
  LANGUAGE_CONFIG = {
    'c' => { language: 'C', starter_extension: '.c', require_starter_file: false, auto_creates_starter: true },
    'cpp' => { language: 'C++', starter_extension: '.cpp', require_starter_file: false, auto_creates_starter: true },
    'ruby' => { language: 'Ruby', starter_extension: '.rb', require_starter_file: false, auto_creates_starter: true }
  }.freeze

  def run
    @repo_root = Pathname.new(__dir__).parent.expand_path
    @curriculum_cache = {}

    total_tests = LANGUAGE_CONFIG.size * 4 + 10
    start_progress_bar(total_tests, 'Learn Modules')

    index = 0
    run_test_with_progress('Exercise descriptions audit', total_tests, index += 1) { run_node_test('exercise_descriptions.test.js') }
    run_test_with_progress('Solution navigation audit', total_tests, index += 1) { run_node_test('solution_navigation.test.js') }
    run_test_with_progress('Exercise escape integrity', total_tests, index += 1) { run_node_test('exercise_escape_integrity.test.js') }
    run_test_with_progress('LearnManager starter creation', total_tests, index += 1) { run_node_test('learn_manager_starter_creation.test.js') }
    run_test_with_progress('Lesson image rendering', total_tests, index += 1) { run_node_test('markdown_image_rendering.test.js') }
    run_test_with_progress('Quiz exercise flow', total_tests, index += 1) { run_node_test('quiz_exercise_flow.test.js') }
    run_test_with_progress('Image resource resolver', total_tests, index += 1) { run_node_test('image_resource_resolver.test.js') }
    run_test_with_progress('Image processing pipeline', total_tests, index += 1) { run_node_test('image_processing_pipeline.test.js') }
    run_test_with_progress('Complete image loading', total_tests, index += 1) { run_node_test('complete_image_loading.test.js') }
    run_test_with_progress('SVG loading validation', total_tests, index += 1) { run_node_test('svg_loading_validation.test.js') }
    LANGUAGE_CONFIG.each do |lang, lang_config|
      run_test_with_progress("#{lang_config[:language]} curriculum", total_tests, index += 1) { validate_curriculum(lang, lang_config) }
      run_test_with_progress("#{lang_config[:language]} lessons", total_tests, index += 1) { validate_lessons(lang, lang_config) }
      run_test_with_progress("#{lang_config[:language]} exercises", total_tests, index += 1) { validate_exercises(lang, lang_config) }
      run_test_with_progress("#{lang_config[:language]} solutions", total_tests, index += 1) { validate_solutions(lang, lang_config) }
    end

    end_progress_bar

    @failed_tests.nil? || @failed_tests.empty?
  end

  private

  def validate_curriculum(lang, lang_config)
    data = curriculum_data(lang)

    assert_equal(lang_config[:language], data['language'], "#{lang} curriculum must declare language #{lang_config[:language]}")

    modules = data['modules']
    assert(modules.is_a?(Array) && !modules.empty?, "#{lang} curriculum must include modules")

    modules.each_with_index do |mod, idx|
      assert(mod['id'].is_a?(String) && !mod['id'].empty?, "Module #{idx + 1} in #{lang} curriculum is missing an id")
      lessons = mod['lessons']
      assert(lessons.is_a?(Array) && !lessons.empty?, "Module #{mod['id']} in #{lang} curriculum must list lessons")
      lessons.each do |lesson|
        assert(lesson.is_a?(Hash), "Lessons within module #{mod['id']} must be objects")
        assert(lesson['id'].is_a?(String) && !lesson['id'].empty?, "Lesson entry missing id in module #{mod['id']}")
      end
    end

    true
  end

  def run_node_test(filename)
    script_path = @repo_root.join('learn', 'tests', filename)
    unless script_path.exist?
      # Treat missing Node-based tests as skipped (the integration tests are
      # optional in some checkouts). Record a warning and continue.
      warn "⚠️ Skipping missing Node test script: #{filename}"
      return true
    end

    stdout, stderr, status = Open3.capture3('node', script_path.to_s)
    return true if status.success?

    raise "#{filename} failed:\n#{stdout}#{stderr}"
  end

  def validate_lessons(lang, _lang_config)
    lessons_dir = language_root(lang).join('lessons')
    assert_directory_exists(lessons_dir, "#{lang} lessons directory missing")

    lesson_ids_for(lang).each do |lesson_id|
      lesson_path = lessons_dir.join("#{lesson_id}.md")
      assert_file_exists(lesson_path, "Lesson markdown missing for #{lang}/#{lesson_id}")

      content = File.read(lesson_path)
      assert(content.start_with?('# '), "Lesson #{lang}/#{lesson_id} should start with a top-level heading")
      assert(content.include?('##'), "Lesson #{lang}/#{lesson_id} should contain at least one secondary heading")
    end

    true
  end

  def validate_exercises(lang, lang_config)
    exercises_dir = language_root(lang).join('exercises')
    assert_directory_exists(exercises_dir, "#{lang} exercises directory missing")

    starters_dir = @repo_root.join('learn_exercises', lang)
    if lang_config[:require_starter_file]
      assert_directory_exists(starters_dir, "Starter exercises directory missing for #{lang}")
    elsif starters_dir.exist?
      visible_entries = starters_dir.children.select { |child| child.basename.to_s !~ /^\./ }
      assert(visible_entries.empty?, "Starter exercises directory should be empty for #{lang}")
    end

    lesson_ids_for(lang).each do |lesson_id|
      exercise_path = exercises_dir.join("#{lesson_id}_exercise.json")
      assert_file_exists(exercise_path, "Exercise JSON missing for #{lang}/#{lesson_id}")

      data = JSON.parse(File.read(exercise_path))
      expected_id = "#{lesson_id}_exercise"
      assert_equal(expected_id, data['id'], "Exercise id mismatch for #{lang}/#{lesson_id}")
      mode = (data['mode'] || 'code').to_s.downcase

      if mode == 'quiz'
        description = data['description']
        assert(description.is_a?(String) && !description.strip.empty?, "Quiz #{lang}/#{lesson_id} must include a learner-facing description")

        questions = data['questions']
        assert(questions.is_a?(Array) && !questions.empty?, "Quiz #{lang}/#{lesson_id} must define questions")

        questions.each_with_index do |question, q_index|
          assert(question.is_a?(Hash), "Quiz question #{q_index + 1} in #{lang}/#{lesson_id} must be an object")
          assert(question['id'].is_a?(String) && !question['id'].empty?, "Quiz question #{q_index + 1} in #{lang}/#{lesson_id} needs an id")
          assert(question['prompt'].is_a?(String) && !question['prompt'].strip.empty?, "Quiz question #{q_index + 1} in #{lang}/#{lesson_id} needs a prompt")

          type = (question['type'] || 'single').to_s.downcase
          case type
          when 'multiple'
            options = question['options']
            assert(options.is_a?(Array) && !options.empty?, "Multiple choice quiz question #{q_index + 1} in #{lang}/#{lesson_id} needs options")
            correct_options = options.select { |opt| opt.is_a?(Hash) && (opt['correct'] || opt['isCorrect']) }
            assert(!correct_options.empty?, "Multiple choice quiz question #{q_index + 1} in #{lang}/#{lesson_id} must mark at least one correct option")
          when 'truefalse'
            answer = question['answer'] || question['correct']
            assert(answer.is_a?(String) && !answer.strip.empty?, "True/False quiz question #{q_index + 1} in #{lang}/#{lesson_id} must define an answer key")
          else
            options = question['options']
            assert(options.is_a?(Array) && !options.empty?, "Single-choice quiz question #{q_index + 1} in #{lang}/#{lesson_id} needs options")
            has_correct = options.any? { |opt| opt.is_a?(Hash) && (opt['correct'] || opt['isCorrect']) }
            has_answer = question['answer']
            assert(has_correct || has_answer, "Single-choice quiz question #{q_index + 1} in #{lang}/#{lesson_id} must define a correct option or answer key")
          end
        end

        next
      end

      starter_code = data['starterCode']
      assert(starter_code.is_a?(String) && !starter_code.strip.empty?, "Starter code must be provided for #{lang}/#{lesson_id}")

      tests = data['tests']
      assert(tests.is_a?(Array) && !tests.empty?, "Exercise #{lang}/#{lesson_id} must define automated tests")
      tests.each_with_index do |test, idx|
        assert(test['name'].is_a?(String) && !test['name'].empty?, "Test #{idx + 1} in #{lang}/#{lesson_id} needs a name")

        if test['type'].is_a?(String) && !test['type'].empty?
          if test['type'] == 'output'
            assert(test['expected'].is_a?(String), "Output test #{idx + 1} in #{lang}/#{lesson_id} requires expected output")
          end
        else
          assert(test['call'].is_a?(String) && !test['call'].empty?, "Test #{idx + 1} in #{lang}/#{lesson_id} must define a call when type is omitted")
          assert(test.key?('expected'), "Test #{idx + 1} in #{lang}/#{lesson_id} must define expected results when type is omitted")
        end
      end

      starter_path = starters_dir.join("#{lesson_id}_exercise#{lang_config[:starter_extension]}")
      if lang_config[:require_starter_file]
        assert_file_exists(starter_path, "Starter code file missing for #{lang}/#{lesson_id}")
      elsif starter_path.exist? && starter_path.file?
        true
      elsif lang_config[:auto_creates_starter]
        assert(!starter_path.exist? || File.zero?(starter_path),
               "Starter file for #{lang}/#{lesson_id} should be generated dynamically, but non-empty file is present")
      end
    end

    true
  end

  def validate_solutions(lang, _lang_config)
    solutions_dir = language_root(lang).join('solutions')
    assert_directory_exists(solutions_dir, "#{lang} solutions directory missing")

    lesson_ids_for(lang).each do |lesson_id|
      solution_path = solutions_dir.join("#{lesson_id}_exercise.json")
      assert_file_exists(solution_path, "Solution file missing for #{lang}/#{lesson_id}")

      data = JSON.parse(File.read(solution_path))
      expected_id = "#{lesson_id}_exercise"
      exercise_identifier = data['exerciseId'] || data['id']
      assert_equal(expected_id, exercise_identifier, "Solution exercise identifier mismatch for #{lang}/#{lesson_id}")

      exercise_path = language_root(lang).join('exercises', "#{lesson_id}_exercise.json")
      exercise_data = JSON.parse(File.read(exercise_path))
      mode = (exercise_data['mode'] || 'code').to_s.downcase

      explanation = data['explanation'] || data['description']
      key_points = data['keyPoints'] || data['key_points'] || data['key_concepts']
      assert(explanation.is_a?(String) && !explanation.strip.empty?, "Solution explanation missing for #{lang}/#{lesson_id}")
      assert(key_points.is_a?(Array) && !key_points.empty?, "Solution key points missing for #{lang}/#{lesson_id}")

      if mode == 'quiz'
        answer_key = data['answerKey'] || data['answers'] || data['solution']
        assert(answer_key.is_a?(Array) && !answer_key.empty?, "Quiz solution for #{lang}/#{lesson_id} must provide an answer key array")
        answer_key.each_with_index do |entry, index|
          assert(entry.is_a?(Hash), "Answer key entry #{index + 1} for #{lang}/#{lesson_id} must be an object")
          assert(entry['questionId'].is_a?(String) && !entry['questionId'].empty?, "Answer key entry #{index + 1} for #{lang}/#{lesson_id} needs questionId")
          answers = entry['answers'] || entry['correctOptions'] || entry['expected']
          assert(answers.is_a?(Array) && !answers.empty?, "Answer key entry #{index + 1} for #{lang}/#{lesson_id} must list correct answers")
        end
        next
      end

      code = data['code'] || data['solution']
      assert(code.is_a?(String) && !code.strip.empty?, "Solution code missing for #{lang}/#{lesson_id}")
    end

    true
  end

  def curriculum_data(lang)
    @curriculum_cache[lang] ||= begin
      path = language_root(lang).join('curriculum.json')
      assert_file_exists(path, "#{lang} curriculum.json is missing")
      JSON.parse(File.read(path))
    end
  end

  def lesson_ids_for(lang)
    data = curriculum_data(lang)
    modules = data['modules'] || []
    modules.flat_map { |mod| (mod['lessons'] || []).map { |lesson| lesson['id'] } }.uniq
  end

  def language_root(lang)
    @repo_root.join('learn', 'curriculum', lang)
  end
end

# Run standalone
if $PROGRAM_NAME == __FILE__
  suite = TestLearnModule.new
  exit(suite.run ? 0 : 1)
end
