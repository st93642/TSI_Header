#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'pathname'

require_relative 'full_test_suite'

class TestLearnModule < TestModule
  EXPECTED_CHAPTERS = [
    { id: 'chapter_01_basic_ideas', lesson: 'chapter_01_basic_ideas', title: 'Chapter 1: Basic Ideas' },
    { id: 'chapter_02_setup_toolchain', lesson: 'chapter_02_setup_toolchain', title: 'Chapter 2: Setting Up Your Toolchain' },
    { id: 'chapter_03_language_basics', lesson: 'chapter_03_language_basics', title: 'Chapter 3: Language Building Blocks' },
    { id: 'chapter_04_control_flow', lesson: 'chapter_04_control_flow', title: 'Chapter 4: Control Flow and Branching' },
    { id: 'chapter_05_functions', lesson: 'chapter_05_functions', title: 'Chapter 5: Functions and Modular Design' },
    { id: 'chapter_06_collections', lesson: 'chapter_06_collections', title: 'Chapter 6: Arrays, Vectors, and Iteration' },
    { id: 'chapter_07_pointers', lesson: 'chapter_07_pointers', title: 'Chapter 7: Pointers and References' },
    { id: 'chapter_08_memory', lesson: 'chapter_08_memory', title: 'Chapter 8: Memory Management' },
    { id: 'chapter_09_classes', lesson: 'chapter_09_classes', title: 'Chapter 9: Classes and Objects' },
    { id: 'chapter_10_operator_overloading', lesson: 'chapter_10_operator_overloading', title: 'Chapter 10: Operator Overloading' },
    { id: 'chapter_11_inheritance', lesson: 'chapter_11_inheritance', title: 'Chapter 11: Inheritance and Polymorphism' },
    { id: 'chapter_12_templates', lesson: 'chapter_12_templates', title: 'Chapter 12: Templates and the STL' }
  ].freeze unless defined?(EXPECTED_CHAPTERS)

  REQUIRED_EXERCISE_KEYS = %w[id title description difficulty starterCode tests hints tags].freeze unless defined?(REQUIRED_EXERCISE_KEYS)
  VALID_DIFFICULTIES = %w[beginner intermediate advanced].freeze unless defined?(VALID_DIFFICULTIES)

  def run
    puts "\n📚 Testing Learn Module"
    puts '=' * 50

    @learn_dir = @config.extension_root.join('learn')
    @cpp_dir = @learn_dir.join('curriculum', 'cpp')
    @lessons_dir = @cpp_dir.join('lessons')
    @exercises_dir = @cpp_dir.join('exercises')
    @solutions_dir = @cpp_dir.join('solutions')
    @cpp_workspace_dir = @config.extension_root.join('learn_exercises', 'cpp')

    @total_tests = 8 + EXPECTED_CHAPTERS.length * 8
    start_progress_bar(@total_tests, 'Learn Module')

    run_structure_tests
    run_curriculum_tests
    run_chapter_tests

    end_progress_bar
  end

  private

  def run_structure_tests
    current = 0
    current = perform(current, 'learn directory exists') { directory?(@learn_dir) }
    current = perform(current, 'cpp curriculum directory exists') { directory?(@cpp_dir) }
    current = perform(current, 'lessons directory exists') { directory?(@lessons_dir) }
    current = perform(current, 'exercises directory exists') { directory?(@exercises_dir) }
    current = perform(current, 'solutions directory exists') { directory?(@solutions_dir) }
    current = perform(current, 'learn_exercises/cpp directory exists') { directory?(@cpp_workspace_dir) }
    current = perform(current, 'learn_exercises/c directory removed') do
      c_dir = @config.extension_root.join('learn_exercises', 'c')
      { passed: !c_dir.exist? || Dir.children(c_dir).empty?, message: c_dir.to_s }
    end
    perform(current, 'cpp workspace has starter files') do
      files = Dir.glob(@cpp_workspace_dir.join('*.cpp')).map { |path| Pathname.new(path).basename.to_s }
      { passed: files.any?, message: files.join(', ') }
    end
  end

  def run_curriculum_tests
    current = 6
    curriculum_path = @cpp_dir.join('curriculum.json')

    current = perform(current, 'curriculum.json exists') { file?(curriculum_path) }

    @curriculum = nil
    current = perform(current, 'curriculum.json parses') do
      begin
        @curriculum = JSON.parse(curriculum_path.read)
        { passed: true, message: 'OK' }
      rescue JSON::ParserError => e
        { passed: false, message: e.message }
      end
    end

    current = perform(current, 'curriculum language is C++') do
      lang = @curriculum.is_a?(Hash) ? @curriculum['language'] : nil
      { passed: lang == 'C++', message: lang.to_s }
    end

    perform(current, 'curriculum has expected chapters') do
      chapter_ids = Array(@curriculum['chapters']).map { |chapter| chapter['id'] }
      missing = EXPECTED_CHAPTERS.reject { |chapter| chapter_ids.include?(chapter[:id]) }
      extra = chapter_ids - EXPECTED_CHAPTERS.map { |c| c[:id] }
      passed = missing.empty? && extra.empty?
      message = []
      message << "missing: #{missing.map { |c| c[:id] }.join(', ')}" unless missing.empty?
      message << "extra: #{extra.join(', ')}" unless extra.empty?
      message << "count=#{chapter_ids.length}" if message.empty?
      { passed:, message: message.join(' | ') }
    end
  end

  def run_chapter_tests
    chapters = Array(@curriculum['chapters'])

    EXPECTED_CHAPTERS.each_with_index do |spec, index|
      chapter = chapters.find { |c| c['id'] == spec[:id] }
      base = 8 + index * 8

      perform(base, "#{spec[:title]} metadata exists") do
        { passed: !chapter.nil?, message: chapter ? chapter['title'].to_s : 'missing' }
      end

      lesson = Array(chapter && chapter['lessons']).find { |l| l['id'] == spec[:lesson] }
      perform(base + 1, "#{spec[:title]} lesson entry present") do
        { passed: !lesson.nil?, message: lesson ? lesson['title'].to_s : 'missing' }
      end

      lesson_path = @lessons_dir.join("#{spec[:lesson]}.md")
      perform(base + 2, "#{spec[:title]} lesson file exists") do
        header = lesson_path.file? ? lesson_path.read.strip.lines.first.to_s.strip : 'missing'
        { passed: lesson_path.file?, message: header }
      end

      exercise_path = @exercises_dir.join("#{spec[:lesson]}_exercise.json")
      exercise_data = nil
      perform(base + 3, "#{spec[:title]} exercise file exists") do
        { passed: exercise_path.file?, message: exercise_path.to_s }
      end

      perform(base + 4, "#{spec[:title]} exercise schema valid") do
        begin
          exercise_data = JSON.parse(exercise_path.read)
          missing = REQUIRED_EXERCISE_KEYS - exercise_data.keys
          { passed: missing.empty?, message: missing.empty? ? 'ok' : "missing: #{missing.join(', ')}" }
        rescue JSON::ParserError => e
          { passed: false, message: e.message }
        end
      end

      perform(base + 5, "#{spec[:title]} exercise focuses on modern C++") do
        difficulty_ok = exercise_data && VALID_DIFFICULTIES.include?(exercise_data['difficulty'])
        variants = Array(exercise_data && exercise_data['variants'])
        languages = variants.map { |variant| variant['language'].to_s.downcase }.uniq
        top_language = exercise_data && exercise_data['language'].to_s.downcase
        only_cpp = (languages.empty? || languages == ['cpp']) && (top_language.empty? || top_language == 'cpp')
        starter = exercise_data ? exercise_data['starterCode'].to_s : ''
        avoids_c_stdio = !starter.include?('printf(')
        { passed: difficulty_ok && only_cpp && avoids_c_stdio, message: "difficulty=#{exercise_data&.fetch('difficulty', nil)}, variants=#{languages.join('/')}, starter_cpp=#{avoids_c_stdio}" }
      end

      perform(base + 6, "#{spec[:title]} exercise has adequate tests and hints") do
        tests = Array(exercise_data && exercise_data['tests'])
        hints = Array(exercise_data && exercise_data['hints'])
        tests_ok = tests.length >= 3 && tests.all? { |test| test.is_a?(Hash) && (test['expected'] || test['call']) }
        hints_ok = hints.length >= 3
        { passed: tests_ok && hints_ok, message: "tests=#{tests.length}, hints=#{hints.length}" }
      end

      solution_path = @solutions_dir.join("#{spec[:lesson]}_exercise.json")
      perform(base + 7, "#{spec[:title]} solution explains approach") do
        next { passed: false, message: 'missing solution' } unless solution_path.file?
        begin
          solution = JSON.parse(solution_path.read)
          variants = Array(solution['variants'])
          has_cpp_code = variants.any? { |variant| variant['code'].to_s.include?('std::') }
          explanation = solution['explanation'].to_s
          has_explanation = !explanation.strip.empty? || variants.any? { |variant| !variant['explanation'].to_s.strip.empty? }
          { passed: has_cpp_code && has_explanation, message: solution_path.to_s }
        rescue JSON::ParserError => e
          { passed: false, message: e.message }
        end
      end
    end
  end

  def perform(index, description)
    index += 1
    run_test_with_progress(description, @total_tests, index) { yield }
    index
  end

  def directory?(path)
    { passed: path.directory?, message: path.to_s }
  end

  def file?(path)
    { passed: path.file?, message: path.to_s }
  end
end

if __FILE__ == $PROGRAM_NAME
  config = TestSuiteConfig.new
  results = TestResults.new
  TestLearnModule.new(config, results).run
end
