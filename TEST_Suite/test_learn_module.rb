#!/usr/bin/env ruby
# frozen_string_literal: true

# Test Learn Module
# Tests the Learn Module curriculum, lessons, exercises, and functionality

require 'json'
require 'pathname'
require 'fileutils'

# Load test base
require_relative 'full_test_suite'

# Test Learn Module functionality
class TestLearnModule < TestModule
  def run
    puts "\n📚 Testing Learn Module"
    puts "=" * 50
    
  @learn_dir = @config.extension_root.join('learn')
  @curriculum_dir = @learn_dir.join('curriculum', 'ruby')
  @cpp_curriculum_dir = @learn_dir.join('curriculum', 'cpp')
    @lib_dir = @learn_dir.join('lib')
    
  # Total tests count
  total_tests = 104
    start_progress_bar(total_tests, "Learn Module")
    
    current = 0
    
    # Directory structure tests (5 tests)
    current += 1
    run_test_with_progress("Learn directory exists", total_tests, current) do
      { passed: @learn_dir.directory?, message: @learn_dir.to_s }
    end
    
    current += 1
    run_test_with_progress("Curriculum directory exists", total_tests, current) do
      { passed: @curriculum_dir.directory?, message: @curriculum_dir.to_s }
    end
    
    current += 1
    run_test_with_progress("Lib directory exists", total_tests, current) do
      { passed: @lib_dir.directory?, message: @lib_dir.to_s }
    end
    
    current += 1
    run_test_with_progress("Lessons directory exists", total_tests, current) do
      lessons_dir = @curriculum_dir.join('lessons')
      { passed: lessons_dir.directory?, message: lessons_dir.to_s }
    end
    
    current += 1
    run_test_with_progress("Exercises directory exists", total_tests, current) do
      exercises_dir = @curriculum_dir.join('exercises')
      { passed: exercises_dir.directory?, message: exercises_dir.to_s }
    end
    
    # Curriculum.json tests (4 tests)
    curriculum_file = @curriculum_dir.join('curriculum.json')
    curriculum = nil
    
    current += 1
    run_test_with_progress("curriculum.json exists", total_tests, current) do
      { passed: curriculum_file.file?, message: curriculum_file.to_s }
    end
    
    current += 1
    run_test_with_progress("curriculum.json is valid JSON", total_tests, current) do
      begin
        curriculum = JSON.parse(curriculum_file.read)
        { passed: true, message: "Valid JSON" }
      rescue JSON::ParserError => e
        { passed: false, message: e.message }
      end
    end
    
    current += 1
    run_test_with_progress("Curriculum has Ruby language", total_tests, current) do
      { passed: curriculum && curriculum['language'] == 'Ruby', message: curriculum ? curriculum['language'] : 'nil' }
    end
    
    current += 1
    run_test_with_progress("Curriculum has 8 modules", total_tests, current) do
      count = curriculum ? curriculum['modules'].length : 0
      { passed: count == 8, message: "Found #{count} modules" }
    end
    
    # Lesson files tests (4 tests)
    lesson_ids = []
    if curriculum && curriculum['modules']
      curriculum['modules'].each do |mod|
        mod['lessons'].each { |lesson| lesson_ids << lesson['id'] }
      end
    end
    
    current += 1
    run_test_with_progress("All 37 lessons defined in curriculum", total_tests, current) do
      { passed: lesson_ids.length == 37, message: "Found #{lesson_ids.length} lessons" }
    end
    
    current += 1
    run_test_with_progress("All lesson files exist", total_tests, current) do
      missing = lesson_ids.reject { |id| @curriculum_dir.join('lessons', "#{id}.md").file? }
      { passed: missing.empty?, message: missing.empty? ? "All present" : "Missing: #{missing.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("All lesson files have content", total_tests, current) do
      empty = lesson_ids.select { |id| 
        file = @curriculum_dir.join('lessons', "#{id}.md")
        file.file? && File.read(file).strip.empty?
      }
      { passed: empty.empty?, message: empty.empty? ? "All have content" : "Empty: #{empty.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("Lessons have proper Markdown headers", total_tests, current) do
      no_headers = lesson_ids.select { |id|
        file = @curriculum_dir.join('lessons', "#{id}.md")
        file.file? && !File.read(file).match?(/^#\s+/)
      }
      { passed: no_headers.empty?, message: no_headers.empty? ? "All valid" : "No header: #{no_headers.join(', ')}" }
    end
    
    # Exercise files tests (6 tests)
    current += 1
    run_test_with_progress("All exercise files exist", total_tests, current) do
      missing = lesson_ids.reject { |id| @curriculum_dir.join('exercises', "#{id}_exercise.json").file? }
      { passed: missing.empty?, message: missing.empty? ? "All present" : "Missing: #{missing.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("All exercises are valid JSON", total_tests, current) do
      invalid = []
      lesson_ids.each do |id|
        file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        next unless file.file?
        JSON.parse(file.read)
      rescue JSON::ParserError
        invalid << id
      end
      { passed: invalid.empty?, message: invalid.empty? ? "All valid" : "Invalid: #{invalid.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("All exercises have required fields", total_tests, current) do
      required = ['id', 'title', 'description', 'difficulty', 'starterCode', 'tests', 'hints', 'tags']
      invalid = []
      lesson_ids.each do |id|
        file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        next unless file.file?
        exercise = JSON.parse(file.read)
        missing = required - exercise.keys
        invalid << "#{id} (missing: #{missing.join(', ')})" unless missing.empty?
      end
      { passed: invalid.empty?, message: invalid.empty? ? "All complete" : invalid.join('; ') }
    end
    
    current += 1
    run_test_with_progress("All exercises have hints", total_tests, current) do
      no_hints = []
      lesson_ids.each do |id|
        file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        next unless file.file?
        exercise = JSON.parse(file.read)
        no_hints << id if !exercise['hints'].is_a?(Array) || exercise['hints'].empty?
      end
      { passed: no_hints.empty?, message: no_hints.empty? ? "All have hints" : "No hints: #{no_hints.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("Exercise IDs match lesson IDs", total_tests, current) do
      mismatched = []
      lesson_ids.each do |id|
        file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        next unless file.file?
        exercise = JSON.parse(file.read)
        expected_id = "#{id}_exercise"
        mismatched << "#{id} (expected: #{expected_id}, got: #{exercise['id']})" if exercise['id'] != expected_id
      end
      { passed: mismatched.empty?, message: mismatched.empty? ? "All match" : mismatched.join('; ') }
    end
    
    current += 1
    run_test_with_progress("Exercise difficulty levels are valid", total_tests, current) do
      valid_levels = ['beginner', 'intermediate', 'advanced']
      invalid = []
      lesson_ids.each do |id|
        file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        next unless file.file?
        exercise = JSON.parse(file.read)
        invalid << "#{id} (#{exercise['difficulty']})" unless valid_levels.include?(exercise['difficulty'])
      end
      { passed: invalid.empty?, message: invalid.empty? ? "All valid" : "Invalid: #{invalid.join(', ')}" }
    end
    
    # Learn module library tests (5 tests)
    current += 1
    run_test_with_progress("index.js exists", total_tests, current) do
      file = @learn_dir.join('index.js')
      { passed: file.file?, message: file.to_s }
    end
    
    current += 1
    run_test_with_progress("learn_manager.js exists", total_tests, current) do
      file = @lib_dir.join('learn_manager.js')
      { passed: file.file?, message: file.to_s }
    end
    
    current += 1
    run_test_with_progress("progress_tracker.js exists", total_tests, current) do
      file = @lib_dir.join('progress_tracker.js')
      { passed: file.file?, message: file.to_s }
    end
    
    current += 1
    run_test_with_progress("exercise_runner.js exists", total_tests, current) do
      file = @lib_dir.join('exercise_runner.js')
      { passed: file.file?, message: file.to_s }
    end
    
    current += 1
    run_test_with_progress("Learn class exported from index.js", total_tests, current) do
      file = @learn_dir.join('index.js')
      content = file.file? ? File.read(file) : ""
      has_export = content.include?('class Learn') && content.include?('module.exports')
      { passed: has_export, message: has_export ? "Learn class exported" : "Learn class not found" }
    end
    
    # Functionality tests (3 tests)
    current += 1
    run_test_with_progress("LearnManager can load curriculum", total_tests, current) do
      file = @lib_dir.join('learn_manager.js')
      content = file.file? ? File.read(file) : ""
      has_load = content.include?('loadCurriculum') || content.include?('_loadCurriculum')
      { passed: has_load, message: has_load ? "loadCurriculum found" : "loadCurriculum not found" }
    end
    
    current += 1
    run_test_with_progress("ExerciseRunner can run exercises", total_tests, current) do
      file = @lib_dir.join('exercise_runner.js')
      content = file.file? ? File.read(file) : ""
      has_run = content.include?('async run(') || content.include?('runTests')
      { passed: has_run, message: has_run ? "Exercise execution found" : "Exercise execution not found" }
    end
    
    current += 1
    run_test_with_progress("ProgressTracker can track progress", total_tests, current) do
      file = @lib_dir.join('progress_tracker.js')
      content = file.file? ? File.read(file) : ""
      has_tracking = content.include?('recordCompletion') || content.include?('getProgress')
      { passed: has_tracking, message: has_tracking ? "Progress tracking found" : "Progress tracking not found" }
    end
    
    # Lesson-Exercise Correspondence Tests (8 tests)
    current += 1
    run_test_with_progress("All lessons have corresponding exercises", total_tests, current) do
      missing_exercises = lesson_ids.reject { |id| @curriculum_dir.join('exercises', "#{id}_exercise.json").file? }
      { passed: missing_exercises.empty?, message: missing_exercises.empty? ? "All lessons have exercises" : "Missing exercises for: #{missing_exercises.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("All exercises correspond to valid lessons", total_tests, current) do
      exercise_files = Dir.glob("#{@curriculum_dir}/exercises/*_exercise.json").map { |f| File.basename(f, '_exercise.json') }
      invalid_exercises = exercise_files.reject { |id| lesson_ids.include?(id) }
      { passed: invalid_exercises.empty?, message: invalid_exercises.empty? ? "All exercises valid" : "Exercises without lessons: #{invalid_exercises.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("Exercise titles match lesson titles", total_tests, current) do
      mismatches = []
      lesson_ids.each do |id|
        lesson_file = @curriculum_dir.join('lessons', "#{id}.md")
        exercise_file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        
        next unless lesson_file.file? && exercise_file.file?
        
        # Extract lesson title from first line of markdown
        lesson_content = File.read(lesson_file)
        lesson_title_match = lesson_content.match(/^#\s+(.+)$/)
        lesson_title = lesson_title_match ? lesson_title_match[1].strip : ""
        
        # Get exercise title
        exercise_data = JSON.parse(File.read(exercise_file))
        exercise_title = exercise_data['title'] || ""
        
        # Check if exercise title contains lesson title or vice versa
        title_matches = lesson_title.include?(exercise_title) || exercise_title.include?(lesson_title) || 
                       lesson_title.downcase.include?(exercise_title.downcase) || 
                       exercise_title.downcase.include?(lesson_title.downcase)
        
        unless title_matches
          mismatches << "#{id}: '#{lesson_title}' vs '#{exercise_title}'"
        end
      end
      { passed: mismatches.empty?, message: mismatches.empty? ? "All titles match" : "Title mismatches: #{mismatches.join('; ')}" }
    end
    
    current += 1
    run_test_with_progress("Exercises have proper difficulty levels", total_tests, current) do
      valid_difficulties = ['beginner', 'intermediate', 'advanced']
      invalid = []
      lesson_ids.each do |id|
        exercise_file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        next unless exercise_file.file?
        
        exercise_data = JSON.parse(File.read(exercise_file))
        difficulty = exercise_data['difficulty']
        unless valid_difficulties.include?(difficulty)
          invalid << "#{id}: '#{difficulty}'"
        end
      end
      { passed: invalid.empty?, message: invalid.empty? ? "All difficulties valid" : "Invalid difficulties: #{invalid.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("Exercises have meaningful descriptions", total_tests, current) do
      empty_descriptions = []
      lesson_ids.each do |id|
        exercise_file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        next unless exercise_file.file?
        
        exercise_data = JSON.parse(File.read(exercise_file))
        description = exercise_data['description'] || ""
        if description.strip.empty? || description.length < 10
          empty_descriptions << id
        end
      end
      { passed: empty_descriptions.empty?, message: empty_descriptions.empty? ? "All descriptions meaningful" : "Empty/short descriptions: #{empty_descriptions.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("Exercises have starter code", total_tests, current) do
      no_starter_code = []
      lesson_ids.each do |id|
        exercise_file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        next unless exercise_file.file?
        
        exercise_data = JSON.parse(File.read(exercise_file))
        starter_code = exercise_data['starterCode'] || ""
        if starter_code.strip.empty?
          no_starter_code << id
        end
      end
      { passed: no_starter_code.empty?, message: no_starter_code.empty? ? "All have starter code" : "No starter code: #{no_starter_code.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("Exercise files have meaningful content", total_tests, current) do
      empty_exercises = []
      exercises_dir = @config.extension_root.join('learn_exercises', 'ruby')
      
      lesson_ids.each do |id|
        exercise_file = exercises_dir.join("#{id}_exercise.rb")
        next unless exercise_file.file?
        
        content = File.read(exercise_file).strip
        # Check if file has meaningful content (more than just "# Your code here" or empty)
        if content.empty? || content == "# Your code here" || content.length < 20
          empty_exercises << id
        end
      end
      { passed: empty_exercises.empty?, message: empty_exercises.empty? ? "All exercise files have content" : "Empty/minimal content: #{empty_exercises.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("Exercises have comprehensive tests", total_tests, current) do
      insufficient_tests = []
      lesson_ids.each do |id|
        exercise_file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        next unless exercise_file.file?
        
        exercise_data = JSON.parse(File.read(exercise_file))
        tests = exercise_data['tests'] || []
        if tests.length < 2
          insufficient_tests << "#{id} (#{tests.length} tests)"
        end
      end
      { passed: insufficient_tests.empty?, message: insufficient_tests.empty? ? "All have sufficient tests" : "Insufficient tests: #{insufficient_tests.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("Exercise tests are valid", total_tests, current) do
      invalid_tests = []
      lesson_ids.each do |id|
        exercise_file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        next unless exercise_file.file?
        
        begin
          exercise_data = JSON.parse(File.read(exercise_file))
          tests = exercise_data['tests'] || []
          
          tests.each do |test|
            unless test.is_a?(Hash) && test['name'] && test['call']
              invalid_tests << "#{id}: invalid test structure"
              break
            end
          end
        rescue JSON::ParserError
          invalid_tests << "#{id}: JSON error"
        end
      end
      { passed: invalid_tests.empty?, message: invalid_tests.empty? ? "All tests valid" : "Invalid tests: #{invalid_tests.join(', ')}" }
    end
    
    # Solution Files Tests (3 tests)
    current += 1
    run_test_with_progress("Solutions directory exists", total_tests, current) do
      solutions_dir = @curriculum_dir.join('solutions')
      { passed: solutions_dir.directory?, message: solutions_dir.to_s }
    end
    
    current += 1
    run_test_with_progress("All exercises have solution files", total_tests, current) do
      missing_solutions = lesson_ids.reject { |id| @curriculum_dir.join('solutions', "#{id}_exercise.json").file? }
      { passed: missing_solutions.empty?, message: missing_solutions.empty? ? "All have solutions" : "Missing solutions: #{missing_solutions.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("Solution files are valid JSON", total_tests, current) do
      invalid_solutions = []
      lesson_ids.each do |id|
        solution_file = @curriculum_dir.join('solutions', "#{id}_exercise.json")
        next unless solution_file.file?
        
        begin
          JSON.parse(File.read(solution_file))
        rescue JSON::ParserError
          invalid_solutions << id
        end
      end
      { passed: invalid_solutions.empty?, message: invalid_solutions.empty? ? "All solutions valid" : "Invalid solutions: #{invalid_solutions.join(', ')}" }
    end
    
    current += 1
    run_test_with_progress("Complete lesson components exist and correspond", total_tests, current) do
      missing_components = []
      inconsistent_components = []
      lesson_ids.each do |id|
        # Check if all 4 components exist
        lesson_file = @curriculum_dir.join('lessons', "#{id}.md")
        exercise_file = @curriculum_dir.join('exercises', "#{id}_exercise.json")
        solution_file = @curriculum_dir.join('solutions', "#{id}_exercise.json")
        starter_file = @config.extension_root.join('learn_exercises', 'ruby', "#{id}_exercise.rb")
        
        components = {
          'lesson' => lesson_file.file?,
          'exercise' => exercise_file.file?,
          'solution' => solution_file.file?,
          'starter' => starter_file.file?
        }
        
        missing = components.select { |name, exists| !exists }.keys
        if !missing.empty?
          missing_components << "#{id}: missing #{missing.join(', ')}"
          next
        end
        
        # Check title correspondence between lesson and exercise
        begin
          lesson_content = File.read(lesson_file)
          lesson_title_match = lesson_content.match(/^#\s+(.+)$/)
          lesson_title = lesson_title_match ? lesson_title_match[1].strip : ""
          
          exercise_data = JSON.parse(File.read(exercise_file))
          exercise_title = exercise_data['title'] || ""
          
          # Extract the core theme from both titles (ignoring "Lesson X.Y:" and "Exercise X.Y:" prefixes)
          lesson_core = lesson_title.gsub(/^Lesson\s+\d+\.\d+:\s*/, '').strip
          exercise_core = exercise_title.gsub(/^Exercise\s+\d+\.\d+:\s*/, '').strip
          starter_code = (exercise_data['starterCode'] || '').strip
          exercise_tests = exercise_data['tests'] || []
          
          # Check if core themes match
          unless lesson_core.downcase == exercise_core.downcase
            inconsistent_components << "#{id}: lesson '#{lesson_core}' vs exercise '#{exercise_core}'"
          end
          if starter_code.empty?
            inconsistent_components << "#{id}: starterCode is blank in exercise definition"
          end
          if exercise_tests.empty?
            inconsistent_components << "#{id}: exercise has no tests defined"
          end
          
          # Check if solution has correct exercise ID
          solution_data = JSON.parse(File.read(solution_file))
          solution_exercise_id = solution_data['exerciseId'] || solution_data['id'] || ""
          unless solution_exercise_id == "#{id}_exercise" || solution_exercise_id == id
            inconsistent_components << "#{id}: solution has wrong exerciseId '#{solution_exercise_id}'"
          end
          solution_source = (solution_data['code'] || solution_data['solution'] || '').strip
          solution_explanation = (solution_data['explanation'] || '').strip
          if solution_source.empty?
            inconsistent_components << "#{id}: solution is missing code content"
          end
          if solution_explanation.empty?
            inconsistent_components << "#{id}: solution explanation is missing"
          end
          
          # Check if starter file has theme-appropriate content
          starter_content = File.read(starter_file)
          starter_has_theme = starter_content.include?(lesson_core.split.first) || 
                             starter_content.downcase.include?(lesson_core.downcase.split.first) ||
                             starter_content.include?('TSI') # Our themed exercises should mention TSI
          unless starter_has_theme
            inconsistent_components << "#{id}: starter file doesn't match theme '#{lesson_core}'"
          end
          if starter_content.strip.empty?
            inconsistent_components << "#{id}: starter file is empty"
          end
          
        rescue JSON::ParserError, StandardError => e
          inconsistent_components << "#{id}: error checking correspondence - #{e.message}"
        end
      end
      
      all_issues = missing_components + inconsistent_components
      { passed: all_issues.empty?, message: all_issues.empty? ? "All lesson components exist and correspond" : "Issues: #{all_issues.join('; ')}" }
    end
    
    # C++ Chapter 1 Consistency Tests (6 tests)
    cpp_ch1_exercise_path = @cpp_curriculum_dir.join('exercises', 'chapter_01_basic_ideas_exercise.json')
    cpp_ch1_solution_path = @cpp_curriculum_dir.join('solutions', 'chapter_01_basic_ideas_exercise.json')
    cpp_ch1_cpp_stub_path = @config.extension_root.join('learn_exercises', 'cpp', 'chapter_01_basic_ideas_cpp.cpp')
    cpp_ch1_c_stub_path = @config.extension_root.join('learn_exercises', 'c', 'chapter_01_basic_ideas_c.c')

    current += 1
    run_test_with_progress("C++ Chapter 1 exercise metadata emphasises basics", total_tests, current) do
      unless cpp_ch1_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch1_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch1_exercise_path))
        tags = Array(exercise['tags'])
        { passed: exercise['difficulty'] == 'beginner' && tags.include?('basics'), message: "difficulty=#{exercise['difficulty']}, tags=#{tags.join(', ')}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 1 tests use the Hello World blueprint", total_tests, current) do
      unless cpp_ch1_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch1_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch1_exercise_path))
        tests = exercise['variants'].flat_map { |variant| Array(variant['tests']) }
        expected_outputs = tests.map { |t| t['expected'] || '' }
        includes_greeting = expected_outputs.any? { |text| text.include?('Hello World') }
        includes_name = expected_outputs.any? { |text| text.include?('Name: TSI Student') }
        { passed: includes_greeting && includes_name, message: "HelloWorld=#{includes_greeting}, NameLine=#{includes_name}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 1 starter guides without providing the solution", total_tests, current) do
      unless cpp_ch1_exercise_path.file? && cpp_ch1_cpp_stub_path.file? && cpp_ch1_c_stub_path.file?
        { passed: false, message: "Missing exercise or starter files" }
      else
        exercise = JSON.parse(File.read(cpp_ch1_exercise_path))
        cpp_variant = exercise['variants'].find { |variant| variant['language'] == 'cpp' } || {}
        c_variant = exercise['variants'].find { |variant| variant['language'] == 'c' } || {}
        cpp_starter = (cpp_variant['starterCode'] || '').to_s
        c_starter = (c_variant['starterCode'] || '').to_s
        cpp_stub = File.read(cpp_ch1_cpp_stub_path)
        c_stub = File.read(cpp_ch1_c_stub_path)
        cpp_has_guidance = cpp_starter.include?('Output blueprint') && cpp_stub.include?('TODO')
        c_has_guidance = c_starter.include?('Output blueprint') && c_stub.include?('TODO')
  cpp_sol_free = !cpp_stub.include?('std::cout << "Hello World')
  c_sol_free = !c_stub.include?('printf("Hello World\n")')
        passed = cpp_has_guidance && c_has_guidance && cpp_sol_free && c_sol_free
        message = []
        message << 'C++ starter missing guidance' unless cpp_has_guidance
        message << 'C starter missing guidance' unless c_has_guidance
        message << 'C++ stub still prints solution' unless cpp_sol_free
        message << 'C stub still prints solution' unless c_sol_free
        { passed: passed, message: message.empty? ? 'Starter files emphasise blueprint without solutions' : message.join('; ') }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 1 solution uses std::cout without namespace pollution", total_tests, current) do
      unless cpp_ch1_solution_path.file?
        { passed: false, message: "Missing #{cpp_ch1_solution_path}" }
      else
        solution = JSON.parse(File.read(cpp_ch1_solution_path))
        cpp_variant = solution['variants'].find { |variant| variant['language'] == 'cpp' } || {}
        code = (cpp_variant['code'] || '').to_s
        uses_cout = code.include?('std::cout << "Hello World') && code.include?('std::cout << "Name: TSI Student')
        avoids_using_namespace = !code.include?('using namespace std')
        { passed: uses_cout && avoids_using_namespace, message: "uses_cout=#{uses_cout}, avoids_using_namespace=#{avoids_using_namespace}" }
      end
    end

    current += 1
    run_test_with_progress("C Chapter 1 solution uses printf blueprint", total_tests, current) do
      unless cpp_ch1_solution_path.file?
        { passed: false, message: "Missing #{cpp_ch1_solution_path}" }
      else
        solution = JSON.parse(File.read(cpp_ch1_solution_path))
        c_variant = solution['variants'].find { |variant| variant['language'] == 'c' } || {}
        code = (c_variant['code'] || '').to_s
        uses_printf = code.include?('printf("Hello World') && code.include?('printf("Name: TSI Student')
        { passed: uses_printf, message: "uses_printf=#{uses_printf}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 1 hints reinforce compile-run workflow", total_tests, current) do
      unless cpp_ch1_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch1_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch1_exercise_path))
        hints = exercise['variants'].flat_map { |variant| Array(variant['hints']) }
        mentions_printf = hints.any? { |h| h.include?('printf') }
        mentions_cout = hints.any? { |h| h.include?('std::cout') }
        { passed: mentions_printf && mentions_cout, message: "printf_hint=#{mentions_printf}, cout_hint=#{mentions_cout}" }
      end
    end

  # C++ Chapter 2 Consistency Tests (6 tests)
    cpp_ch2_exercise_path = @cpp_curriculum_dir.join('exercises', 'chapter_02_fundamental_types_exercise.json')
    cpp_ch2_solution_path = @cpp_curriculum_dir.join('solutions', 'chapter_02_fundamental_types_exercise.json')
    cpp_ch2_stub_path = @config.extension_root.join('learn_exercises', 'cpp', 'chapter_02_fundamental_types_exercise.cpp')

    current += 1
    run_test_with_progress("C++ Chapter 2 exercise metadata emphasises type reporting", total_tests, current) do
      unless cpp_ch2_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch2_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch2_exercise_path))
        tags = Array(exercise['tags'])
        tags_required = %w[types literals]
        tags_ok = (tags_required - tags).empty?
        { passed: exercise['difficulty'] == 'beginner' && tags_ok, message: "difficulty=#{exercise['difficulty']}, tags=#{tags.join(', ')}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 2 starter highlights numeric_limits usage", total_tests, current) do
      unless cpp_ch2_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch2_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch2_exercise_path))
        starter = (exercise['starterCode'] || '').to_s
        mentions_numeric_limits = starter.include?('std::numeric_limits')
        mentions_setw = starter.include?('std::setw')
        has_todos = starter.include?('TODO')
        { passed: mentions_numeric_limits && mentions_setw && has_todos, message: "numeric_limits=#{mentions_numeric_limits}, setw=#{mentions_setw}, todos=#{has_todos}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 2 hints reinforce numeric_limits", total_tests, current) do
      unless cpp_ch2_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch2_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch2_exercise_path))
        hints = Array(exercise['hints'])
        mentions_numeric_limits = hints.any? { |h| h.include?('numeric_limits') }
        mentions_setw = hints.any? { |h| h.include?('setw') }
        { passed: mentions_numeric_limits && mentions_setw, message: "numeric_limits_hint=#{mentions_numeric_limits}, setw_hint=#{mentions_setw}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 2 solution uses lowest() for doubles", total_tests, current) do
      unless cpp_ch2_solution_path.file?
        { passed: false, message: "Missing #{cpp_ch2_solution_path}" }
      else
        solution = JSON.parse(File.read(cpp_ch2_solution_path))
        variant = Array(solution['variants']).first || {}
        code = (variant['code'] || '').to_s
        uses_lowest = code.include?('std::numeric_limits<double>::lowest()')
        uses_sizeof = code.include?('sizeof(double)')
        { passed: uses_lowest && uses_sizeof, message: "uses_lowest=#{uses_lowest}, uses_sizeof=#{uses_sizeof}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 2 expected output remains intact", total_tests, current) do
      unless cpp_ch2_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch2_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch2_exercise_path))
        tests = Array(exercise['tests'])
        sample_output = tests.map { |t| t['expected'] || '' }.join("\n")
        covers_report = sample_output.include?('Type Report: C++ Fundamentals') && sample_output.include?('Pass Rate (double): 92.3')
        { passed: covers_report, message: covers_report ? 'Report lines intact' : 'Report blueprint missing' }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 2 starter stub avoids pre-written solution", total_tests, current) do
      unless cpp_ch2_stub_path.file?
        { passed: false, message: "Missing #{cpp_ch2_stub_path}" }
      else
        stub_content = File.read(cpp_ch2_stub_path)
        has_todo = stub_content.include?('TODO')
  lacks_solution_streams = !stub_content.include?('std::cout << "Type Report')
  { passed: has_todo && lacks_solution_streams, message: "has_todo=#{has_todo}, lacks_solution_streams=#{lacks_solution_streams}" }
      end
    end

    # C++ Chapter 4 Consistency Tests (6 tests)
    cpp_ch4_exercise_path = @cpp_curriculum_dir.join('exercises', 'chapter_04_making_decisions_exercise.json')
    cpp_ch4_solution_path = @cpp_curriculum_dir.join('solutions', 'chapter_04_making_decisions_exercise.json')
    cpp_ch4_stub_path = @config.extension_root.join('learn_exercises', 'cpp', 'chapter_04_making_decisions_exercise.cpp')

    current += 1
    run_test_with_progress("C++ Chapter 4 exercise metadata emphasises control flow", total_tests, current) do
      unless cpp_ch4_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch4_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch4_exercise_path))
        tags = Array(exercise['tags'])
        required_tags = %w[conditionals switch]
        has_tags = (required_tags - tags).empty?
        { passed: exercise['difficulty'] == 'beginner' && has_tags, message: "difficulty=#{exercise['difficulty']}, tags=#{tags.join(', ')}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 4 expected outputs cover access and errors", total_tests, current) do
      unless cpp_ch4_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch4_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch4_exercise_path))
        tests = Array(exercise['tests'])
        expected_outputs = tests.map { |t| t['expected'] || '' }
        includes_access = expected_outputs.any? { |text| text.include?('Access: General Lab') } &&
                          expected_outputs.any? { |text| text.include?('Access: Advanced Lab') }
        includes_error = expected_outputs.any? { |text| text.include?('Error: invalid age') }
        { passed: includes_access && includes_error, message: "access=#{includes_access}, error=#{includes_error}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 4 hints mention switch guidance", total_tests, current) do
      unless cpp_ch4_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch4_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch4_exercise_path))
        hints = Array(exercise['hints'])
        mentions_switch = hints.any? { |h| h.include?('switch') }
        mentions_role = hints.any? { |h| h.include?('Role') }
        { passed: mentions_switch && mentions_role, message: "switch_hint=#{mentions_switch}, role_hint=#{mentions_role}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 4 starter guides input order without solving", total_tests, current) do
      unless cpp_ch4_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch4_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch4_exercise_path))
        starter = (exercise['starterCode'] || '').to_s
        mentions_order = starter.include?('Read two values from stdin in this order')
        mentions_switch_instruction = starter.include?('Use a switch statement')
        has_todo = starter.include?('TODO')
        { passed: mentions_order && mentions_switch_instruction && has_todo, message: "order=#{mentions_order}, switch_instruction=#{mentions_switch_instruction}, todo=#{has_todo}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 4 stub exists with TODO placeholders", total_tests, current) do
      unless cpp_ch4_stub_path.file?
        { passed: false, message: "Missing #{cpp_ch4_stub_path}" }
      else
        stub_content = File.read(cpp_ch4_stub_path)
        has_todo = stub_content.include?('TODO')
        lacks_switch_logic = !stub_content.include?('switch (')
        lacks_output_actions = !stub_content.include?('Access: General Lab')
        { passed: has_todo && lacks_switch_logic && lacks_output_actions, message: "todo=#{has_todo}, lacks_switch=#{lacks_switch_logic}, lacks_outputs=#{lacks_output_actions}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 4 solution uses switch on role initial", total_tests, current) do
      unless cpp_ch4_solution_path.file?
        { passed: false, message: "Missing #{cpp_ch4_solution_path}" }
      else
        solution = JSON.parse(File.read(cpp_ch4_solution_path))
        variant = Array(solution['variants']).first || {}
        code = (variant['code'] || '').to_s
        uses_switch = code.include?('switch (')
        handles_error = code.include?('invalid age')
        { passed: uses_switch && handles_error, message: "uses_switch=#{uses_switch}, handles_error=#{handles_error}" }
      end
    end

    # C++ Chapter 5 Consistency Tests (6 tests)
    cpp_ch5_exercise_path = @cpp_curriculum_dir.join('exercises', 'chapter_05_arrays_and_loops_exercise.json')
    cpp_ch5_solution_path = @cpp_curriculum_dir.join('solutions', 'chapter_05_arrays_and_loops_exercise.json')
    cpp_ch5_stub_path = @config.extension_root.join('learn_exercises', 'cpp', 'chapter_05_arrays_and_loops_exercise.cpp')

    current += 1
    run_test_with_progress("C++ Chapter 5 exercise metadata emphasises arrays and loops", total_tests, current) do
      unless cpp_ch5_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch5_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch5_exercise_path))
        tags = Array(exercise['tags'])
        required_tags = %w[arrays loops]
        has_tags = (required_tags - tags).empty?
        { passed: exercise['difficulty'] == 'intermediate' && has_tags, message: "difficulty=#{exercise['difficulty']}, tags=#{tags.join(', ')}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 5 expected outputs cover array operations and loop control", total_tests, current) do
      unless cpp_ch5_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch5_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch5_exercise_path))
        tests = Array(exercise['tests'])
        expected_outputs = tests.map { |t| (t['expected'] || '').to_s }
        includes_array_sum = expected_outputs.any? { |text| text.include?('Array sum: 15') }
        includes_loop_sum = expected_outputs.any? { |text| text.include?('Sum to 10: 55') }
        includes_vector_double = expected_outputs.any? { |text| text.include?('Doubled values: 2 4 6') }
        { passed: includes_array_sum && includes_loop_sum && includes_vector_double, message: "array_sum=#{includes_array_sum}, loop_sum=#{includes_loop_sum}, vector_double=#{includes_vector_double}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 5 hints mention array bounds and loop types", total_tests, current) do
      unless cpp_ch5_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch5_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch5_exercise_path))
        hints = Array(exercise['hints'])
        mentions_bounds = hints.any? { |h| h.include?('bounds') || h.include?('std::size') }
        mentions_loops = hints.any? { |h| h.include?('for') || h.include?('while') || h.include?('range') }
        { passed: mentions_bounds && mentions_loops, message: "bounds_hint=#{mentions_bounds}, loops_hint=#{mentions_loops}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 5 starter guides array and loop usage without solving", total_tests, current) do
      unless cpp_ch5_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch5_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch5_exercise_path))
        starter = (exercise['starterCode'] || '').to_s
        mentions_arrays = starter.include?('std::array') || starter.include?('std::vector') || starter.include?('array sum')
        mentions_loops = starter.include?('for loop') || starter.include?('range-based') || starter.include?('while')
        has_todo = starter.include?('TODO')
        { passed: mentions_arrays && mentions_loops && has_todo, message: "arrays=#{mentions_arrays}, loops=#{mentions_loops}, todo=#{has_todo}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 5 stub exists with TODO placeholders", total_tests, current) do
      unless cpp_ch5_stub_path.file?
        { passed: false, message: "Missing #{cpp_ch5_stub_path}" }
      else
        stub_content = File.read(cpp_ch5_stub_path)
        todo_count = stub_content.scan('TODO').size
        returns_zero_count = stub_content.scan('return 0;').size
        has_placeholder_casts = stub_content.include?('(void)numbers') && stub_content.include?('(void)table')
        blueprint_comment = stub_content.include?('Output blueprint')
        { passed: todo_count >= 7 && returns_zero_count >= 5 && has_placeholder_casts && blueprint_comment,
          message: "todos=#{todo_count}, returns_zero=#{returns_zero_count}, placeholder_casts=#{has_placeholder_casts}, blueprint_comment=#{blueprint_comment}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 5 solution uses proper array and loop constructs", total_tests, current) do
      unless cpp_ch5_solution_path.file?
        { passed: false, message: "Missing #{cpp_ch5_solution_path}" }
      else
        solution = JSON.parse(File.read(cpp_ch5_solution_path))
        variants = Array(solution['variants'])
        code_samples = if variants.any?
                         variants.select { |variant| variant['language'] == 'cpp' }
                                 .map { |variant| variant['code'] || '' }
                       else
                         [Array(solution['cpp']).join("\n")]
                       end
        code = code_samples.join("\n")
        uses_std_array = code.include?('std::array') || code.include?('std::size(')
        uses_range_loop = code.include?('for (int&') || code.include?('for (auto&')
        uses_loop_control = code.include?('continue')
        { passed: uses_std_array && uses_range_loop && uses_loop_control,
          message: "std_array=#{uses_std_array}, range_loop=#{uses_range_loop}, loop_control=#{uses_loop_control}" }
      end
    end

    cpp_ch5_lesson_path = @cpp_curriculum_dir.join('lessons', 'chapter_05_arrays_and_loops.md')

    current += 1
    run_test_with_progress("C++ Chapter 5 lesson introduces concept map linking arrays and loops", total_tests, current) do
      unless cpp_ch5_lesson_path.file?
        { passed: false, message: "Missing #{cpp_ch5_lesson_path}" }
      else
        content = File.read(cpp_ch5_lesson_path)
        has_heading = content.include?('## Concept map')
        has_arrow_flow = content.include?('arrays -> loops -> algorithms')
        has_dual_container = content.include?('std::array -> deterministic memory') && content.include?('std::vector -> dynamic growth')
        { passed: has_heading && has_arrow_flow && has_dual_container, message: "heading=#{has_heading}, arrow_flow=#{has_arrow_flow}, dual_container=#{has_dual_container}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 5 lesson warns about misleading array parameter sizes", total_tests, current) do
      unless cpp_ch5_lesson_path.file?
        { passed: false, message: "Missing #{cpp_ch5_lesson_path}" }
      else
        content = File.read(cpp_ch5_lesson_path)
        mentions_signature = content.include?('average10(double array[10])')
        mentions_false_expectation = content.include?('false expectation') || content.include?('false expectations')
        emphasises_pointer_equivalence = content.include?('array parameter collapses to a pointer')
        { passed: mentions_signature && mentions_false_expectation && emphasises_pointer_equivalence, message: "signature=#{mentions_signature}, expectation=#{mentions_false_expectation}, pointer_equivalence=#{emphasises_pointer_equivalence}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 5 lesson supplies offline code examples for arrays and loops", total_tests, current) do
      unless cpp_ch5_lesson_path.file?
        { passed: false, message: "Missing #{cpp_ch5_lesson_path}" }
      else
        content = File.read(cpp_ch5_lesson_path)
        example_count = content.scan('Example:').size
        mentions_rolling_average = content.include?('Example: rolling_average.cpp')
        mentions_temperature_conversion = content.include?('Example: temperature_log.cpp')
        { passed: example_count >= 3 && mentions_rolling_average && mentions_temperature_conversion, message: "example_count=#{example_count}, rolling_average=#{mentions_rolling_average}, temperature_conversion=#{mentions_temperature_conversion}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 5 lesson reinforces do/while practice with offline drills", total_tests, current) do
      unless cpp_ch5_lesson_path.file?
        { passed: false, message: "Missing #{cpp_ch5_lesson_path}" }
      else
        content = File.read(cpp_ch5_lesson_path)
        mentions_do_while = content.include?('do/while rehearsal') || content.include?('do/while loop rehearsal')
        mentions_offline = content.include?('offline logbook drill')
        ties_to_range_based = content.include?('range-based for loop keeps the code efficient')
        { passed: mentions_do_while && mentions_offline && ties_to_range_based, message: "do_while=#{mentions_do_while}, offline=#{mentions_offline}, range_based=#{ties_to_range_based}" }
      end
    end

    cpp_ch6_lesson_path = @cpp_curriculum_dir.join('lessons', 'chapter_06_pointers_and_references.md')

    current += 1
    run_test_with_progress("C++ Chapter 6 lesson maps pointer families", total_tests, current) do
      unless cpp_ch6_lesson_path.file?
        { passed: false, message: "Missing #{cpp_ch6_lesson_path}" }
      else
        content = File.read(cpp_ch6_lesson_path)
        has_heading = content.include?('## Concept map')
        mentions_pointer_flow = content.include?('pointers -> addresses -> indirection')
        mentions_reference_flow = content.include?('references -> aliases -> stability') || content.include?('references -> aliases -> safety')
        mentions_smart_flow = content.include?('smart pointers -> RAII -> ownership')
        { passed: has_heading && mentions_pointer_flow && mentions_reference_flow && mentions_smart_flow,
          message: "heading=#{has_heading}, pointer_flow=#{mentions_pointer_flow}, reference_flow=#{mentions_reference_flow}, smart_flow=#{mentions_smart_flow}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 6 lesson explains pointer constness contrasts", total_tests, current) do
      unless cpp_ch6_lesson_path.file?
        { passed: false, message: "Missing #{cpp_ch6_lesson_path}" }
      else
        content = File.read(cpp_ch6_lesson_path)
        mentions_pointer_to_const = content.include?('const double*') || content.include?('const double\*')
        mentions_const_pointer = content.include?('double* const') || content.include?('double\* const')
        mentions_reference_alias = content.include?('reference as an alias') || content.include?('reference acts as an alias')
        { passed: mentions_pointer_to_const && mentions_const_pointer && mentions_reference_alias,
          message: "pointer_to_const=#{mentions_pointer_to_const}, const_pointer=#{mentions_const_pointer}, reference_alias=#{mentions_reference_alias}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 6 lesson highlights pointer arithmetic with std::ptrdiff_t", total_tests, current) do
      unless cpp_ch6_lesson_path.file?
        { passed: false, message: "Missing #{cpp_ch6_lesson_path}" }
      else
        content = File.read(cpp_ch6_lesson_path)
        mentions_ptrdiff = content.include?('std::ptrdiff_t')
        mentions_pointer_difference = content.include?('pointer difference') || content.include?('pointer differences') || content.include?('subtracting pointers')
        emphasises_same_array = content.include?('same array') || content.include?('same underlying array')
        { passed: mentions_ptrdiff && mentions_pointer_difference && emphasises_same_array,
          message: "ptrdiff=#{mentions_ptrdiff}, pointer_difference=#{mentions_pointer_difference}, same_array=#{emphasises_same_array}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 6 lesson provides offline pointer practice examples", total_tests, current) do
      unless cpp_ch6_lesson_path.file?
        { passed: false, message: "Missing #{cpp_ch6_lesson_path}" }
      else
        content = File.read(cpp_ch6_lesson_path)
        example_count = content.scan('Example:').size
        includes_pointer_diary = content.include?('Example: pointer_diary.cpp')
        includes_lifetime_tracker = content.include?('Example: lifetime_tracker.cpp')
        includes_reference_alias = content.include?('Example: reference_aliases.cpp')
        { passed: example_count >= 4 && includes_pointer_diary && includes_lifetime_tracker && includes_reference_alias,
          message: "example_count=#{example_count}, pointer_diary=#{includes_pointer_diary}, lifetime_tracker=#{includes_lifetime_tracker}, reference_alias=#{includes_reference_alias}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 6 lesson reconnects shared_ptr temperature log", total_tests, current) do
      unless cpp_ch6_lesson_path.file?
        { passed: false, message: "Missing #{cpp_ch6_lesson_path}" }
      else
        content = File.read(cpp_ch6_lesson_path)
        mentions_shared_ptr = content.include?('shared_ptr')
        mentions_vector_temperature = content.include?('vector') && content.include?('temperature log')
        { passed: mentions_shared_ptr && mentions_vector_temperature,
          message: "shared_ptr=#{mentions_shared_ptr}, vector_temperature=#{mentions_vector_temperature}" }
      end
    end

    cpp_ch6_exercise_path = @cpp_curriculum_dir.join('exercises', 'chapter_06_pointers_and_references_exercise.json')
    cpp_ch6_solution_path = @cpp_curriculum_dir.join('solutions', 'chapter_06_pointers_and_references_exercise.json')
    cpp_ch6_stub_path = @config.extension_root.join('learn_exercises', 'cpp', 'chapter_06_pointers_and_references_exercise.cpp')

    current += 1
    run_test_with_progress("C++ Chapter 6 exercise metadata highlights pointers and references", total_tests, current) do
      unless cpp_ch6_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch6_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch6_exercise_path))
        difficulty_ok = exercise['difficulty'] == 'advanced' || exercise['difficulty'] == 'intermediate'
        tags = Array(exercise['tags'])
        has_pointer_tags = tags.include?('pointers') && tags.include?('references')
        { passed: difficulty_ok && has_pointer_tags, message: "difficulty=#{exercise['difficulty']}, tags=#{tags.join(', ')}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 6 exercise expected output summarises pointer results", total_tests, current) do
      unless cpp_ch6_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch6_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch6_exercise_path))
        tests = Array(exercise['tests'])
        expected = tests.map { |t| t['expected'] || '' }.join("\n")
        includes_span = expected.include?('Pointer span:')
        includes_reseat = expected.include?('Pointer value after reseat:')
        includes_alias = expected.include?('Alias result:')
        includes_shared = expected.include?('Shared owners (before reset):')
        includes_weak = expected.include?('Weak expired after reset:')
        { passed: includes_span && includes_reseat && includes_alias && includes_shared && includes_weak,
          message: "span=#{includes_span}, reseat=#{includes_reseat}, alias=#{includes_alias}, shared=#{includes_shared}, weak=#{includes_weak}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 6 exercise hints emphasise pointer safety", total_tests, current) do
      unless cpp_ch6_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch6_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch6_exercise_path))
        hints = Array(exercise['hints'])
        mentions_ptrdiff = hints.any? { |h| h.include?('std::ptrdiff_t') }
        mentions_shared_ptr = hints.any? { |h| h.include?('shared_ptr') }
        mentions_alias = hints.any? { |h| h.include?('reference') }
        { passed: mentions_ptrdiff && mentions_shared_ptr && mentions_alias,
          message: "ptrdiff_hint=#{mentions_ptrdiff}, shared_ptr_hint=#{mentions_shared_ptr}, alias_hint=#{mentions_alias}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 6 starter blueprint outlines pointer tasks", total_tests, current) do
      unless cpp_ch6_exercise_path.file?
        { passed: false, message: "Missing #{cpp_ch6_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_ch6_exercise_path))
        starter = (exercise['starterCode'] || '').to_s
        has_todo = starter.include?('TODO')
        mentions_span = starter.include?('Pointer span: <value>')
        mentions_shared = starter.include?('Shared owners (before reset): <value>')
        mentions_weak = starter.include?('Weak expired after reset: <yes/no>')
        { passed: has_todo && mentions_span && mentions_shared && mentions_weak,
          message: "todo=#{has_todo}, span=#{mentions_span}, shared=#{mentions_shared}, weak=#{mentions_weak}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 6 stub guides pointer implementation", total_tests, current) do
      unless cpp_ch6_stub_path.file?
        { passed: false, message: "Missing #{cpp_ch6_stub_path}" }
      else
        stub_content = File.read(cpp_ch6_stub_path)
        has_todo = stub_content.include?('TODO')
        blueprint_comment = stub_content.include?('Pointer span: <value>')
        mentions_ptrdiff = stub_content.include?('std::ptrdiff_t')
        mentions_shared_ptr = stub_content.include?('std::shared_ptr')
        { passed: has_todo && blueprint_comment && mentions_ptrdiff && mentions_shared_ptr,
          message: "todo=#{has_todo}, blueprint=#{blueprint_comment}, ptrdiff=#{mentions_ptrdiff}, shared_ptr=#{mentions_shared_ptr}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 6 solution uses pointer arithmetic and shared ownership", total_tests, current) do
      unless cpp_ch6_solution_path.file?
        { passed: false, message: "Missing #{cpp_ch6_solution_path}" }
      else
        solution = JSON.parse(File.read(cpp_ch6_solution_path))
        variants = Array(solution['variants'])
        code = variants.map { |variant| variant['code'] }.join("\n")
        uses_ptrdiff = code.include?('std::ptrdiff_t')
        uses_shared = code.include?('std::shared_ptr')
        uses_weak = code.include?('std::weak_ptr')
        reseats_pointer = code.include?('reseat_pointer(')
        { passed: uses_ptrdiff && uses_shared && uses_weak && reseats_pointer,
          message: "ptrdiff=#{uses_ptrdiff}, shared=#{uses_shared}, weak=#{uses_weak}, reseat=#{reseats_pointer}" }
      end
    end

    # C++ Curriculum Sanity Tests (6 tests)
    cpp_exercise_path = @cpp_curriculum_dir.join('exercises', 'chapter_03_working_with_data_exercise.json')
    cpp_solution_path = @cpp_curriculum_dir.join('solutions', 'chapter_03_working_with_data_exercise.json')
    cpp_starter_path = @config.extension_root.join('learn_exercises', 'cpp', 'chapter_03_working_with_data_exercise.cpp')

    current += 1
    run_test_with_progress("C++ curriculum directory exists", total_tests, current) do
      { passed: @cpp_curriculum_dir.directory?, message: @cpp_curriculum_dir.to_s }
    end

    current += 1
    run_test_with_progress("C++ Chapter 3 exercise marked beginner with bitmask tags", total_tests, current) do
      unless cpp_exercise_path.file?
        { passed: false, message: "Missing #{cpp_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_exercise_path))
        difficulty_ok = exercise['difficulty'] == 'beginner'
        required_tags = %w[bitwise masks]
        tags = Array(exercise['tags'])
        tags_ok = (required_tags - tags).empty?
        { passed: difficulty_ok && tags_ok, message: "difficulty=#{exercise['difficulty']}, tags=#{tags.join(', ')}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 3 starter highlights sensor masks", total_tests, current) do
      unless cpp_exercise_path.file? && cpp_starter_path.file?
        { passed: false, message: "Missing exercise or starter file" }
      else
        exercise = JSON.parse(File.read(cpp_exercise_path))
        starter = exercise['starterCode'] || ''
        starter_includes_masks = starter.include?('FRONT_DOOR_MASK') && starter.include?('Garage Door Open')
        starter_excludes_enum = !starter.include?('enum class')
        stub_content = File.read(cpp_starter_path)
        stub_mentions_masks = stub_content.include?('FRONT_DOOR_MASK') && stub_content.include?('bitwise AND')
        stub_excludes_enum = !stub_content.include?('enum class')
        passed = starter_includes_masks && starter_excludes_enum && stub_mentions_masks && stub_excludes_enum
        message = []
        message << "starter missing mask hints" unless starter_includes_masks
        message << "starter still mentions enum" unless starter_excludes_enum
        message << "stub missing mask guidance" unless stub_mentions_masks
        message << "stub still mentions enum" unless stub_excludes_enum
        { passed: passed, message: message.empty? ? 'Starter and stub emphasise masks without enums' : message.join('; ') }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 3 exercise tests use door wording", total_tests, current) do
      unless cpp_exercise_path.file?
        { passed: false, message: "Missing #{cpp_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_exercise_path))
        tests = Array(exercise['tests'])
        expected_samples = tests.map { |t| t['expected'] || '' }
        includes_front = expected_samples.any? { |text| text.include?('Front Door Open') }
        includes_garage = expected_samples.any? { |text| text.include?('Garage Door Open') }
        excludes_student = expected_samples.none? { |text| text.include?('Student Access') }
        passed = includes_front && includes_garage && excludes_student
        message = "includes_front=#{includes_front}, includes_garage=#{includes_garage}, excludes_student=#{excludes_student}"
        { passed: passed, message: message }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 3 hints cover masks and bitset", total_tests, current) do
      unless cpp_exercise_path.file?
        { passed: false, message: "Missing #{cpp_exercise_path}" }
      else
        exercise = JSON.parse(File.read(cpp_exercise_path))
        hints = Array(exercise['hints'])
        mentions_mask = hints.any? { |h| h.include?('mask') }
        mentions_bitset = hints.any? { |h| h.include?('bitset') }
        { passed: mentions_mask && mentions_bitset, message: "mask_hint=#{mentions_mask}, bitset_hint=#{mentions_bitset}" }
      end
    end

    current += 1
    run_test_with_progress("C++ Chapter 3 solution uses constexpr masks", total_tests, current) do
      unless cpp_solution_path.file?
        { passed: false, message: "Missing #{cpp_solution_path}" }
      else
        solution = JSON.parse(File.read(cpp_solution_path))
        variant = Array(solution['variants']).first || {}
        code = (variant['code'] || '').to_s
        uses_masks = code.include?('constexpr unsigned int FRONT_DOOR_MASK') && code.include?('sensor_active')
        excludes_enum = !code.include?('enum class')
        { passed: uses_masks && excludes_enum, message: "uses_masks=#{uses_masks}, excludes_enum=#{excludes_enum}" }
      end
    end

    # Progress Tracking Tests (4 tests)
    current += 1
    run_test_with_progress("ProgressTracker has achievement system", total_tests, current) do
      file = @lib_dir.join('progress_tracker.js')
      content = file.file? ? File.read(file) : ""
      has_achievements = content.include?('checkAchievements') || content.include?('achievements')
      { passed: has_achievements, message: has_achievements ? "Achievement system found" : "Achievement system not found" }
    end
    
    current += 1
    run_test_with_progress("ProgressTracker handles streaks", total_tests, current) do
      file = @lib_dir.join('progress_tracker.js')
      content = file.file? ? File.read(file) : ""
      has_streaks = content.include?('streak') || content.include?('updateStreak')
      { passed: has_streaks, message: has_streaks ? "Streak tracking found" : "Streak tracking not found" }
    end
    
    current += 1
    run_test_with_progress("ProgressTracker has statistics", total_tests, current) do
      file = @lib_dir.join('progress_tracker.js')
      content = file.file? ? File.read(file) : ""
      has_stats = content.include?('getStats') || content.include?('statistics')
      { passed: has_stats, message: has_stats ? "Statistics found" : "Statistics not found" }
    end
    
    current += 1
    run_test_with_progress("ProgressTracker can reset progress", total_tests, current) do
      file = @lib_dir.join('progress_tracker.js')
      content = file.file? ? File.read(file) : ""
      has_reset = content.include?('resetProgress') || content.include?('reset')
      { passed: has_reset, message: has_reset ? "Reset functionality found" : "Reset functionality not found" }
    end
    
    # Exercise Runner Tests (6 tests)
    current += 1
    run_test_with_progress("ExerciseRunner supports Ruby", total_tests, current) do
      file = @lib_dir.join('exercise_runner.js')
      content = file.file? ? File.read(file) : ""
      supports_ruby = content.include?('runRubyTests') || content.include?('ruby')
      { passed: supports_ruby, message: supports_ruby ? "Ruby support found" : "Ruby support not found" }
    end
    
    current += 1
    run_test_with_progress("ExerciseRunner supports Python", total_tests, current) do
      file = @lib_dir.join('exercise_runner.js')
      content = file.file? ? File.read(file) : ""
      supports_python = content.include?('runPythonTests') || content.include?('python')
      { passed: supports_python, message: supports_python ? "Python support found" : "Python support not found" }
    end
    
    current += 1
    run_test_with_progress("ExerciseRunner supports JavaScript", total_tests, current) do
      file = @lib_dir.join('exercise_runner.js')
      content = file.file? ? File.read(file) : ""
      supports_js = content.include?('runJavaScriptTests') || content.include?('javascript')
      { passed: supports_js, message: supports_js ? "JavaScript support found" : "JavaScript support not found" }
    end
    
    current += 1
    run_test_with_progress("ExerciseRunner handles output tests", total_tests, current) do
      file = @lib_dir.join('exercise_runner.js')
      content = file.file? ? File.read(file) : ""
      handles_output = content.include?('StringIO') || content.include?('output') || content.include?('stdout')
      { passed: handles_output, message: handles_output ? "Output testing found" : "Output testing not found" }
    end
    
    current += 1
    run_test_with_progress("ExerciseRunner provides detailed feedback", total_tests, current) do
      file = @lib_dir.join('exercise_runner.js')
      content = file.file? ? File.read(file) : ""
      detailed_feedback = content.include?('parseRubyTestOutput') || content.include?('parseTestOutput') || content.include?('failures')
      { passed: detailed_feedback, message: detailed_feedback ? "Detailed feedback found" : "Detailed feedback not found" }
    end
    
    current += 1
    run_test_with_progress("ExerciseRunner handles manual exercises", total_tests, current) do
      file = @lib_dir.join('exercise_runner.js')
      content = file.file? ? File.read(file) : ""
      manual_exercises = content.include?('isManual') || content.include?('manual')
      { passed: manual_exercises, message: manual_exercises ? "Manual exercise support found" : "Manual exercise support not found" }
    end
    
    # Learn Manager Tests (5 tests)
    current += 1
    run_test_with_progress("LearnManager can open lessons", total_tests, current) do
      file = @lib_dir.join('learn_manager.js')
      content = file.file? ? File.read(file) : ""
      can_open = content.include?('openLesson') || content.include?('createWebviewPanel')
      { passed: can_open, message: can_open ? "Lesson opening found" : "Lesson opening not found" }
    end
    
    current += 1
    run_test_with_progress("LearnManager handles exercise starting", total_tests, current) do
      file = @lib_dir.join('learn_manager.js')
      content = file.file? ? File.read(file) : ""
      handles_exercises = content.include?('startExercise') || content.include?('openTextDocument')
      { passed: handles_exercises, message: handles_exercises ? "Exercise handling found" : "Exercise handling not found" }
    end
    
    current += 1
    run_test_with_progress("LearnManager provides file extensions", total_tests, current) do
      file = @lib_dir.join('learn_manager.js')
      content = file.file? ? File.read(file) : ""
      file_extensions = content.include?('getFileExtension') || content.include?('.rb') || content.include?('.py')
      { passed: file_extensions, message: file_extensions ? "File extension support found" : "File extension support not found" }
    end
    
    current += 1
    run_test_with_progress("LearnManager handles lesson completion", total_tests, current) do
      file = @lib_dir.join('learn_manager.js')
      content = file.file? ? File.read(file) : ""
      lesson_completion = content.include?('completeLesson') || content.include?('markComplete')
      { passed: lesson_completion, message: lesson_completion ? "Lesson completion found" : "Lesson completion not found" }
    end
    
    current += 1
    run_test_with_progress("LearnManager can load solutions", total_tests, current) do
      file = @lib_dir.join('learn_manager.js')
      content = file.file? ? File.read(file) : ""
      load_solutions = content.include?('loadSolution') || content.include?('solution')
      { passed: load_solutions, message: load_solutions ? "Solution loading found" : "Solution loading not found" }
    end
    
    # Main Learn Class Tests (4 tests)
    current += 1
    run_test_with_progress("Main Learn class exists", total_tests, current) do
      file = @learn_dir.join('index.js')
      content = file.file? ? File.read(file) : ""
      has_class = content.include?('class Learn')
      { passed: has_class, message: has_class ? "Learn class found" : "Learn class not found" }
    end
    
    current += 1
    run_test_with_progress("Learn class orchestrates learning", total_tests, current) do
      file = @learn_dir.join('index.js')
      content = file.file? ? File.read(file) : ""
      orchestrates = content.include?('startLearning') || content.include?('runExercise')
      { passed: orchestrates, message: orchestrates ? "Learning orchestration found" : "Learning orchestration not found" }
    end
    
    current += 1
    run_test_with_progress("Learn class provides navigation", total_tests, current) do
      file = @learn_dir.join('index.js')
      content = file.file? ? File.read(file) : ""
      navigation = content.include?('browseLessons') || content.include?('reviewLessons')
      { passed: navigation, message: navigation ? "Navigation features found" : "Navigation features not found" }
    end
    
    current += 1
    run_test_with_progress("Learn class shows solutions", total_tests, current) do
      file = @learn_dir.join('index.js')
      content = file.file? ? File.read(file) : ""
      solutions = content.include?('showSolution') || content.include?('solution')
      { passed: solutions, message: solutions ? "Solution display found" : "Solution display not found" }
    end
    
    end_progress_bar
  end
end

# Run if executed directly
if __FILE__ == $0
  config = TestSuiteConfig.new
  results = TestResults.new
  test = TestLearnModule.new(config, results)
  test.run
end
