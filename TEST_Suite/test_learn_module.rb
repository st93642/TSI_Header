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
    @lib_dir = @learn_dir.join('lib')
    
    # Total tests count
    total_tests = 57
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
    run_test_with_progress("Curriculum has 7 modules", total_tests, current) do
      count = curriculum ? curriculum['modules'].length : 0
      { passed: count == 7, message: "Found #{count} modules" }
    end
    
    # Lesson files tests (4 tests)
    lesson_ids = []
    if curriculum && curriculum['modules']
      curriculum['modules'].each do |mod|
        mod['lessons'].each { |lesson| lesson_ids << lesson['id'] }
      end
    end
    
    current += 1
    run_test_with_progress("All 29 lessons defined in curriculum", total_tests, current) do
      { passed: lesson_ids.length == 29, message: "Found #{lesson_ids.length} lessons" }
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
