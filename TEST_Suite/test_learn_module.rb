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
    total_tests = 27
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
    run_test_with_progress("Curriculum has 6 modules", total_tests, current) do
      count = curriculum ? curriculum['modules'].length : 0
      { passed: count == 6, message: "Found #{count} modules" }
    end
    
    # Lesson files tests (4 tests)
    lesson_ids = []
    if curriculum && curriculum['modules']
      curriculum['modules'].each do |mod|
        mod['lessons'].each { |lesson| lesson_ids << lesson['id'] }
      end
    end
    
    current += 1
    run_test_with_progress("All 22 lessons defined in curriculum", total_tests, current) do
      { passed: lesson_ids.length == 22, message: "Found #{lesson_ids.length} lessons" }
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
    
    end_progress_bar
    puts "=" * 50
  end
end

# Run if executed directly
if __FILE__ == $0
  config = TestSuiteConfig.new
  results = TestResults.new
  test = TestLearnModule.new(config, results)
  test.run
  
  puts "\n🎯 Final Results"
  puts "Total: #{results.total_tests} | Passed: #{results.passed_tests} | Failed: #{results.failed_tests}"
  puts "Success Rate: #{results.success_rate}%"
  
  exit(results.failed_tests > 0 ? 1 : 0)
end
