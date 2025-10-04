#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'pathname'

repo_root = Pathname.new(__dir__).parent.expand_path
dsa_root = repo_root.join('learn', 'curriculum', 'dsa_cpp')
curriculum_path = dsa_root.join('curriculum.json')

abort('Missing DSA C++ curriculum.json') unless curriculum_path.exist?

expected_structure = {
  'dsa_foundations_cpp' => %w[
    dsa_intro_cpp
    dsa_simple_algorithms_cpp
    dsa_core_data_structures_cpp
  ],
  'dsa_arrays_cpp' => %w[
    dsa_arrays_analysis_cpp
  ],
  'dsa_linked_lists_cpp' => %w[
    dsa_linked_lists_cpp
  ],
  'dsa_stacks_queues_cpp' => %w[
    dsa_stacks_queues_cpp
  ],
  'dsa_hash_tables_cpp' => %w[
    dsa_hash_tables_cpp
  ]
}

data = JSON.parse(curriculum_path.read)
modules = data['modules'] || []

expected_structure.each do |module_id, lesson_ids|
  module_data = modules.find { |mod| mod['id'] == module_id }
  raise "Module #{module_id} is missing from the C++ DSA curriculum" unless module_data

  actual_lesson_ids = (module_data['lessons'] || []).map { |lesson| lesson['id'] }
  unless actual_lesson_ids == lesson_ids
    raise "Module #{module_id} lessons mismatch. Expected #{lesson_ids.join(', ')}, got #{actual_lesson_ids.join(', ')}"
  end

  lesson_ids.each do |lesson_id|
    lesson_path = dsa_root.join('lessons', "#{lesson_id}.md")
    raise "Lesson markdown missing for #{lesson_id}" unless lesson_path.exist?

    lesson_content = lesson_path.read
    total_lines = lesson_content.each_line.count
    if total_lines < 500
      raise "Lesson #{lesson_id} must contain at least 500 lines of content (found #{total_lines})"
    end
    unless lesson_content.include?('![')
      raise "Lesson #{lesson_id} must include at least one embedded image or diagram"
    end

    code_lines = 0
    inside_cpp_block = false
    lesson_content.each_line do |line|
      stripped = line.strip
      if stripped.start_with?('```')
        if inside_cpp_block
          inside_cpp_block = false
        else
          language = stripped.delete_prefix('```').strip.downcase
          inside_cpp_block = %w[cpp c++].include?(language)
        end
        next
      end

      code_lines += 1 if inside_cpp_block
    end

    if code_lines < 300
      raise "Lesson #{lesson_id} must include at least 300 lines of C++ code examples (found #{code_lines})"
    end

    exercise_path = dsa_root.join('exercises', "#{lesson_id}_exercise.json")
    raise "Exercise JSON missing for #{lesson_id}" unless exercise_path.exist?
    exercise = JSON.parse(exercise_path.read)

    mode = exercise['mode'] || exercise['type']
    raise "Exercise for #{lesson_id} must be a quiz" unless mode == 'quiz'

    questions = exercise['questions'] || []
    raise "Exercise for #{lesson_id} must contain exactly 10 questions" unless questions.length == 10

    solution_path = dsa_root.join('solutions', "#{lesson_id}_exercise.json")
    raise "Solution JSON missing for #{lesson_id}" unless solution_path.exist?
    solution = JSON.parse(solution_path.read)

    solution_mode = solution['mode'] || solution['type']
    raise "Solution for #{lesson_id} must declare quiz mode" unless solution_mode == 'quiz'

    answer_key = solution['answerKey'] || []
    raise "Solution for #{lesson_id} must supply an answer key" if answer_key.empty?
    raise "Solution for #{lesson_id} must provide exactly 10 answer key entries" unless answer_key.length == 10

    key_points = solution['keyPoints'] || []
    raise "Solution for #{lesson_id} must include key points" if key_points.empty?
  end
end

puts '✅ C++ DSA roadmap structure validated'