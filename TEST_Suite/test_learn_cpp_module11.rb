#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'pathname'

repo_root = Pathname.new(__dir__).parent.expand_path
cpp_root = repo_root.join('learn', 'curriculum', 'cpp')
curriculum_path = cpp_root.join('curriculum.json')

abort('Missing C++ curriculum.json') unless curriculum_path.exist?

data = JSON.parse(curriculum_path.read)
modules = data['modules'] || []

module_11 = modules.find { |mod| mod['id'] == 'advanced_data_structures_cpp' }
raise 'Module 11 (advanced_data_structures_cpp) is missing' unless module_11

lesson_ids = (module_11['lessons'] || []).map { |lesson| lesson['id'] }
expected_lessons = %w[priority_queues_intro_cpp priority_queues_heaps_cpp union_find_disjoint_sets_cpp]
raise "Module 11 lessons mismatch. Expected #{expected_lessons.join(', ')}, got #{lesson_ids.join(', ')}" unless lesson_ids == expected_lessons

lesson_path = cpp_root.join('lessons', 'union_find_disjoint_sets_cpp.md')
raise 'Lesson markdown missing for union_find_disjoint_sets_cpp' unless lesson_path.exist?

lesson_content = lesson_path.read
raise 'Lesson must include a Practice Time section' unless lesson_content.include?('## Practice Time')

exercise_path = cpp_root.join('exercises', 'union_find_disjoint_sets_cpp_exercise.json')
raise 'Exercise file missing for union_find_disjoint_sets_cpp' unless exercise_path.exist?
exercise = JSON.parse(exercise_path.read)
raise 'Exercise starterCode must include TODO guidance' unless exercise['starterCode'].is_a?(String) && exercise['starterCode'].include?('TODO')
raise 'Exercise must provide tests' if (exercise['tests'] || []).empty?

solution_path = cpp_root.join('solutions', 'union_find_disjoint_sets_cpp_exercise.json')
raise 'Solution file missing for union_find_disjoint_sets_cpp' unless solution_path.exist?
solution = JSON.parse(solution_path.read)
raise 'Solution must reference exercise id' unless solution['exerciseId'] == 'union_find_disjoint_sets_cpp_exercise'
raise 'Solution explanation must be present' unless solution['explanation'].is_a?(String) && !solution['explanation'].strip.empty?

puts '✅ Module 11 union-find lesson bundle validated'
