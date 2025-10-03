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
expected_lessons = %w[priority_queues_intro_cpp priority_queues_heaps_cpp union_find_disjoint_sets_cpp graph_traversal_dfs_bfs_cpp shortest_paths_dijkstra_bellman_ford_cpp]
raise "Module 11 lessons mismatch. Expected #{expected_lessons.join(', ')}, got #{lesson_ids.join(', ')}" unless lesson_ids == expected_lessons

lesson_path = cpp_root.join('lessons', 'graph_traversal_dfs_bfs_cpp.md')
raise 'Lesson markdown missing for graph_traversal_dfs_bfs_cpp' unless lesson_path.exist?

lesson_content = lesson_path.read
raise 'Lesson must include a Practice Time section' unless lesson_content.include?('## Practice Time')

exercise_path = cpp_root.join('exercises', 'graph_traversal_dfs_bfs_cpp_exercise.json')
raise 'Exercise file missing for graph_traversal_dfs_bfs_cpp' unless exercise_path.exist?
exercise = JSON.parse(exercise_path.read)
unless exercise['mode'] == 'quiz'
  raise 'Exercise starterCode must include TODO guidance' unless exercise['starterCode'].is_a?(String) && exercise['starterCode'].include?('TODO')
end
raise 'Exercise must provide tests' if (exercise['tests'] || []).empty? && exercise['mode'] != 'quiz'

solution_path = cpp_root.join('solutions', 'graph_traversal_dfs_bfs_cpp_exercise.json')
raise 'Solution file missing for graph_traversal_dfs_bfs_cpp' unless solution_path.exist?
solution = JSON.parse(solution_path.read)
raise 'Solution must reference exercise id' unless solution['exerciseId'] == 'graph_traversal_dfs_bfs_cpp_exercise'
raise 'Solution explanation must be present' unless solution['explanation'].is_a?(String) && !solution['explanation'].strip.empty?

union_find_exercise_path = cpp_root.join('exercises', 'union_find_disjoint_sets_cpp_exercise.json')
raise 'Exercise file missing for union_find_disjoint_sets_cpp' unless union_find_exercise_path.exist?
union_find_exercise = JSON.parse(union_find_exercise_path.read)
raise 'Union-Find exercise must be a quiz' unless union_find_exercise['mode'] == 'quiz'
questions = union_find_exercise['questions'] || []
raise 'Union-Find quiz must contain at least 8 questions' if questions.length < 8
raise 'Union-Find quiz must define a numeric passScore' unless union_find_exercise['passScore'].is_a?(Numeric)

union_find_solution_path = cpp_root.join('solutions', 'union_find_disjoint_sets_cpp_exercise.json')
raise 'Solution file missing for union_find_disjoint_sets_cpp' unless union_find_solution_path.exist?
union_find_solution = JSON.parse(union_find_solution_path.read)
raise 'Union-Find solution must reference exercise id' unless union_find_solution['exerciseId'] == 'union_find_disjoint_sets_cpp_exercise'
raise 'Union-Find solution must specify quiz mode' unless union_find_solution['mode'] == 'quiz'
answer_key = union_find_solution['answerKey'] || []
raise 'Union-Find quiz solution must provide answer key entries' if answer_key.empty?
question_ids = questions.map { |q| q['id'] }
answer_ids = answer_key.map { |entry| entry['questionId'] }
missing = question_ids - answer_ids
raise "Union-Find quiz solution missing answers for: #{missing.join(', ')}" unless missing.empty?
raise 'Union-Find quiz solution must include key points' unless (union_find_solution['keyPoints'] || []).any?

shortest_paths_lesson_path = cpp_root.join('lessons', 'shortest_paths_dijkstra_bellman_ford_cpp.md')
raise 'Lesson markdown missing for shortest_paths_dijkstra_bellman_ford_cpp' unless shortest_paths_lesson_path.exist?

shortest_paths_lesson_content = shortest_paths_lesson_path.read
raise 'Lesson must include a Practice Time section' unless shortest_paths_lesson_content.include?('## Practice Time')

shortest_paths_exercise_path = cpp_root.join('exercises', 'shortest_paths_dijkstra_bellman_ford_cpp_exercise.json')
raise 'Exercise file missing for shortest_paths_dijkstra_bellman_ford_cpp' unless shortest_paths_exercise_path.exist?
shortest_paths_exercise = JSON.parse(shortest_paths_exercise_path.read)
raise 'Shortest paths exercise must be a quiz' unless shortest_paths_exercise['type'] == 'quiz'
questions = shortest_paths_exercise['questions'] || []
raise 'Shortest paths quiz must contain at least 8 questions' if questions.length < 8

shortest_paths_solution_path = cpp_root.join('solutions', 'shortest_paths_dijkstra_bellman_ford_cpp_exercise.json')
raise 'Solution file missing for shortest_paths_dijkstra_bellman_ford_cpp' unless shortest_paths_solution_path.exist?
shortest_paths_solution = JSON.parse(shortest_paths_solution_path.read)
raise 'Shortest paths solution must reference exercise id' unless shortest_paths_solution['id'] == 'shortest_paths_dijkstra_bellman_ford_cpp_exercise'
raise 'Shortest paths solution must specify quiz type' unless shortest_paths_solution['type'] == 'quiz'
answer_key = shortest_paths_solution['answerKey'] || []
raise 'Shortest paths quiz solution must provide answer key' if answer_key.empty?
raise 'Shortest paths quiz solution must include key points' unless (shortest_paths_solution['keyPoints'] || []).any?

puts '✅ Module 11 shortest paths lesson bundle validated'
