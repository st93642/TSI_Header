#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'pathname'

repo_root = Pathname.new(__dir__).parent.expand_path
dsa_root = repo_root.join('learn', 'curriculum', 'dsa_cpp')
resources_root = repo_root.join('resources', 'dsa_cpp')

curriculum_path = dsa_root.join('curriculum.json')
abort('Missing DSA curriculum.json') unless curriculum_path.exist?

curriculum = JSON.parse(curriculum_path.read)
modules = curriculum.fetch('modules', [])

lesson_ids = modules.flat_map do |mod|
  lessons = mod.fetch('lessons', [])
  lessons.map { |lesson| lesson.fetch('id') }
end

allowed_types = %w[single multiple truefalse]

lesson_ids.each do |lesson_id|
  lesson_path = dsa_root.join('lessons', "#{lesson_id}.md")
  raise "Lesson markdown missing for #{lesson_id}" unless lesson_path.exist?

  markdown = lesson_path.read
  markdown.scan(/!\[[^\]]*\]\(([^)]+)\)/).each do |match|
    rel_path = Array(match).first
    next if rel_path.nil? || rel_path.strip.empty?

    resource_path = lesson_path.dirname.join(rel_path).cleanpath
    unless resource_path.to_s.start_with?(resources_root.to_s)
      raise "Lesson #{lesson_id} references asset outside resources/dsa_cpp: #{rel_path}"
    end
    raise "Missing image asset referenced by #{lesson_id}: #{resource_path}" unless resource_path.exist?
  end

  exercise_path = dsa_root.join('exercises', "#{lesson_id}_exercise.json")
  raise "Exercise JSON missing for #{lesson_id}" unless exercise_path.exist?

  exercise_data = JSON.parse(exercise_path.read)
  expected_exercise_id = "#{lesson_id}_exercise"

  raise "Exercise #{lesson_id} missing id" unless exercise_data['id'].is_a?(String) && !exercise_data['id'].empty?
  unless exercise_data['id'] == expected_exercise_id
    raise "Exercise id mismatch for #{lesson_id}: expected #{expected_exercise_id}, got #{exercise_data['id']}"
  end

  raise "Exercise #{lesson_id} must declare quiz mode" unless exercise_data['mode'] == 'quiz'
  raise "Exercise #{lesson_id} missing title" unless exercise_data['title'].is_a?(String) && !exercise_data['title'].strip.empty?
  raise "Exercise #{lesson_id} missing description" unless exercise_data['description'].is_a?(String) && !exercise_data['description'].strip.empty?

  pass_score = exercise_data['passScore']
  unless pass_score.is_a?(Numeric) && pass_score.positive?
    raise "Exercise #{lesson_id} must declare a numeric passScore"
  end

  questions = exercise_data['questions']
  unless questions.is_a?(Array) && questions.length == 10
    raise "Exercise #{lesson_id} must define exactly 10 questions"
  end

  question_ids = []

  questions.each do |question|
    raise "Question missing id in #{lesson_id}" unless question['id'].is_a?(String) && !question['id'].empty?
    question_ids << question['id']

    type = question['type']
    raise "Question #{question['id']} in #{lesson_id} missing type" unless type.is_a?(String)
    unless allowed_types.include?(type)
      raise "Question #{question['id']} in #{lesson_id} has unsupported type #{type}"
    end

    unless question['prompt'].is_a?(String) && !question['prompt'].strip.empty?
      raise "Question #{question['id']} in #{lesson_id} missing prompt"
    end

    unless question['explanation'].is_a?(String) && !question['explanation'].strip.empty?
      raise "Question #{question['id']} in #{lesson_id} missing explanation"
    end

    if %w[single multiple].include?(type)
      options = question['options']
      raise "Question #{question['id']} in #{lesson_id} missing options" unless options.is_a?(Array) && !options.empty?

      options.each do |opt|
        raise "Option missing id in #{lesson_id}##{question['id']}" unless opt['id'].is_a?(String) && !opt['id'].empty?
        raise "Option missing text in #{lesson_id}##{question['id']}" unless opt['text'].is_a?(String) && !opt['text'].strip.empty?
      end

      answers = if question.key?('answer')
                  question['answer']
                else
                  correct = options.select { |opt| opt['correct'] }
                  if type == 'multiple'
                    correct.map { |opt| opt['id'] }
                  else
                    correct.first&.fetch('id', nil)
                  end
                end

      if type == 'multiple'
        answers = Array(answers)
        if answers.empty? || answers.any? { |ans| ans.to_s.strip.empty? }
          raise "Question #{question['id']} in #{lesson_id} must declare at least one correct option"
        end
      else
        answer_value = answers.is_a?(Array) ? answers.first : answers
        unless answer_value.is_a?(String) && !answer_value.strip.empty?
          raise "Question #{question['id']} in #{lesson_id} must declare exactly one correct option"
        end
      end
    elsif type == 'truefalse'
      answer = question['answer']
      unless %w[true false].include?(answer.to_s.downcase)
        raise "True/false question #{question['id']} in #{lesson_id} must declare answer as 'true' or 'false'"
      end
    end
  end

  solution_path = dsa_root.join('solutions', "#{lesson_id}_exercise.json")
  raise "Solution JSON missing for #{lesson_id}" unless solution_path.exist?

  solution_data = JSON.parse(solution_path.read)

  solution_exercise_id = solution_data['exerciseId'] || solution_data['id']
  unless solution_exercise_id == expected_exercise_id
    raise "Solution #{lesson_id} must reference #{expected_exercise_id}, got #{solution_exercise_id.inspect}"
  end

  unless solution_data['mode'] == 'quiz'
    raise "Solution #{lesson_id} must declare quiz mode"
  end

  answer_key = solution_data['answerKey']
  answer_map =
    if answer_key.is_a?(Array)
      if answer_key.first.is_a?(Hash)
        answer_key.each_with_object({}) do |entry, memo|
          qid = entry['questionId']
          answers = entry['answers']
          raise "Solution entry missing questionId for #{lesson_id}" unless qid
          memo[qid] = answers
        end
      else
        raise "Solution #{lesson_id} answerKey count mismatch" unless answer_key.length == question_ids.length
        question_ids.each_with_index.with_object({}) do |(qid, index), memo|
          memo[qid] = answer_key[index]
        end
      end
    elsif answer_key.is_a?(Hash)
      answer_key
    elsif solution_data['questions'].is_a?(Array)
      solution_data['questions'].each_with_object({}) do |question, memo|
        qid = question['id']
        raise "Solution question missing id for #{lesson_id}" unless qid

        qtype = question['type']
        case qtype
        when 'single'
          options = question['options'] || []
          answers = if question.key?('answer')
                      Array(question['answer'])
                    else
                      options.select { |opt| opt['correct'] }.map { |opt| opt['id'] }
                    end
          raise "Solution #{lesson_id} must supply exactly one answer for #{qid}" unless answers.length == 1
          memo[qid] = answers.first
        when 'multiple'
          options = question['options'] || []
          answers = if question.key?('answer')
                      Array(question['answer'])
                    else
                      options.select { |opt| opt['correct'] }.map { |opt| opt['id'] }
                    end
          raise "Solution #{lesson_id} must supply answers for #{qid}" if answers.empty?
          memo[qid] = answers
        when 'truefalse'
          answer_value = question['answer']
          if answer_value.nil?
            correct_opt = (question['options'] || []).find { |opt| opt['correct'] }
            answer_value = correct_opt&.fetch('id', nil)
          end
          raise "Solution #{lesson_id} must provide true/false answer for #{qid}" if answer_value.to_s.strip.empty?
          memo[qid] = answer_value
        else
          raise "Solution #{lesson_id} has unsupported question type #{qtype}"
        end
      end
    else
      raise "Solution #{lesson_id} must provide answerKey or questions with answers"
    end

  unless answer_map.keys.sort == question_ids.sort
    raise "Solution #{lesson_id} answerKey must cover all questions"
  end

  answer_map.each do |qid, answers|
    if answers.is_a?(Array)
      raise "Solution #{lesson_id} answer array for #{qid} cannot be empty" if answers.empty?
    else
      raise "Solution #{lesson_id} answer for #{qid} must be non-empty" if answers.to_s.strip.empty?
    end
  end

  key_points = solution_data['keyPoints'] || []
  if !key_points.is_a?(Array) || key_points.empty?
    raise "Solution #{lesson_id} must provide keyPoints"
  end
end

puts '✅ C++ DSA lesson assets and quiz integrity validated'