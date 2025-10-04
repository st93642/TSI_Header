#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'pathname'

repo_root = Pathname.new(__dir__).parent.expand_path
curriculum_root = repo_root.join('learn', 'curriculum', 'git')
resources_root = repo_root.join('resources', 'git')

raise 'Missing Git curriculum folder' unless curriculum_root.directory?
raise 'Missing Git resources folder' unless resources_root.directory?

curriculum_path = curriculum_root.join('curriculum.json')
raise 'Missing Git curriculum.json' unless curriculum_path.exist?

curriculum = JSON.parse(curriculum_path.read)
modules = curriculum.fetch('modules')
raise 'Git curriculum must declare modules' unless modules.is_a?(Array) && !modules.empty?

modules.each do |mod|
  title = mod['title'] || mod['id']
  raise 'Module missing title' if title.to_s.strip.empty?

  lessons = mod.fetch('lessons', [])
  raise "Module #{title} must include lessons" unless lessons.is_a?(Array) && !lessons.empty?

  lessons.each do |lesson|
    lesson_id = lesson.fetch('id')
    raise "Lesson missing id in module #{title}" if lesson_id.to_s.strip.empty?

    lesson_path = curriculum_root.join('lessons', "#{lesson_id}.md")
    raise "Missing lesson markdown for #{lesson_id}" unless lesson_path.exist?

    markdown = lesson_path.read
    markdown.scan(/!\[[^\]]*\]\(([^)]+)\)/).each do |match|
      rel_path = match.first
      next if rel_path.nil? || rel_path.strip.empty?

      resource_path = lesson_path.dirname.join(rel_path).cleanpath
      unless resource_path.to_s.start_with?(resources_root.to_s)
        raise "Lesson #{lesson_id} references resource outside resources/git: #{rel_path}"
      end
      raise "Missing resource referenced by #{lesson_id}: #{resource_path}" unless resource_path.exist?
    end

    exercise_path = curriculum_root.join('exercises', "#{lesson_id}_exercise.json")
    raise "Missing exercise JSON for #{lesson_id}" unless exercise_path.exist?

    exercise = JSON.parse(exercise_path.read)
    expected_id = "#{lesson_id}_exercise"
    unless exercise['id'] == expected_id
      raise "Exercise id mismatch for #{lesson_id}: expected #{expected_id}, got #{exercise['id']}"
    end

    raise "Exercise #{lesson_id} must declare quiz mode" unless exercise['mode'] == 'quiz'
    raise "Exercise #{lesson_id} missing passScore" unless exercise['passScore'].to_f.positive?
    raise "Exercise #{lesson_id} missing totalQuestions" unless exercise['totalQuestions'].to_i.positive?

    questions = exercise['questions']
    unless questions.is_a?(Array) && questions.length == exercise['totalQuestions']
      raise "Exercise #{lesson_id} totalQuestions mismatch"
    end

    questions.each do |question|
      qid = question['id']
      raise "Question missing id in #{lesson_id}" if qid.to_s.strip.empty?
      qtype = question['type']
      unless %w[single multiple truefalse scenario].include?(qtype)
        raise "Unsupported question type #{qtype} in #{lesson_id}"
      end
      prompt = question['prompt'] || question['question']
      raise "Question #{qid} in #{lesson_id} missing prompt" if prompt.to_s.strip.empty?
      explanation = question['explanation']
      raise "Question #{qid} in #{lesson_id} missing explanation" if explanation.to_s.strip.empty?

      if %w[single multiple scenario].include?(qtype)
        options = question['options']
        raise "Question #{qid} in #{lesson_id} missing options" unless options.is_a?(Array) && !options.empty?
        options.each do |opt|
          raise "Option missing id in #{lesson_id}##{qid}" if opt['id'].to_s.strip.empty?
          raise "Option missing text in #{lesson_id}##{qid}" if opt['text'].to_s.strip.empty?
        end
      end
    end

    solution_path = curriculum_root.join('solutions', "#{lesson_id}_exercise.json")
    raise "Missing solution JSON for #{lesson_id}" unless solution_path.exist?

    solution = JSON.parse(solution_path.read)
    unless solution['exerciseId'] == expected_id && solution['mode'] == 'quiz'
      raise "Solution metadata mismatch for #{lesson_id}"
    end

    answer_key = solution['answerKey']
    unless answer_key.is_a?(Hash)
      raise "Solution #{lesson_id} must expose hash answerKey"
    end

    question_ids = questions.map { |q| q['id'] }.sort
    unless answer_key.keys.sort == question_ids
      raise "Solution #{lesson_id} answerKey must cover all questions"
    end

    answer_key.each do |qid, answers|
      if answers.is_a?(Array)
        raise "Solution #{lesson_id} answer array empty for #{qid}" if answers.empty?
      else
        raise "Solution #{lesson_id} answer blank for #{qid}" if answers.to_s.strip.empty?
      end
    end

    key_points = solution['keyPoints']
    unless key_points.is_a?(Array) && !key_points.empty?
      raise "Solution #{lesson_id} must provide keyPoints"
    end
  end
end

puts '❌ Git curriculum integrity test should fail until roadmap assets are added.'
raise 'Git curriculum incomplete - add lessons, exercises, solutions, and resources.'
