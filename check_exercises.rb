#!/usr/bin/env ruby
require 'json'

exercise_dir = '/home/altin/Desktop/TSI_Header/learn_exercises/ruby'
json_dir = '/home/altin/Desktop/TSI_Header/learn/curriculum/ruby/exercises'

puts 'Checking all exercises and their JSON definitions...'

Dir.glob("#{exercise_dir}/*.rb").each do |exercise_file|
  next if File.basename(exercise_file) == '# Solution for Hello World Exercise.rb'

  basename = File.basename(exercise_file, '.rb')
  json_file = "#{json_dir}/#{basename}.json"

  content = File.read(exercise_file).strip
  has_content = content != '# Your code here' && content.length > 20

  if File.exist?(json_file)
    begin
      json_data = JSON.parse(File.read(json_file))
      starter_code = json_data['starterCode']
      has_starter = starter_code && !starter_code.strip.empty? && starter_code.strip != '# Your code here'

      status = has_content ? 'HAS_CONTENT' : 'EMPTY'
      json_status = has_starter ? 'HAS_STARTER' : 'NO_STARTER'

      puts "#{basename}: #{status} | JSON: #{json_status}"

      if !has_content && has_starter
        puts "  -> RESTORING from JSON"
        File.write(exercise_file, starter_code)
      end
    rescue JSON::ParserError => e
      puts "#{basename}: JSON_ERROR - #{e.message}"
    end
  else
    puts "#{basename}: NO_JSON_FILE"
  end
end

puts 'Exercise check complete!'