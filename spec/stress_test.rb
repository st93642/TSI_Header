#!/usr/bin/env ruby

#*****************************************************************************#
#                                                                             #
#  stress_test.rb                                         TTTTTTTT SSSSSSS II #
#                                                            TT    SS      II #
#  By: st93642@students.tsi.lv                               TT    SSSSSSS II #
#                                                            TT         SS II #
#  Created: Sep 23 2025 11:19 st93642               TT    SSSSSSS II #
#  Updated: Sep 23 2025 11:19 st93642                                #
#                                                                             #
#   Transport and Telecommunication Institute - Riga, Latvia                  #
#                       https://tsi.lv                                        #
#*****************************************************************************#

require_relative '../lib/tsi_header'

puts "🧪 COMPREHENSIVE TSI Header Stress Test"
puts "=" * 60

# Test cases with extreme variations
test_cases = [
  { 
    desc: "Short names",
    name: "a", 
    filename: "a.c" 
  },
  { 
    desc: "Medium names",
    name: "student", 
    filename: "project.c" 
  },
  { 
    desc: "Long names", 
    name: "alexandra", 
    filename: "complex_algorithm.c" 
  },
  { 
    desc: "Very long names",
    name: "maximilian", 
    filename: "very_long_descriptive_filename.c" 
  },
  { 
    desc: "Extremely long names",
    name: "constantinople", 
    filename: "extremely_long_filename_with_many_descriptive_words.c" 
  },
  { 
    desc: "Edge case - max length",
    name: "verylongusernamethatapproacheslimits", 
    filename: "this_filename_is_intentionally_very_long_to_test_edge_cases.c" 
  }
]

test_cases.each_with_index do |test_case, index|
  puts "\n#{index + 1}. #{test_case[:desc]}"
  puts "   Name: #{test_case[:name]} (#{test_case[:name].length} chars)"
  puts "   File: #{test_case[:filename]} (#{test_case[:filename].length} chars)"
  puts "   Email: #{test_case[:name]}@students.tsi.lv (#{(test_case[:name] + '@students.tsi.lv').length} chars)"
  
  # Mock configuration
  TSIHeader::Configuration.define_singleton_method(:current_user) { test_case[:name] }
  TSIHeader::Configuration.define_singleton_method(:current_user_email) { "#{test_case[:name]}@students.tsi.lv" }
  
  # Create mock document
  mock_document = Struct.new(:file_name).new("/test/#{test_case[:filename]}")
  
  # Generate header
  header_info = TSIHeader::HeaderInfo.new_header(mock_document)
  header = TSIHeader::HeaderGenerator.render_header('c', header_info)
  
  # Analyze the header
  lines = header.split("\n")
  header_lines = lines.first(12)
  
  all_correct = true
  issues = []
  
  header_lines.each_with_index do |line, line_idx|
    if line.length != 79
      all_correct = false
      issues << "Line #{line_idx + 1}: #{line.length} chars (#{line.length > 79 ? 'TOO LONG' : 'too short'})"
    end
  end
  
  if all_correct
    puts "   ✅ PASS - All lines exactly 79 characters"
  else
    puts "   ❌ FAIL - Line length issues:"
    issues.each { |issue| puts "      #{issue}" }
  end
  
  # Test specific features
  has_logo = header.include?("TTTTTTTT SSSSSSS II")
  has_tsi_info = header.include?("Transport and Telecommunication Institute")
  has_url = header.include?("https://tsi.lv")
  
  puts "   🎨 Logo present: #{has_logo ? '✅' : '❌'}"
  puts "   🏢 TSI info: #{has_tsi_info ? '✅' : '❌'}"
  puts "   🌐 URL: #{has_url ? '✅' : '❌'}"
  
  # Show the actual header for review
  puts "\n   📄 Generated Header:"
  header_lines.each_with_index do |line, i|
    status = line.length == 79 ? "✓" : "✗"
    puts "   #{status} #{line}"
  end
  puts "   " + "-" * 77
end

puts "\n🎯 SUMMARY"
puts "Testing completed! Review the output above to verify:"
puts "• All lines are exactly 79 characters"
puts "• TSI logo is properly right-aligned"  
puts "• Text content is properly left-aligned"
puts "• Long names/filenames are handled gracefully"
puts "• All TSI branding elements are present"