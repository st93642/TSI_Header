#!/usr/bin/env ruby

#*****************************************************************************#
#                                                                             #
#  simple_test.rb                                         TTTTTTTT SSSSSSS II #
#                                                            TT    SS      II #
#  By: st93642@students.tsi.lv                               TT    SSSSSSS II #
#                                                            TT         SS II #
#  Created: Sep 23 2025 11:19 igors.oleinikovs               TT    SSSSSSS II #
#  Updated: Sep 23 2025 11:19 igors.oleinikovs                                #
#                                                                             #
#   Transport and Telecommunication Institute - Riga, Latvia                  #
#                       https://tsi.lv                                        #
#*****************************************************************************#

require_relative '../lib/tsi_header'

def run_tests
  puts "Running TSI Header test suite..."
  puts "=" * 40
  
  test_count = 0
  passed_count = 0
  
  # Test 1: Language support
  test_count += 1
  if TSIHeader::HeaderGenerator.supports_language?('c')
    puts "✓ Test 1: C language support"
    passed_count += 1
  else
    puts "✗ Test 1: C language support"
  end
  
  # Test 2: Unknown language
  test_count += 1
  if !TSIHeader::HeaderGenerator.supports_language?('unknown')
    puts "✓ Test 2: Unknown language rejection"
    passed_count += 1
  else
    puts "✗ Test 2: Unknown language rejection"
  end
  
  # Test 3: Header generation
  test_count += 1
  begin
    mock_doc = Struct.new(:file_name).new('/test/example.c')
    header_info = TSIHeader::HeaderInfo.new_header(mock_doc)
    header = TSIHeader::HeaderGenerator.render_header('c', header_info)
    
    if header.include?('example.c') && header.include?('TTTTTTTT SSSSSSS II')
      puts "✓ Test 3: Header generation"
      passed_count += 1
    else
      puts "✗ Test 3: Header generation"
    end
  rescue => e
    puts "✗ Test 3: Header generation - #{e.message}"
  end
  
  # Test 4: Header extraction
  test_count += 1
  begin
    sample_file = '/home/altin/vscode-42header/tsi_example.c'
    if File.exist?(sample_file)
      content = File.read(sample_file)
      extracted = TSIHeader::HeaderExtractor.extract_header(content)
      
      if extracted && extracted.include?('tsi_example.c')
        puts "✓ Test 4: Header extraction"
        passed_count += 1
      else
        puts "✗ Test 4: Header extraction"
      end
    else
      puts "✗ Test 4: Header extraction - sample file not found"
    end
  rescue => e
    puts "✗ Test 4: Header extraction - #{e.message}"
  end
  
  # Test 5: Header parsing
  test_count += 1
  begin
    sample_file = '/home/altin/vscode-42header/tsi_example.c'
    if File.exist?(sample_file)
      content = File.read(sample_file)
      extracted = TSIHeader::HeaderExtractor.extract_header(content)
      
      if extracted
        parsed = TSIHeader::HeaderParser.parse_header(extracted)
        if parsed.filename == 'tsi_example.c'
          puts "✓ Test 5: Header parsing"
          passed_count += 1
        else
          puts "✗ Test 5: Header parsing - filename mismatch"
        end
      else
        puts "✗ Test 5: Header parsing - no header extracted"
      end
    else
      puts "✗ Test 5: Header parsing - sample file not found"
    end
  rescue => e
    puts "✗ Test 5: Header parsing - #{e.message}"
  end
  
  puts "=" * 40
  puts "Test Results: #{passed_count}/#{test_count} passed"
  
  if passed_count == test_count
    puts "🎉 All tests passed!"
    exit 0
  else
    puts "❌ Some tests failed"
    exit 1
  end
end

run_tests if __FILE__ == $0