#!/usr/bin/env ruby

require_relative '../lib/tsi_header'

puts "🧪 UNIFIED TSI HEADER RUBY TEST SUITE"
puts "=" * 60
puts "Author: TSI Development Team"
puts "Institution: Transport and Telecommunication Institute"
puts "Version: 3.0.5"
puts

# Test all major language categories and functionality
test_cases = [
  # System Programming
  ['c', 'main.c', 'System Programming'],
  ['cpp', 'algorithm.cpp', 'System Programming'],
  ['rust', 'main.rs', 'System Programming'],

  # Web Development
  ['javascript', 'app.js', 'Web Development'],
  ['typescript', 'types.ts', 'Web Development'],
  ['html', 'index.html', 'Web Development'],

  # Mobile Development
  ['swift', 'ViewController.swift', 'Mobile Development'],
  ['kotlin', 'MainActivity.kt', 'Mobile Development'],
  ['java', 'Application.java', 'Mobile Development'],

  # Functional Programming
  ['haskell', 'Main.hs', 'Functional Programming'],
  ['erlang', 'server.erl', 'Functional Programming'],
  ['elixir', 'worker.ex', 'Functional Programming'],

  # Scientific Computing
  ['python', 'analysis.py', 'Scientific Computing'],
  ['r', 'statistics.R', 'Scientific Computing'],
  ['matlab', 'compute.m', 'Scientific Computing'],
  ['labview', 'control.vi', 'Scientific Computing'],
  ['cuda', 'kernel.cu', 'Scientific Computing'],
  ['fortran', 'simulation.f90', 'Scientific Computing'],

  # IBM iSeries
  ['rpg', 'program.rpg', 'IBM iSeries'],

  # Salesforce
  ['apex', 'controller.cls', 'Salesforce'],

  # GNOME Desktop
  ['vala', 'application.vala', 'GNOME Desktop'],
  ['genie', 'application.gs', 'GNOME Desktop'],

  # Specialized
  ['latex', 'document.tex', 'Document Preparation'],
  ['prolog', 'facts.pl', 'Logic Programming'],
  ['smalltalk', 'object.st', 'Object-Oriented'],

  # Additional comprehensive tests
  ['go', 'server.go', 'Cloud Native'],
  ['php', 'api.php', 'Web Backend'],
  ['ruby', 'application.rb', 'Full Stack'],
  ['scala', 'processor.scala', 'Big Data'],
  ['dart', 'mobile.dart', 'Cross Platform'],
  ['lua', 'script.lua', 'Game Development'],
  ['perl', 'utility.pl', 'System Administration'],
  ['shellscript', 'deploy.sh', 'DevOps']
]

# Additional test categories
unit_tests = [
  ['Language Support', lambda { TSIHeader::HeaderGenerator.supports_language?('c') }],
  ['Unknown Language Rejection', lambda { !TSIHeader::HeaderGenerator.supports_language?('unknown') }],
  ['Configuration - User', lambda { TSIHeader::Configuration.current_user.is_a?(String) && !TSIHeader::Configuration.current_user.empty? }],
  ['Configuration - Email', lambda { TSIHeader::Configuration.current_user_email.match?(/@students\.tsi\.lv|@tsi\.lv/) }],
  ['Configuration - Institution', lambda { TSIHeader::Configuration.institution_name.include?('Transport and Telecommunication Institute') }],
  ['Configuration - URL', lambda { TSIHeader::Configuration.institution_url == 'https://tsi.lv' }]
]

def check_comment_markers(header, language)
  # Define expected comment markers for different languages
  markers = {
    'c' => ['/*', '*/'],
    'cpp' => ['/*', '*/'],
    'java' => ['/*', '*/'],
    'javascript' => ['/*', '*/'],
    'typescript' => ['/*', '*/'],
    'vala' => ['/*', '*/'],
    'genie' => ['/*', '*/'],
    'python' => ['#'],
    'ruby' => ['#'],
    'shellscript' => ['#'],
    'html' => ['<!--', '-->'],
    'php' => ['/*', '*/'],
    'go' => ['/*', '*/'],
    'rust' => ['/*', '*/'],
    'swift' => ['/*', '*/'],
    'kotlin' => ['/*', '*/'],
    'scala' => ['//'],
    'dart' => ['/*', '*/'],
    'labview' => ['//'],
    'cuda' => ['/*', '*/'],
    'lua' => ['--'],
    'perl' => ['#'],
    'haskell' => ['--'],
    'erlang' => ['%%'],
    'elixir' => ['#'],
    'r' => ['#'],
    'matlab' => ['%%'],
    'fortran' => ['!'],
    'pascal' => ['{', '}'],
    'basic' => [';; '],
    'cobol' => [';; '],
    'latex' => ['%%'],
    'prolog' => ['%%'],
    'smalltalk' => ['"'],
    'rpg' => ['//'],
    'apex' => ['/*', '*/']
  }

  expected_markers = markers[language] || ['/*', '*/'] # Default to C-style

  # Check if header contains the expected markers
  expected_markers.all? { |marker| header.include?(marker) }
end

passed_tests = 0
failed_tests = 0
total_tests = test_cases.length + unit_tests.length

puts "🔬 RUNNING #{total_tests} UNIFIED COMPREHENSIVE TESTS"
puts "   • #{test_cases.length} language header tests"
puts "   • #{unit_tests.length} unit functionality tests"
puts

# Run language header tests
test_cases.each_with_index do |(lang, filename, category), index|
  print "#{(index + 1).to_s.rjust(2)}/#{total_tests} #{lang.upcase.ljust(12)} (#{category.ljust(20)}): "

  begin
    # Generate header
    mock_doc = Struct.new(:file_name).new(filename)
    header_info = TSIHeader::HeaderInfo.new_header(mock_doc)
    header = TSIHeader::HeaderGenerator.render_header(lang, header_info)

    # Validate header
    lines = header.split("\n")

    # Check line lengths (79 characters)
    line_length_ok = lines.all? { |line| line.length == 79 || line.empty? }

    # Check TSI branding (flexible since credentials are now configurable)
    header_text = header.downcase
    tsi_branding_ok = header_text.include?('transport and telecommunication institute') &&
                      header_text.include?('tsi.lv')

    # Check filename presence
    filename_ok = header.include?(filename)

    # Check comment markers for the language
    comment_markers_ok = check_comment_markers(header, lang)

    if line_length_ok && tsi_branding_ok && filename_ok && comment_markers_ok
      puts "✅ PASS"
      passed_tests += 1
    else
      puts "❌ FAIL"
      failed_tests += 1

      unless line_length_ok
        puts "    ⚠️  Line length issues detected"
      end
      unless tsi_branding_ok
        puts "    ⚠️  TSI branding missing or incorrect"
      end
      unless filename_ok
        puts "    ⚠️  Filename not found in header"
      end
      unless comment_markers_ok
        puts "    ⚠️  Comment markers incorrect for #{lang}"
      end
    end

  rescue => e
    puts "💥 ERROR: #{e.message}"
    failed_tests += 1
  end
end

puts

# Run unit tests
unit_tests.each_with_index do |(description, test_lambda), index|
  test_number = test_cases.length + index + 1
  print "#{test_number.to_s.rjust(2)}/#{total_tests} #{description.ljust(30)}: "

  begin
    if test_lambda.call
      puts "✅ PASS"
      passed_tests += 1
    else
      puts "❌ FAIL"
      failed_tests += 1
    end
  rescue => e
    puts "💥 ERROR: #{e.message}"
    failed_tests += 1
  end
end

puts
puts "=" * 60
puts "🏆 UNIFIED TEST RESULTS SUMMARY"
puts "  Total Tests:    #{total_tests}"
puts "  Passed:         #{passed_tests} ✅"
puts "  Failed:         #{failed_tests} ❌"
puts "  Success Rate:   #{((passed_tests.to_f / total_tests) * 100).round(1)}%"

if failed_tests == 0
  puts
  puts "🎉 ALL TESTS PASSED! 🎉"
  puts "🚀 TSI Header Extension is PRODUCTION READY!"
  puts
  puts "📊 Capabilities Verified:"
  puts "  ✅ #{test_cases.length} programming languages tested"
  puts "  ✅ Perfect 79-character alignment"
  puts "  ✅ Correct TSI institutional branding"
  puts "  ✅ Author information (configurable via settings/git)"
  puts "  ✅ Multi-character delimiter support"
  puts "  ✅ Dynamic content alignment"
  puts "  ✅ Unit functionality tests"
  puts
  puts "🎓 Ready for deployment in TSI Computer Science program!"
  exit 0
else
  puts
  puts "⚠️  SOME TESTS FAILED - Review and fix issues before production deployment"
  puts
  puts "📋 Failed Tests Analysis:"
  puts "  Language header tests: #{failed_tests}/#{test_cases.length} failed"
  puts "  Unit tests: #{failed_tests - (test_cases.length - (passed_tests - unit_tests.select { |_, test| test.call rescue false }.length))}/#{unit_tests.length} failed"
  exit 1
end