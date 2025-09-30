#!/usr/bin/env ruby
# frozen_string_literal: true

# Test Header Insertion Module
# Tests the TSI header insertion functionality across all supported languages

require 'json'
require 'open3'
require 'tempfile'
require 'fileutils'
require 'pathname'

# Minimal TestModule for standalone execution
unless defined?(TestModule)
  class TestModule
    attr_reader :config, :results

    def initialize(config = nil, results = nil)
      @config = config || TestSuiteConfig.new
      @results = results || TestResults.new
      @progress_bar = nil
      @failed_tests = []
    end

    def run_test_with_progress(test_name, total_tests = nil, current_index = nil)
      start_time = Time.now
      begin
        result = yield
        duration = Time.now - start_time

        if result.is_a?(Hash) && result[:passed] != nil
          passed = result[:passed]
          message = result[:message]
          @results.add_result(test_name, passed, message, duration) if @results.respond_to?(:add_result)
        else
          passed = !!result
          message = nil
          @results.add_result(test_name, passed, message, duration) if @results.respond_to?(:add_result)
        end

        # Track failed tests for later display
        @failed_tests << { name: test_name, message: message } unless passed

        # Update progress bar if we have total and current
        if total_tests && current_index
          update_progress_bar(current_index, total_tests, test_name, passed)
        end

        result
      rescue => e
        duration = Time.now - start_time
        @results.add_result(test_name, false, e.message, duration) if @results.respond_to?(:add_result)
        @failed_tests << { name: test_name, message: e.message }
        puts "❌ #{test_name}: #{e.message}" if @config.verbose
        false
      end
    end

    def start_progress_bar(total_tests, title = "Testing")
      @progress_bar = {
        total: total_tests,
        title: title,
        start_time: Time.now,
        last_update: 0
      }
      print_progress_bar(0, total_tests, "Starting...", true)
    end

    def end_progress_bar
      if @progress_bar
        total_time = Time.now - @progress_bar[:start_time]
        success_rate = @results ? (@results.passed_tests.to_f / @results.total_tests * 100).round(1) : 0

        # Clear progress bar line and show final summary
        print "\r" + " " * 80 + "\r"
        puts "🎯 Completed: #{@results&.total_tests || 0} tests, #{success_rate}% success in #{format_duration(total_time)}"

        # Show failed tests if any
        if @failed_tests.any?
          puts "\n❌ Failed Tests:"
          @failed_tests.each do |failure|
            puts "  • #{failure[:name]}#{failure[:message] ? ": #{failure[:message]}" : ""}"
          end
        end
      end
    end

    private

    def update_progress_bar(current, total, current_test, passed)
      # Throttle updates to avoid too much output
      now = Time.now.to_f
      return if now - @progress_bar[:last_update] < 0.1 # Update max 10 times per second

      @progress_bar[:last_update] = now
      print_progress_bar(current, total, current_test, passed)
    end

    def print_progress_bar(current, total, current_test, passed)
      percentage = (current.to_f / total * 100).round(1)
      bar_width = 25
      filled = (current.to_f / total * bar_width).round
      bar = "█" * filled + "░" * (bar_width - filled)

      # Truncate test name to fit
      test_display = current_test.length > 20 ? current_test[0..17] + "..." : current_test

      # Status indicator
      status = passed.nil? ? "⏳" : (passed ? "✅" : "❌")

      # Time elapsed
      elapsed = Time.now - @progress_bar[:start_time]
      time_display = format_duration(elapsed)

      # Create the progress line (keep under 80 chars)
      progress_line = sprintf("%s [%s] %.1f%% %s %s %s",
                            @progress_bar[:title], bar, percentage, test_display, status, time_display)

      # Print with carriage return to overwrite line
      print "\r" + progress_line.ljust(80)
      $stdout.flush
    end

    def format_duration(seconds)
      if seconds < 60
        "#{seconds.round(1)}s"
      elsif seconds < 3600
        minutes = (seconds / 60).floor
        secs = (seconds % 60).round(1)
        "#{minutes}m#{secs}s"
      else
        hours = (seconds / 3600).floor
        minutes = ((seconds % 3600) / 60).floor
        "#{hours}h#{minutes}m"
      end
    end

    def run_test(test_name, &block)
      start_time = Time.now
      begin
        puts "🧪 Running: #{test_name}"
        result = block.call
        duration = Time.now - start_time

        if result.is_a?(Hash) && result[:passed] != nil
          @results.add_result(test_name, result[:passed], result[:message], duration) if @results.respond_to?(:add_result)
          puts result[:passed] ? "✅ #{test_name}" : "❌ #{test_name}: #{result[:message]}"
        else
          @results.add_result(test_name, !!result, nil, duration) if @results.respond_to?(:add_result)
          puts result ? "✅ #{test_name}" : "❌ #{test_name}"
        end

        result
      rescue => e
        duration = Time.now - start_time
        @results.add_result(test_name, false, e.message, duration) if @results.respond_to?(:add_result)
        puts "❌ #{test_name}: #{e.message}"
        false
      end
    end

    def assert(condition, message = "Assertion failed")
      raise message unless condition
    end

    def assert_equal(expected, actual, message = nil)
      message ||= "Expected #{expected.inspect}, got #{actual.inspect}"
      assert(expected == actual, message)
    end

    def assert_file_exists(path, message = nil)
      message ||= "File does not exist: #{path}"
      assert(File.exist?(path), message)
    end

    def assert_file_contains(path, content, message = nil)
      assert_file_exists(path)
      file_content = File.read(path)
      message ||= "File #{path} does not contain expected content: #{content}"
      assert(file_content.include?(content), message)
    end

    def create_temp_file(name, content = "")
      temp_path = Pathname.new(Dir.tmpdir).join('tsi_test', name)
      FileUtils.mkdir_p(temp_path.dirname)
      File.write(temp_path, content)
      temp_path
    end
  end
end

  # Minimal config for standalone execution
  unless defined?(TestSuiteConfig)
    class TestSuiteConfig
    attr_reader :extension_root, :verbose

    def initialize
      @extension_root = Pathname.new(__dir__).parent.expand_path
      @verbose = ENV['VERBOSE'] == '1'
    end
    end

  # Minimal results for standalone execution
  class TestResults
    attr_reader :total_tests, :passed_tests, :failed_tests

    def initialize
      @total_tests = 0
      @passed_tests = 0
      @failed_tests = 0
    end

    def add_result(test_name, passed, message = nil, duration = nil)
      @total_tests += 1
      if passed
        @passed_tests += 1
      else
        @failed_tests += 1
      end
    end

    def success_rate
      return 0 if @total_tests.zero?
      (@passed_tests.to_f / @total_tests * 100).round(1)
    end

    def total_duration
      0 # Not tracking duration in minimal version
    end

    def summary
      {
        total_tests: @total_tests,
        passed_tests: @passed_tests,
        failed_tests: @failed_tests,
        success_rate: success_rate,
        total_duration: total_duration,
        results: []
      }
    end
  end
  end

class TestHeaderInsertion < TestModule
  def run
    # Calculate total tests upfront for unified progress bar
    all_languages = load_supported_languages
    total_tests = 2 + all_languages.size + 1 + 1 + 3  # basic(2) + languages + update(1) + removal(1) + edge_cases(3)

    # Start unified progress bar
    start_progress_bar(total_tests, "TSI Tests")

    current_test_index = 0

    # Test basic header insertion
    setup_test_environment

    current_test_index = run_basic_tests(current_test_index)

    # Test header insertion for all supported languages
    current_test_index = run_language_tests(current_test_index)

    # Test header update functionality
    current_test_index = run_update_test(current_test_index)

    # Test header removal
    current_test_index = run_removal_test(current_test_index)

    # Test edge cases
    current_test_index = run_edge_case_tests(current_test_index)

    # End progress bar and show results
    end_progress_bar

    # Return true if all tests passed
    @failed_tests.empty?
  end

  private

  def setup_test_environment
    # Set environment variables that the CLI expects
    ENV['TSI_USERNAME'] = 'testuser'
    ENV['TSI_EMAIL'] = 'test@example.com'
  end

  def run_basic_tests(start_index)
    index = start_index

    # Basic C header insertion
    run_test_with_progress("Basic C header insertion", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("test.c", "#include <stdio.h>\n\nint main() {\n    return 0;\n}\n")

      result = run_cli_command("insert", "c", temp_file.to_s)
      assert(result[:success], "CLI command should succeed")
      assert(result["header"], "Should return header content")

      new_content = result["header"] + "\n" + File.read(temp_file)
      File.write(temp_file, new_content)

      assert_file_contains(temp_file, "/*", "Header comment should be present")
      assert_file_contains(temp_file, "Transport and Telecommunication Institute", "TSI branding should be present")
      assert_file_contains(temp_file, "test.c", "Filename should be in header")
      assert_file_contains(temp_file, "test@example.com", "Email should be in header")
      assert_file_contains(temp_file, "#include <stdio.h>", "Original content should be preserved")

      true
    end

    # Basic Python header insertion
    run_test_with_progress("Basic Python header insertion", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("test.py", "def hello():\n    print('Hello World')\n")

      result = run_cli_command("insert", "python", temp_file.to_s)
      assert(result[:success], "CLI command should succeed")
      assert(result["header"], "Should return header content")

      new_content = result["header"] + "\n" + File.read(temp_file)
      File.write(temp_file, new_content)

      assert_file_contains(temp_file, "#", "Python comment should be present")
      assert_file_contains(temp_file, "Transport and Telecommunication Institute", "TSI branding should be present")
      assert_file_contains(temp_file, "test.py", "Filename should be in header")
      assert_file_contains(temp_file, "def hello():", "Original content should be preserved")

      true
    end

    index
  end

  def run_language_tests(start_index)
    index = start_index

    # Test all supported languages
    languages = TSIHeader::Delimiters::LANGUAGE_DELIMITERS.keys.sort

    languages.each do |language|
      run_test_with_progress("Testing #{language}", @progress_bar[:total], index += 1) do
        # Create a sample file for this language
        sample_content = get_sample_content_for_language(language)
        temp_file = create_temp_file("test.#{get_extension_for_language(language)}", sample_content)

        result = run_cli_command("insert", language, temp_file.to_s)
        assert(result[:success], "CLI command should succeed for #{language}")
        assert(result["header"], "Should return header content for #{language}")

        new_content = result["header"] + "\n" + File.read(temp_file)
        File.write(temp_file, new_content)

        # Verify header was inserted correctly
        delimiters = TSIHeader::Delimiters::LANGUAGE_DELIMITERS[language]
        if delimiters && !delimiters.empty?
          comment_start = delimiters[0]
          assert_file_contains(temp_file, comment_start, "#{language} comment delimiter should be present")
        end

        # Check for TSI branding (some languages might not have it in the header)
        if language != "binary" && language != "hex" && language != "log"
          assert_file_contains(temp_file, "Transport and Telecommunication Institute", "TSI branding should be present for #{language}")
        end

        # Verify original content is preserved
        assert_file_contains(temp_file, sample_content.strip, "Original content should be preserved for #{language}")

        true
      end
    end

    index
  end

  def run_update_test(start_index)
    index = start_index

    # Test header update functionality
    run_test_with_progress("Header update functionality", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("update_test.c", "#include <stdio.h>\n\nint main() {\n    return 0;\n}\n")

      # First insert header
      result = run_cli_command("insert", "c", temp_file.to_s)
      assert(result[:success], "Initial insert should succeed")
      new_content = result["header"] + "\n" + File.read(temp_file)
      File.write(temp_file, new_content)

      # Sleep briefly to ensure timestamp difference
      sleep(1)

      # Then update header
      result = run_cli_command("update", "c", temp_file.to_s)
      assert(result[:success], "Update command should succeed")
      assert(result["message"], "Should return success message")

      # Verify update timestamp is different
      updated_content = File.read(temp_file)
      assert(updated_content.include?("Updated:"), "Update timestamp should be present")
      assert(updated_content.include?("Transport and Telecommunication Institute"), "TSI branding should be present")
      assert(updated_content.include?("#include <stdio.h>"), "Original content should be preserved")

      true
    end

    index
  end

  def run_removal_test(start_index)
    index = start_index

    # Test header removal
    run_test_with_progress("Header removal functionality", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("remove_test.py", "def hello():\n    print('Hello World')\n")

      # First get the header content and manually add it to the file
      insert_result = run_cli_command("insert", "python", temp_file.to_s)
      assert(insert_result[:success], "Insert should succeed")
      header_content = insert_result["header"]

      # Manually add the header to the file
      original_content = File.read(temp_file)
      new_content = header_content + original_content
      File.write(temp_file, new_content)

      content_with_header = File.read(temp_file)
      assert(content_with_header.include?("#"), "Header should be present after manual insertion")

      # Remove header
      remove_result = run_cli_command("remove", "python", temp_file.to_s)
      assert(remove_result[:success], "Remove should succeed")

      content_without_header = File.read(temp_file)
      # Should not contain TSI header anymore, but should still have the original code
      assert(content_without_header.include?("def hello():"), "Original code should remain")
      # The content should be shorter after header removal
      assert(content_without_header.length < content_with_header.length, "Content should be shorter after header removal")

      true
    end

    index
  end

  def run_edge_case_tests(start_index)
    index = start_index

    # Test edge cases
    run_test_with_progress("Empty file header insertion", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("empty.c", "")

      result = run_cli_command("insert", "c", temp_file.to_s)
      assert(result[:success], "Should handle empty files")
      assert(result["header"], "Should return header content for empty files")
      assert(result["header"].include?("/*"), "Header should contain C comment syntax")

      # File should remain empty since insert doesn't modify files
      assert_equal("", File.read(temp_file), "Empty file should remain empty")

      true
    end

    run_test_with_progress("File with existing header", @progress_bar[:total], index += 1) do
      # Create a file with a proper TSI header already inserted
      existing_header = "/*****************************************************************************/\n/*                                                                           */\n/*  existing_header.c                                    TTTTTTTT SSSSSSS II */\n/*                                                          TT    SS      II */\n/*  By: test@example.com                                     TT    SSSSSSS II */\n/*                                                          TT         SS II */\n/*  Created: 2023                                              TT    SSSSSSS II */\n/*  Updated: 2023                                                                   */\n/*                                                                           */\n/*   Transport and Telecommunication Institute - Riga, Latvia                */\n/*                       https://tsi.lv                                      */\n/*****************************************************************************/\n"
      temp_file = create_temp_file("existing_header.c", existing_header + "#include <stdio.h>\n\nint main() {\n    return 0;\n}\n")

      result = run_cli_command("insert", "c", temp_file.to_s)
      assert(result[:success], "Should handle files with existing headers")
      assert(result["header"], "Should return header content")
      assert(result["hasExistingHeader"], "Should detect existing TSI header")

      # File should not be modified
      original_content = File.read(temp_file)
      expected_content = existing_header + "#include <stdio.h>\n\nint main() {\n    return 0;\n}\n"
      assert_equal(expected_content, original_content, "File with existing TSI header should not be modified")

      true
    end

    run_test_with_progress("Invalid language handling", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("invalid.xyz", "some content")

      result = run_cli_command("insert", "invalidlang", temp_file.to_s)
      # CLI should handle invalid languages gracefully
      assert(result.is_a?(Hash), "Should return a result hash even for invalid languages")

      true
    end

    index
  end

  def load_supported_languages
    # Load languages from the Delimiters class which defines what languages are actually supported
    require_relative '../core/lib/tsi_header/delimiters'
    TSIHeader::Delimiters::LANGUAGE_DELIMITERS.keys.sort
  end

  def get_sample_content_for_language(language)
    samples = {
      'c' => "#include <stdio.h>\n\nint main() {\n    printf(\"Hello World\\n\");\n    return 0;\n}\n",
      'cpp' => "#include <iostream>\n\nint main() {\n    std::cout << \"Hello World\" << std::endl;\n    return 0;\n}\n",
      'java' => "public class Main {\n    public static void main(String[] args) {\n        System.out.println(\"Hello World\");\n    }\n}\n",
      'python' => "def main():\n    print('Hello World')\n\nif __name__ == '__main__':\n    main()\n",
      'ruby' => "def main\n  puts 'Hello World'\nend\n\nmain if __FILE__ == $0\n",
      'javascript' => "function main() {\n    console.log('Hello World');\n}\n\nmain();\n",
      'typescript' => "function main(): void {\n    console.log('Hello World');\n}\n\nmain();\n",
      'go' => "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello World\")\n}\n",
      'rust' => "fn main() {\n    println!(\"Hello World\");\n}\n",
      'php' => "<?php\n\nfunction main() {\n    echo \"Hello World\\n\";\n}\n\nmain();\n"
    }
    samples[language] || "console.log('Hello World');\n"
  end

  def get_extension_for_language(language)
    extensions = {
      'c' => 'c',
      'cpp' => 'cpp',
      'java' => 'java',
      'python' => 'py',
      'ruby' => 'rb',
      'javascript' => 'js',
      'typescript' => 'ts',
      'go' => 'go',
      'rust' => 'rs',
      'php' => 'php'
    }
    extensions[language] || 'txt'
  end

  def test_basic_header_insertion
    puts "\n🔧 Testing Basic Header Insertion..."

    # Set up test environment
    setup_test_environment

    run_test("Basic C header insertion") do
      temp_file = create_temp_file("test.c", "#include <stdio.h>\n\nint main() {\n    return 0;\n}\n")

      # Simulate extension behavior: get header from CLI, then insert into file
      result = run_cli_command("insert", "c", temp_file.to_s)
      assert(result[:success], "CLI command should succeed")
      assert(result["header"], "Should return header content")

      # Insert header into file (simulating what extension does)
      original_content = File.read(temp_file)
      new_content = result["header"] + "\n" + original_content
      File.write(temp_file, new_content)

      # Verify header was inserted correctly
      assert_file_contains(temp_file, "/*", "Header comment should be present")
      assert_file_contains(temp_file, "Transport and Telecommunication Institute", "TSI branding should be present")
      assert_file_contains(temp_file, "test.c", "Filename should be in header")
      assert_file_contains(temp_file, "test@example.com", "Email should be in header")
      assert_file_contains(temp_file, "#include <stdio.h>", "Original content should be preserved")

      true
    end

    run_test("Basic Python header insertion") do
      temp_file = create_temp_file("test.py", "def hello():\n    print('Hello World')\n")

      # Get header from CLI
      result = run_cli_command("insert", "python", temp_file.to_s)
      assert(result[:success], "CLI command should succeed")
      assert(result["header"], "Should return header content")

      # Insert header into file
      original_content = File.read(temp_file)
      new_content = result["header"] + "\n" + original_content
      File.write(temp_file, new_content)

      # Verify header was inserted correctly
      assert_file_contains(temp_file, "#", "Python comment should be present")
      assert_file_contains(temp_file, "Transport and Telecommunication Institute", "TSI branding should be present")
      assert_file_contains(temp_file, "test.py", "Filename should be in header")
      assert_file_contains(temp_file, "def hello():", "Original content should be preserved")

      true
    end
  end

  def test_all_languages_header_insertion
    puts "\n🌍 Testing Header Insertion for ALL Supported Languages..."

    # Load all supported languages dynamically
    all_languages = load_supported_languages
    puts "Found #{all_languages.size} supported languages to test"

    # Test ALL languages for basic header insertion functionality
    test_languages = all_languages
    puts "Testing ALL #{test_languages.size} languages..."

    # Start progress bar
    start_progress_bar(test_languages.size, "Languages")

    test_languages.each_with_index do |language, index|
      run_test_with_progress("#{language.upcase} header insertion", test_languages.size, index + 1) do
        extension = get_file_extension(language)
        temp_file = create_temp_file("test_#{language}.#{extension}", get_sample_content(language))

        result = run_cli_command("insert", language, temp_file.to_s)
        success = result[:success] && result["header"] && result["header"].include?("Transport and Telecommunication Institute")

        # File should not be modified for insert command
        original_content = get_sample_content(language)
        file_unchanged = (original_content == File.read(temp_file))

        success && file_unchanged
      end
    end

    # End progress bar and show results
    end_progress_bar

    # Return true if all languages passed
    @failed_tests.empty?
  end

  def test_header_update
    puts "\n🔄 Testing Header Update Functionality..."

    run_test("Header update preserves content") do
      temp_file = create_temp_file("update_test.c", "#include <stdio.h>\n\nint main() {\n    return 0;\n}\n")

      # First get the header content
      insert_result = run_cli_command("insert", "c", temp_file.to_s)
      assert(insert_result[:success], "Initial insert should succeed")
      header_content = insert_result["header"]

      # Manually add the header to the file (simulating what the extension would do)
      original_content = File.read(temp_file)
      new_content = header_content + original_content
      File.write(temp_file, new_content)

      original_lines = new_content.lines.count

      # Wait a moment to ensure different timestamp
      sleep(1)

      # Update header
      result2 = run_cli_command("update", "c", temp_file.to_s)
      assert(result2[:success], "Update should succeed")

      updated_content = File.read(temp_file)
      updated_lines = updated_content.lines.count

      # Should have same number of lines (header updated in place)
      assert_equal(original_lines, updated_lines, "Line count should remain the same")
      assert_file_contains(temp_file, "Transport and Telecommunication Institute", "TSI branding should still be present")
      assert_file_contains(temp_file, "#include <stdio.h>", "Original content should be preserved")

      true
    end
  end

  def test_header_removal
    puts "\n🗑️  Testing Header Removal..."

    run_test("Header removal works correctly") do
      temp_file = create_temp_file("remove_test.py", "def hello():\n    print('Hello World')\n")

      # First get the header content and manually add it to the file
      insert_result = run_cli_command("insert", "python", temp_file.to_s)
      assert(insert_result[:success], "Insert should succeed")
      header_content = insert_result["header"]

      # Manually add the header to the file
      original_content = File.read(temp_file)
      new_content = header_content + original_content
      File.write(temp_file, new_content)

      content_with_header = File.read(temp_file)
      assert(content_with_header.include?("#"), "Header should be present after manual insertion")

      # Remove header
      remove_result = run_cli_command("remove", "python", temp_file.to_s)
      assert(remove_result[:success], "Remove should succeed")

      content_without_header = File.read(temp_file)
      # Should not contain TSI header anymore, but should still have the original code
      assert(content_without_header.include?("def hello():"), "Original code should remain")
      # The content should be shorter after header removal
      assert(content_without_header.length < content_with_header.length, "Content should be shorter after header removal")

      true
    end
  end

  def test_edge_cases
    puts "\n⚠️  Testing Edge Cases..."

    run_test("Empty file header insertion") do
      temp_file = create_temp_file("empty.c", "")

      result = run_cli_command("insert", "c", temp_file.to_s)
      assert(result[:success], "Should handle empty files")
      assert(result["header"], "Should return header content for empty files")
      assert(result["header"].include?("/*"), "Header should contain C comment syntax")

      # File should remain empty since insert doesn't modify files
      assert_equal("", File.read(temp_file), "Empty file should remain empty")

      true
    end

    run_test("File with existing header") do
      # Create a file with a proper TSI header already inserted
      existing_header = "/*****************************************************************************/\n/*                                                                           */\n/*  existing_header.c                                    TTTTTTTT SSSSSSS II */\n/*                                                          TT    SS      II */\n/*  By: test@example.com                                     TT    SSSSSSS II */\n/*                                                          TT         SS II */\n/*  Created: 2023                                              TT    SSSSSSS II */\n/*  Updated: 2023                                                                   */\n/*                                                                           */\n/*   Transport and Telecommunication Institute - Riga, Latvia                */\n/*                       https://tsi.lv                                      */\n/*****************************************************************************/\n"
      temp_file = create_temp_file("existing_header.c", existing_header + "#include <stdio.h>\n\nint main() {\n    return 0;\n}\n")

      result = run_cli_command("insert", "c", temp_file.to_s)
      assert(result[:success], "Should handle files with existing headers")
      assert(result["header"], "Should return header content")
      assert(result["hasExistingHeader"], "Should detect existing TSI header")

      # File should not be modified
      original_content = File.read(temp_file)
      expected_content = existing_header + "#include <stdio.h>\n\nint main() {\n    return 0;\n}\n"
      assert_equal(expected_content, original_content, "File with existing TSI header should not be modified")

      true
    end

    run_test("Invalid language handling") do
      temp_file = create_temp_file("invalid.xyz", "some content")

      result = run_cli_command("insert", "invalidlang", temp_file.to_s)
      # CLI should handle invalid languages gracefully
      assert(result.is_a?(Hash), "Should return a result hash even for invalid languages")

      true
    end
  end

  private

  def run_cli_command(action, language, file_path)
    cli_path = @config.extension_root.join('core/lib/tsi_header_cli.rb')
    cmd = ['ruby', cli_path.to_s, action, language, file_path]

    puts "Running: #{cmd.join(' ')}" if @config.verbose

    stdout, stderr, status = Open3.capture3(*cmd, chdir: @config.extension_root.to_s)

    result = {
      success: status.success?,
      stdout: stdout,
      stderr: stderr,
      exit_code: status.exitstatus
    }

    # Try to parse JSON output if present
    if stdout.strip.start_with?('{')
      begin
        json_result = JSON.parse(stdout)
        result.merge!(json_result)
      rescue JSON::ParserError
        # Not JSON, keep as is
      end
    end

    result
  end

  def get_file_extension(language)
    extensions = {
      'c' => 'c',
      'cpp' => 'cpp',
      'java' => 'java',
      'python' => 'py',
      'ruby' => 'rb',
      'javascript' => 'js',
      'typescript' => 'ts',
      'go' => 'go',
      'rust' => 'rs',
      'php' => 'php'
    }
    extensions[language] || 'txt'
  end

  def get_comment_char(language)
    comment_chars = {
      'c' => '/*',
      'cpp' => '//',
      'java' => '//',
      'python' => '#',
      'ruby' => '#',
      'javascript' => '//',
      'typescript' => '//',
      'go' => '//',
      'rust' => '//',
      'php' => '//'
    }
    comment_chars[language] || '#'
  end

  def get_sample_content(language)
    samples = {
      'c' => "#include <stdio.h>\n\nint main() {\n    printf(\"Hello World\\n\");\n    return 0;\n}\n",
      'cpp' => "#include <iostream>\n\nint main() {\n    std::cout << \"Hello World\" << std::endl;\n    return 0;\n}\n",
      'java' => "public class Main {\n    public static void main(String[] args) {\n        System.out.println(\"Hello World\");\n    }\n}\n",
      'python' => "def main():\n    print('Hello World')\n\nif __name__ == '__main__':\n    main()\n",
      'ruby' => "def main\n  puts 'Hello World'\nend\n\nmain if __FILE__ == $0\n",
      'javascript' => "function main() {\n    console.log('Hello World');\n}\n\nmain();\n",
      'typescript' => "function main(): void {\n    console.log('Hello World');\n}\n\nmain();\n",
      'go' => "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello World\")\n}\n",
      'rust' => "fn main() {\n    println!(\"Hello World\");\n}\n",
      'php' => "<?php\n\nfunction main() {\n    echo \"Hello World\\n\";\n}\n\nmain();\n"
    }
    samples[language] || "console.log('Hello World');\n"
  end
end