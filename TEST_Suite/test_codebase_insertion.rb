#!/usr/bin/env ruby
# frozen_string_literal: true

# Test Codebase Insertion Module
# Tests the TSI codebase insertion functionality across all supported languages

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

    def cleanup_temp_files
      temp_dir = Pathname.new(Dir.tmpdir).join('tsi_test')
      FileUtils.rm_rf(temp_dir) if temp_dir.exist?
    end
  end

  # Minimal config for standalone execution
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

class TestCodebaseInsertion < TestModule
  def run
    # Calculate total tests upfront for unified progress bar
    all_languages = load_supported_languages
    total_tests = 2 + all_languages.size + 1 + 1 + 3  # basic(2) + languages + update(1) + removal(1) + edge_cases(3)

    # Start unified progress bar
    start_progress_bar(total_tests, "TSI Codebase Tests")

    current_test_index = 0

    # Test basic codebase insertion
    setup_test_environment

    current_test_index = run_basic_tests(current_test_index)

    # Test codebase insertion for all supported languages
    current_test_index = run_language_tests(current_test_index)

    # Test codebase update functionality
    current_test_index = run_update_test(current_test_index)

    # Test codebase removal
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

    # Basic C codebase insertion
    run_test_with_progress("Basic C codebase insertion", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("test.c", "")

      # Get header from CLI
      header_result = run_cli_command("insert", "c", temp_file.to_s)
      assert(header_result[:success], "Header CLI command should succeed")
      assert(header_result["header"], "Should return header content")

      # Get codebase from JavaScript generator
      codebase_result = run_codebase_generator("c", temp_file.to_s)
      assert(codebase_result[:success], "Codebase generator should succeed")
      assert(codebase_result["content"], "Should return codebase content")

      # Combine header and codebase
      full_content = header_result["header"] + codebase_result["content"]
      File.write(temp_file, full_content)

      # Verify header was inserted correctly
      assert_file_contains(temp_file, "/*", "Header comment should be present")
      assert_file_contains(temp_file, "Transport and Telecommunication Institute", "TSI branding should be present")
      assert_file_contains(temp_file, "test.c", "Filename should be in header")
      assert_file_contains(temp_file, "test@example.com", "Email should be in header")

      # Verify codebase content is present
      assert_file_contains(temp_file, "#include <stdio.h>", "C codebase should contain standard includes")
      assert_file_contains(temp_file, "int main(int argc, char *argv[])", "C codebase should contain main function")

      true
    end

    # Basic Python codebase insertion
    run_test_with_progress("Basic Python codebase insertion", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("test.py", "")

      # Get header from CLI
      header_result = run_cli_command("insert", "python", temp_file.to_s)
      assert(header_result[:success], "Header CLI command should succeed")
      assert(header_result["header"], "Should return header content")

      # Get codebase from JavaScript generator
      codebase_result = run_codebase_generator("python", temp_file.to_s)
      assert(codebase_result[:success], "Codebase generator should succeed")
      assert(codebase_result["content"], "Should return codebase content")

      # Combine header and codebase
      full_content = header_result["header"] + codebase_result["content"]
      File.write(temp_file, full_content)

      # Verify header was inserted correctly
      assert_file_contains(temp_file, "#", "Python comment should be present")
      assert_file_contains(temp_file, "Transport and Telecommunication Institute", "TSI branding should be present")
      assert_file_contains(temp_file, "test.py", "Filename should be in header")

      # Verify codebase content is present
      assert_file_contains(temp_file, "def main():", "Python codebase should contain main function")

      true
    end

    index
  end

  def run_language_tests(start_index)
    index = start_index

    # Test all supported languages
    languages = TSIHeader::Delimiters::LANGUAGE_DELIMITERS.keys.sort

    languages.each do |language|
      run_test_with_progress("Testing #{language} codebase", @progress_bar[:total], index += 1) do
        # Create a sample file for this language
        extension = get_extension_for_language(language)
        temp_file = create_temp_file("test_codebase.#{extension}", "")

        # Get header from CLI
        header_result = run_cli_command("insert", language, temp_file.to_s)
        assert(header_result[:success], "Header CLI command should succeed for #{language}")
        assert(header_result["header"], "Should return header content for #{language}")

        # Get codebase from JavaScript generator
        codebase_result = run_codebase_generator(language, temp_file.to_s)
        assert(codebase_result[:success], "Codebase generator should succeed for #{language}")
        assert(codebase_result["content"], "Should return codebase content for #{language}")

        # Combine header and codebase
        full_content = header_result["header"] + codebase_result["content"]
        File.write(temp_file, full_content)

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

        # Verify codebase content is present (basic check that content was added)
        file_content = File.read(temp_file)
        header_length = header_result["header"].length
        codebase_part = file_content[header_length..-1]
        assert(codebase_part && !codebase_part.strip.empty?, "#{language} should have codebase content after header")

        true
      end
    end

    index
  end

  def run_update_test(start_index)
    index = start_index

    # Test codebase update functionality
    run_test_with_progress("Codebase update functionality", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("update_codebase_test.c", "")

      # First insert header and codebase
      header_result = run_cli_command("insert", "c", temp_file.to_s)
      assert(header_result[:success], "Initial header insert should succeed")

      codebase_result = run_codebase_generator("c", temp_file.to_s)
      assert(codebase_result[:success], "Initial codebase generation should succeed")

      full_content = header_result["header"] + codebase_result["content"]
      File.write(temp_file, full_content)

      # Sleep briefly to ensure timestamp difference
      sleep(1)

      # Then update header (codebase stays the same)
      update_result = run_cli_command("update", "c", temp_file.to_s)
      assert(update_result[:success], "Update command should succeed")
      assert(update_result["message"], "Should return success message")

      # Verify update timestamp is different but codebase content remains
      updated_content = File.read(temp_file)
      assert(updated_content.include?("Updated:"), "Update timestamp should be present")
      assert(updated_content.include?("Transport and Telecommunication Institute"), "TSI branding should be present")

      # Check that the codebase part (after header) is still present
      # This is a basic check - in a real update, only the header timestamps change
      assert(updated_content.include?("#include <stdio.h>"), "Original codebase content should be preserved")
      assert(updated_content.include?("int main(int argc, char *argv[])"), "Main function should still be present")

      true
    end

    index
  end

  def run_removal_test(start_index)
    index = start_index

    # Test codebase removal
    run_test_with_progress("Codebase removal functionality", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("remove_codebase_test.py", "")

      # First get header and codebase content
      header_result = run_cli_command("insert", "python", temp_file.to_s)
      assert(header_result[:success], "Header insert should succeed")

      codebase_result = run_codebase_generator("python", temp_file.to_s)
      assert(codebase_result[:success], "Codebase generation should succeed")

      # Manually combine and write to file
      full_content = header_result["header"] + codebase_result["content"]
      File.write(temp_file, full_content)

      content_with_header_and_codebase = File.read(temp_file)
      assert(content_with_header_and_codebase.include?("#"), "Header should be present")
      assert(content_with_header_and_codebase.include?("def main():"), "Codebase should be present")

      # Remove header (this should remove both header and leave the codebase)
      remove_result = run_cli_command("remove", "python", temp_file.to_s)
      assert(remove_result[:success], "Remove should succeed")

      content_after_removal = File.read(temp_file)
      # Should not contain TSI header anymore, but should still have the codebase
      assert(content_after_removal.include?("def main():"), "Codebase should remain after header removal")
      # The content should be shorter after header removal
      assert(content_after_removal.length < content_with_header_and_codebase.length, "Content should be shorter after header removal")

      true
    end

    index
  end

  def run_edge_case_tests(start_index)
    index = start_index

    # Test edge cases
    run_test_with_progress("Empty file codebase insertion", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("empty_codebase.c", "")

      header_result = run_cli_command("insert", "c", temp_file.to_s)
      assert(header_result[:success], "Should handle empty files for header")
      assert(header_result["header"], "Should return header content for empty files")

      codebase_result = run_codebase_generator("c", temp_file.to_s)
      assert(codebase_result[:success], "Should handle empty files for codebase")
      assert(codebase_result["content"], "Should return codebase content for empty files")

      # File should remain empty since neither command modifies files directly
      assert_equal("", File.read(temp_file), "Empty file should remain empty")

      true
    end

    run_test_with_progress("File with existing header codebase insertion", @progress_bar[:total], index += 1) do
      # Create a file with a proper TSI header already inserted
      existing_header = "/*****************************************************************************/\n/*                                                                           */\n/*  existing_codebase.c                   TTTTTTTT SSSSSSS II */\n/*                                                          TT    SS      II */\n/*  By: test@example.com                                     TT    SSSSSSS II */\n/*                                                          TT         SS II */\n/*  Created: 2023                                              TT    SSSSSSS II */\n/*  Updated: 2023                                                                   */\n/*                                                                           */\n/*   Transport and Telecommunication Institute - Riga, Latvia                */\n/*                       https://tsi.lv                                      */\n/*****************************************************************************/\n"
      temp_file = create_temp_file("existing_codebase.c", existing_header)

      header_result = run_cli_command("insert", "c", temp_file.to_s)
      assert(header_result[:success], "Should handle files with existing headers")
      assert(header_result["header"], "Should return header content")
      assert(header_result["hasExistingHeader"], "Should detect existing TSI header")

      codebase_result = run_codebase_generator("c", temp_file.to_s)
      assert(codebase_result[:success], "Should generate codebase even with existing header")

      # File should not be modified by either command
      original_content = File.read(temp_file)
      expected_content = existing_header
      assert_equal(expected_content, original_content, "File with existing TSI header should not be modified")

      true
    end

    run_test_with_progress("Invalid language codebase handling", @progress_bar[:total], index += 1) do
      temp_file = create_temp_file("invalid_codebase.xyz", "")

      header_result = run_cli_command("insert", "invalidlang", temp_file.to_s)
      # CLI should handle invalid languages gracefully
      assert(header_result.is_a?(Hash), "Should return a result hash even for invalid languages")

      codebase_result = run_codebase_generator("invalidlang", temp_file.to_s)
      # JavaScript generator should handle invalid languages gracefully
      assert(codebase_result.is_a?(Hash), "Should return a result hash even for invalid languages in codebase generator")

      true
    end

    index
  end

  def load_supported_languages
    # Load languages from the Delimiters class which defines what languages are actually supported
    require_relative '../core/lib/tsi_header/delimiters'
    TSIHeader::Delimiters::LANGUAGE_DELIMITERS.keys.sort
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

  def run_cli_command(action, language, file_path)
    cli_path = @config.extension_root.join('core/lib/tsi_header_cli.rb')
    cmd = ['ruby', cli_path.to_s, action, language, file_path]

    puts "Running CLI: #{cmd.join(' ')}" if @config.verbose

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

  def run_codebase_generator(language, file_path)
    # Run Node.js directly with inline script to execute the JavaScript codebase generator
    # Escape single quotes in the language and file_path for shell safety
    escaped_language = language.gsub("'", "\\'")
    escaped_file_path = file_path.to_s.gsub("'", "\\'")

    node_command = [
      'node',
      '-e',
      "const { generateCodeBase } = require('./core/generators/codeBaseGenerators'); const result = generateCodeBase('#{escaped_language}', '#{escaped_file_path}'); console.log(JSON.stringify(result));"
    ]

    puts "Running Node.js generator: #{node_command.join(' ')}" if @config.verbose

    stdout, stderr, status = Open3.capture3(*node_command, chdir: @config.extension_root.to_s)

    result = {
      success: status.success?,
      stdout: stdout,
      stderr: stderr,
      exit_code: status.exitstatus
    }

    # Try to parse JSON output
    if stdout.strip.start_with?('{')
      begin
        json_result = JSON.parse(stdout.strip)
        result.merge!(json_result)
      rescue JSON::ParserError => e
        result[:parse_error] = e.message
        # Not JSON, keep as is
      end
    end

    result
  end
end