#!/usr/bin/env ruby
# frozen_string_literal: true

# Test Project Creation Module
# Tests the TSI project creation functionality across all supported languages

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

    def assert_directory_exists(path, message = nil)
      message ||= "Directory does not exist: #{path}"
      assert(Dir.exist?(path), message)
    end

    def create_temp_file(name, content = "")
      temp_path = Pathname.new(Dir.tmpdir).join('tsi_test', name)
      FileUtils.mkdir_p(temp_path.dirname)
      File.write(temp_path, content)
      temp_path
    end

    def create_temp_directory(name)
      temp_path = Pathname.new(Dir.tmpdir).join('tsi_test', name)
      FileUtils.mkdir_p(temp_path)
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

class TestProjectCreation < TestModule
  def run
    # Supported languages for project creation
    supported_languages = ['c', 'cpp', 'python', 'java', 'rust', 'ruby', 'php', 'html']

    # Calculate total tests upfront for unified progress bar
    # For each language: basic structure test + detailed validation test
    total_tests = supported_languages.size * 2

    # Start unified progress bar
    start_progress_bar(total_tests, "TSI Project Tests")

    current_test_index = 0

    # Test project creation for all supported languages
    supported_languages.each do |language|
      current_test_index = run_basic_project_test(language, current_test_index)
      current_test_index = run_detailed_project_test(language, current_test_index)
    end

    # End progress bar and show results
    end_progress_bar

    # Return true if all tests passed
    @failed_tests.empty?
  end

  private

  def run_basic_project_test(language, start_index)
    index = start_index

    # Basic project structure test
    run_test_with_progress("#{language.upcase} basic project structure", @progress_bar[:total], index += 1) do
      project_name = "test_#{language}_project"
      temp_project_dir = create_temp_directory(project_name)

      # Create project structure using JavaScript project creator
    result = run_project_creation(language, project_name, temp_project_dir.to_s)
    if @config.verbose
      puts "Project creation result: #{result.inspect}"
      puts "STDOUT: #{result[:stdout]}"
      puts "STDERR: #{result[:stderr]}"
    end
    assert(result[:success], "Project creation should succeed for #{language}. Error: #{result[:error] || 'Unknown error'}")      # Check that project directory was created
      project_root = temp_project_dir.join(project_name)
      assert_directory_exists(project_root, "Project root directory should exist")

      # Check basic directory structure
      expected_dirs = get_expected_directories(language)
      expected_dirs.each do |dir|
        dir_path = project_root.join(dir)
        assert_directory_exists(dir_path, "Directory #{dir} should exist for #{language}")
      end

      # Check that main source file exists
      main_file = get_main_source_file(language, project_root)
      assert_file_exists(main_file, "Main source file should exist for #{language}")

      true
    end

    index
  end

  def run_detailed_project_test(language, start_index)
    index = start_index

    # Detailed project validation test
    run_test_with_progress("#{language.upcase} detailed project validation", @progress_bar[:total], index += 1) do
      project_name = "test_#{language}_project_detailed"
      temp_project_dir = create_temp_directory(project_name)

      # Create project structure
      result = run_project_creation(language, project_name, temp_project_dir.to_s)
      assert(result[:success], "Project creation should succeed for #{language}")

      project_root = temp_project_dir.join(project_name)

      # Validate main source file has TSI header and codebase
      main_file = get_main_source_file(language, project_root)
      validate_main_source_file(language, main_file)

      # Validate header file for C/C++
      if language == 'c' || language == 'cpp'
        header_file = get_header_file(language, project_name, project_root)
        validate_header_file(language, header_file, project_name)
      end

      # Validate build files
      validate_build_files(language, project_name, project_root)

      # Validate documentation files
      validate_documentation_files(language, project_name, project_root)

      # Validate git ignore file
      validate_gitignore_file(language, project_root)

      true
    end

    index
  end

  def get_expected_directories(language)
    common_dirs = ['src', 'docs']

    case language
    when 'c', 'cpp'
      common_dirs + ['include', 'build']
    when 'python'
      common_dirs + ['tests', 'scripts']
    when 'java'
      common_dirs + ['src/main/java', 'src/test/java', 'target']
    when 'rust'
      common_dirs + ['src', 'tests', 'examples', 'benches']
    when 'ruby'
      common_dirs + ['lib', 'spec', 'bin', 'config']
    when 'php'
      common_dirs + ['src', 'public', 'tests']
    when 'html'
      common_dirs + ['src', 'assets', 'css', 'js']
    else
      common_dirs
    end
  end

  def get_main_source_file(language, project_dir)
    case language
    when 'java'
      project_dir.join('src', 'main', 'java', 'Main.java')
    when 'php'
      project_dir.join('public', 'index.php')
    else
      extension = get_file_extension(language)
      project_dir.join('src', "main.#{extension}")
    end
  end

  def get_header_file(language, project_name, project_dir)
    extension = language == 'c' ? 'h' : 'hpp'
    project_dir.join('include', "#{project_name}.#{extension}")
  end

  def get_file_extension(language)
    extensions = {
      'c' => 'c',
      'cpp' => 'cpp',
      'python' => 'py',
      'java' => 'java',
      'rust' => 'rs',
      'ruby' => 'rb',
      'php' => 'php',
      'html' => 'html'
    }
    extensions[language] || 'txt'
  end

  def validate_main_source_file(language, file_path)
    assert_file_exists(file_path, "Main source file should exist")

    content = File.read(file_path)

    # Check for TSI header
    assert(content.include?("Transport and Telecommunication Institute"), "Main file should contain TSI header")

    # Check for appropriate comment syntax
    comment_start = get_comment_start(language)
    assert(content.include?(comment_start), "Main file should contain proper comment syntax for #{language}")

    # Check for basic code structure
    case language
    when 'c'
      assert(content.include?("int main(int argc, char"), "C main file should contain main function")
      assert(content.include?("#include <stdio.h>"), "C main file should contain standard includes")
    when 'cpp'
      assert(content.include?("int main(int argc, char"), "C++ main file should contain main function")
      assert(content.include?("#include <iostream>"), "C++ main file should contain standard includes")
    when 'python'
      assert(content.include?("def main():"), "Python main file should contain main function")
    when 'java'
      assert(content.include?("public class Main"), "Java main file should contain Main class")
      assert(content.include?("public static void main"), "Java main file should contain main method")
    when 'rust'
      assert(content.include?("fn main()"), "Rust main file should contain main function")
    when 'ruby'
      assert(content.include?("def run"), "Ruby main file should contain run method")
    when 'php'
      assert(content.include?("<?php"), "PHP main file should contain PHP opening tag")
      assert(content.include?("function main()"), "PHP main file should contain main function")
    when 'html'
      assert(content.include?("<!DOCTYPE html>"), "HTML main file should contain DOCTYPE")
      assert(content.include?("<html"), "HTML main file should contain html tag")
    end
  end

  def validate_header_file(language, file_path, project_name)
    assert_file_exists(file_path, "Header file should exist")

    content = File.read(file_path)

    # Check for TSI header
    assert(content.include?("Transport and Telecommunication Institute"), "Header file should contain TSI header")

    # Check for include guards
    guard_name = "#{project_name.upcase.gsub(/[^A-Z0-9_]/, '_')}_#{language == 'c' ? 'H' : 'HPP'}"
    assert(content.include?("#ifndef #{guard_name}"), "Header file should contain include guard")
    assert(content.include?("#define #{guard_name}"), "Header file should contain include guard definition")
    assert(content.include?("#endif // #{guard_name}"), "Header file should contain include guard endif")
  end

  def validate_build_files(language, project_name, project_dir)
    case language
    when 'c', 'cpp'
      makefile_path = project_dir.join('Makefile')
      assert_file_exists(makefile_path, "Makefile should exist for #{language}")

      makefile_content = File.read(makefile_path)
      assert(makefile_content.include?("Transport and Telecommunication Institute"), "Makefile should contain TSI header")
      assert(makefile_content.include?(project_name), "Makefile should contain project name")

    when 'python'
      makefile_path = project_dir.join('Makefile')
      assert_file_exists(makefile_path, "Makefile should exist for Python")

      makefile_content = File.read(makefile_path)
      assert(makefile_content.include?("Transport and Telecommunication Institute"), "Python Makefile should contain TSI header")

    when 'java'
      pom_path = project_dir.join('pom.xml')
      gradle_path = project_dir.join('build.gradle')

      assert_file_exists(pom_path, "Maven pom.xml should exist for Java")
      assert_file_exists(gradle_path, "Gradle build.gradle should exist for Java")

      pom_content = File.read(pom_path)
      gradle_content = File.read(gradle_path)

      assert(pom_content.include?("Transport and Telecommunication Institute"), "Maven pom.xml should contain TSI header")
      assert(gradle_content.include?("Transport and Telecommunication Institute"), "Gradle build.gradle should contain TSI header")
    end
  end

  def validate_documentation_files(language, project_name, project_dir)
    readme_path = project_dir.join('README.md')
    assert_file_exists(readme_path, "README.md should exist")

    readme_content = File.read(readme_path)
    assert(readme_content.include?(project_name), "README should contain project name")
    assert(readme_content.upcase.include?(language.upcase), "README should mention the language")
  end

  def validate_gitignore_file(language, project_dir)
    gitignore_path = project_dir.join('.gitignore')
    assert_file_exists(gitignore_path, ".gitignore should exist")

    gitignore_content = File.read(gitignore_path)

    # Check for language-specific ignore patterns
    case language
    when 'c', 'cpp'
      assert(gitignore_content.include?("*.o") || gitignore_content.include?("build/"), ".gitignore should contain C/C++ patterns")
    when 'python'
      assert(gitignore_content.include?("__pycache__") || gitignore_content.include?("*.pyc"), ".gitignore should contain Python patterns")
    when 'java'
      assert(gitignore_content.include?("*.class") || gitignore_content.include?("target/"), ".gitignore should contain Java patterns")
    when 'rust'
      assert(gitignore_content.include?("target/") || gitignore_content.include?("Cargo.lock"), ".gitignore should contain Rust patterns")
    when 'ruby'
      assert(gitignore_content.include?(".bundle") || gitignore_content.include?("Gemfile.lock"), ".gitignore should contain Ruby patterns")
    end
  end

  def get_comment_start(language)
    # Get the comment start delimiter for the language
    require_relative '../core/lib/tsi_header/delimiters'
    delimiters = TSIHeader::Delimiters::LANGUAGE_DELIMITERS[language]
    delimiters ? delimiters[0] : '#'
  end

  def run_project_creation(language, project_name, project_dir)
    # Run Node.js to execute the JavaScript project creation
    # Create a temporary Node.js script to call the project creator
    node_script = <<~NODE_SCRIPT
      const { createProjectStructure } = require('#{@config.extension_root}/core/generators/project/projectCreator');

      // Mock vscode API for testing
      const mockVscode = {
        Uri: {
          joinPath: (base, ...parts) => {
            const path = require('path');
            return path.join(base, ...parts);
          }
        },
        workspace: {
          fs: {
            createDirectory: async (uri) => {
              const fs = require('fs').promises;
              await fs.mkdir(uri, { recursive: true });
            },
            writeFile: async (uri, content) => {
              const fs = require('fs').promises;
              await fs.writeFile(uri, content);
            }
          },
          getConfiguration: (section) => ({
            get: (key) => {
              if (key === 'username') return 'testuser';
              if (key === 'email') return 'test@example.com';
              return undefined;
            }
          })
        },
        extensions: {
          getExtension: (id) => ({
            extensionPath: process.cwd()
          })
        },
        window: {
          showInformationMessage: () => {},
          showErrorMessage: () => {}
        }
      };

      // Create project structure directly (bypass the UI parts)
      createProjectStructure('#{language}', '#{project_name}', '#{project_dir}', mockVscode)
        .then(() => {
          console.log(JSON.stringify({ success: true }));
        })
        .catch((error) => {
          console.log(JSON.stringify({ success: false, error: error.message }));
        });
    NODE_SCRIPT

    temp_script_path = create_temp_file("project_creator.js", node_script)

    cmd = ['node', temp_script_path.to_s]

    puts "Running Node.js project creation: #{cmd.join(' ')}" if @config.verbose

    stdout, stderr, status = Open3.capture3(*cmd, chdir: @config.extension_root.to_s)

    # Clean up temp script
    FileUtils.rm_f(temp_script_path) if temp_script_path.exist?

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