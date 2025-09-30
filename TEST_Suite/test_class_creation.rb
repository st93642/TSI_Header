#!/usr/bin/env ruby
# frozen_string_literal: true

# Test Class Creation Module
# Tests the TSI class creation functionality across all supported languages

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
        puts e.backtrace.join("\n") if @config.verbose
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

class TestClassCreation < TestModule
  def run
    # Supported languages for class creation
    supported_languages = ['java', 'cpp', 'csharp', 'python', 'javascript', 'kotlin', 'php', 'typescript', 'ruby', 'go', 'swift', 'dart', 'scala']

    # Calculate total tests upfront for unified progress bar
    # For each language: basic class creation test + detailed validation test
    total_tests = supported_languages.size * 2

    # Start unified progress bar
    start_progress_bar(total_tests, "TSI Class Tests")

    current_test_index = 0

    # Test class creation for all supported languages
    supported_languages.each do |language|
      current_test_index = run_basic_class_test(language, current_test_index)
      current_test_index = run_detailed_class_test(language, current_test_index)
    end

    # End progress bar and show results
    end_progress_bar

    # Cleanup
    # cleanup_temp_files  # Comment out for debugging

    # Return true if all tests passed
    @failed_tests.empty?
  end

  private

  def run_basic_class_test(language, start_index)
    index = start_index

    # Basic class creation test
    run_test_with_progress("#{language.upcase} basic class creation", @progress_bar[:total], index += 1) do
      class_name = "Test#{language.capitalize}Class"
      temp_dir = create_temp_directory("class_test_#{language}")

      # Create class using JavaScript class generator
      result = run_class_creation(language, class_name, temp_dir.to_s)
      assert(result[:success], "Class creation should succeed for #{language}")

      # Check that class files were created
      expected_files = get_expected_class_files(language, class_name, temp_dir)
      expected_files.each do |file_path|
        assert_file_exists(file_path, "Class file should exist for #{language}")
      end

      true
    end

    index
  end

  def run_detailed_class_test(language, start_index)
    index = start_index

    # Detailed class validation test
    run_test_with_progress("#{language.upcase} detailed class validation", @progress_bar[:total], index += 1) do
      class_name = "Test#{language.capitalize}ClassDetailed"
      temp_dir = create_temp_directory("class_test_detailed_#{language}")

      # Create class
      result = run_class_creation(language, class_name, temp_dir.to_s)
      assert(result[:success], "Class creation should succeed for #{language}")

      # Validate class content
      validate_class_content(language, class_name, temp_dir)

      true
    end

    index
  end

  def get_expected_class_files(language, class_name, temp_dir)
    case language
    when 'cpp'
      [
        temp_dir.join("#{class_name}.hpp"),
        temp_dir.join("#{class_name}.cpp")
      ]
    else
      extension = get_file_extension(language)
      [temp_dir.join("#{class_name}.#{extension}")]
    end
  end

  def get_file_extension(language)
    extensions = {
      'java' => 'java',
      'cpp' => 'cpp',
      'csharp' => 'cs',
      'python' => 'py',
      'javascript' => 'js',
      'kotlin' => 'kt',
      'php' => 'php',
      'typescript' => 'ts',
      'ruby' => 'rb',
      'go' => 'go',
      'swift' => 'swift',
      'dart' => 'dart',
      'scala' => 'scala'
    }
    extensions[language] || 'txt'
  end

  def validate_class_content(language, class_name, temp_dir)
    case language
    when 'cpp'
      # Validate header file
      header_file = temp_dir.join("#{class_name}.hpp")
      validate_cpp_header_file(header_file, class_name)

      # Validate implementation file
      impl_file = temp_dir.join("#{class_name}.cpp")
      validate_cpp_impl_file(impl_file, class_name)
    else
      # Validate single file for other languages
      extension = get_file_extension(language)
      class_file = temp_dir.join("#{class_name}.#{extension}")
      validate_single_class_file(language, class_file, class_name)
    end
  end

  def validate_cpp_header_file(file_path, class_name)
    assert_file_exists(file_path, "Header file should exist")

    content = File.read(file_path)

    # Check for TSI header
    assert(content.include?("Transport and Telecommunication Institute"), "Header file should contain TSI header")

    # Check for class declaration
    assert(content.include?("class #{class_name}"), "Header file should contain class declaration")

    # Check for include guards
    guard_name = "#{class_name.upcase}_HPP"
    assert(content.include?("#ifndef #{guard_name}"), "Header file should contain include guard")
    assert(content.include?("#define #{guard_name}"), "Header file should contain include guard definition")

    # Check for basic methods
    assert(content.include?("void display()"), "Header file should contain display method")
    assert(content.include?("bool equals("), "Header file should contain equals method")
  end

  def validate_cpp_impl_file(file_path, class_name)
    assert_file_exists(file_path, "Implementation file should exist")

    content = File.read(file_path)

    # Check for TSI header
    assert(content.include?("Transport and Telecommunication Institute"), "Implementation file should contain TSI header")

    # Check for header include
    assert(content.include?("#include \"#{class_name}.hpp\""), "Implementation file should include header")

    # Check for constructor implementations
    assert(content.include?("#{class_name}::#{class_name}()"), "Implementation file should contain default constructor")
    assert(content.include?("#{class_name}::#{class_name}(const std::string&"), "Implementation file should contain parameterized constructor")

    # Check for method implementations
    assert(content.include?("void #{class_name}::display()"), "Implementation file should contain display method")
    assert(content.include?("bool #{class_name}::equals("), "Implementation file should contain equals method")
  end

  def validate_single_class_file(language, file_path, class_name)
    assert_file_exists(file_path, "Class file should exist")

    content = File.read(file_path)

    # Check for TSI header
    assert(content.include?("Transport and Telecommunication Institute"), "Class file should contain TSI header")

    # Language-specific validations
    case language
    when 'java'
      validate_java_class(content, class_name)
    when 'csharp'
      validate_csharp_class(content, class_name)
    when 'python'
      validate_python_class(content, class_name)
    when 'javascript'
      validate_javascript_class(content, class_name)
    when 'kotlin'
      validate_kotlin_class(content, class_name)
    when 'php'
      validate_php_class(content, class_name)
    when 'typescript'
      validate_typescript_class(content, class_name)
    when 'ruby'
      validate_ruby_class(content, class_name)
    when 'go'
      validate_go_class(content, class_name)
    when 'swift'
      validate_swift_class(content, class_name)
    when 'dart'
      validate_dart_class(content, class_name)
    when 'scala'
      validate_scala_class(content, class_name)
    end
  end

  def validate_java_class(content, class_name)
    assert(content.include?("public class #{class_name}"), "Java class should contain class declaration")
    assert(content.include?("public #{class_name}()"), "Java class should contain default constructor")
    assert(content.include?("public #{class_name}(String name, int id)"), "Java class should contain parameterized constructor")
    assert(content.include?("public String getName()"), "Java class should contain getter")
    assert(content.include?("public void setName(String name)"), "Java class should contain setter")
    assert(content.include?("@Override"), "Java class should contain override annotations")
  end

  def validate_csharp_class(content, class_name)
    assert(content.include?("public class #{class_name}"), "C# class should contain class declaration")
    assert(content.include?("public #{class_name}()"), "C# class should contain default constructor")
    assert(content.include?("public #{class_name}(string name, int id)"), "C# class should contain parameterized constructor")
    assert(content.include?("public string Name"), "C# class should contain auto-property")
    assert(content.include?("public int Id"), "C# class should contain auto-property")
    assert(content.include?("public override string ToString()"), "C# class should contain ToString override")
  end

  def validate_python_class(content, class_name)
    assert(content.include?("class #{class_name}:"), "Python class should contain class declaration")
    assert(content.include?("def __init__(self"), "Python class should contain constructor")
    assert(content.include?("@property"), "Python class should contain property decorator")
    assert(content.include?("def __str__(self):"), "Python class should contain __str__ method")
    assert(content.include?("def __eq__(self"), "Python class should contain __eq__ method")
  end

  def validate_javascript_class(content, class_name)
    assert(content.include?("class #{class_name}"), "JavaScript class should contain class declaration")
    assert(content.include?("constructor("), "JavaScript class should contain constructor")
    assert(content.include?("get name()"), "JavaScript class should contain getter")
    assert(content.include?("set name("), "JavaScript class should contain setter")
    assert(content.include?("toString()"), "JavaScript class should contain toString method")
  end

  def validate_kotlin_class(content, class_name)
    assert(content.include?("class #{class_name}"), "Kotlin class should contain class declaration")
    assert(content.include?("constructor("), "Kotlin class should contain constructor")
    assert(content.include?("var name: String"), "Kotlin class should contain property")
    assert(content.include?("var id: Int"), "Kotlin class should contain property")
    assert(content.include?("override fun toString()"), "Kotlin class should contain toString override")
  end

  def validate_php_class(content, class_name)
    assert(content.include?("class #{class_name}"), "PHP class should contain class declaration")
    assert(content.include?("public function __construct("), "PHP class should contain constructor")
    assert(content.include?("public function getName("), "PHP class should contain getter")
    assert(content.include?("public function setName("), "PHP class should contain setter")
    assert(content.include?("public function __toString("), "PHP class should contain __toString method")
  end

  def validate_typescript_class(content, class_name)
    assert(content.include?("class #{class_name}"), "TypeScript class should contain class declaration")
    assert(content.include?("constructor("), "TypeScript class should contain constructor")
    assert(content.include?("get name("), "TypeScript class should contain getter")
    assert(content.include?("set name("), "TypeScript class should contain setter")
    assert(content.include?("toString()"), "TypeScript class should contain toString method")
  end

  def validate_ruby_class(content, class_name)
    assert(content.include?("class #{class_name}"), "Ruby class should contain class declaration")
    assert(content.include?("def initialize("), "Ruby class should contain initialize method")
    assert(content.include?("attr_accessor"), "Ruby class should contain attribute accessors")
    assert(content.include?("def to_s"), "Ruby class should contain to_s method")
    assert(content.include?("def ==("), "Ruby class should contain equality method")
  end

  def validate_go_class(content, class_name)
    assert(content.include?("type #{class_name} struct"), "Go should contain struct declaration")
    assert(content.include?("func New#{class_name}("), "Go should contain constructor function")
    assert(content.include?("func (c *#{class_name}) GetName("), "Go should contain getter method")
    assert(content.include?("func (c *#{class_name}) SetName("), "Go should contain setter method")
    assert(content.include?("func (c *#{class_name}) String("), "Go should contain String method")
  end

  def validate_swift_class(content, class_name)
    assert(content.include?("class #{class_name}"), "Swift class should contain class declaration")
    assert(content.include?("init("), "Swift class should contain initializer")
    assert(content.include?("var name: String"), "Swift class should contain property")
    assert(content.include?("var id: Int"), "Swift class should contain property")
    assert(content.include?("var description: String"), "Swift class should contain computed property")
  end

  def validate_dart_class(content, class_name)
    assert(content.include?("class #{class_name}"), "Dart class should contain class declaration")
    assert(content.include?("String name;"), "Dart class should contain field")
    assert(content.include?("int id;"), "Dart class should contain field")
    assert(content.include?("String toString()"), "Dart class should contain toString method")
    assert(content.include?("bool operator =="), "Dart class should contain equality operator")
  end

  def validate_scala_class(content, class_name)
    assert(content.include?("class #{class_name}"), "Scala class should contain class declaration")
    assert(content.include?("var name: String"), "Scala class should contain parameter")
    assert(content.include?("var id: Int"), "Scala class should contain parameter")
    assert(content.include?("override def toString"), "Scala class should contain toString override")
    assert(content.include?("override def equals"), "Scala class should contain equals override")
  end

  def run_class_creation(language, class_name, temp_dir)
    # Run Node.js to execute the JavaScript class generator
    # Create a temporary Node.js script to call the class generator
    node_script = <<~NODE_SCRIPT
      const { generateClass } = require('#{@config.extension_root}/core/generators/classGenerators');
      const fs = require('fs');
      const path = require('path');
      const { execSync } = require('child_process');

      // Create a temporary file path for the class
      const classFileName = '#{class_name}.' + getExtension('#{language}');
      const classFilePath = path.join('#{temp_dir}', classFileName);

      // Generate class
      const result = generateClass('#{language}', '#{class_name}', classFilePath, '#{@config.extension_root}', '#{@config.extension_root}/core/lib/tsi_header_cli.rb', process.env);
      console.log('Generate result:', JSON.stringify(result));

      if (result.success) {
        // For C++, files are already created by the generator with headers
        if (result.files) {
          // C++ case - files should already exist with headers
          console.log(JSON.stringify({ success: true, files_created: true }));
        } else if (result.content) {
          // Other languages - write content to file first
          fs.writeFileSync(classFilePath, result.content);
          
          // Then add TSI header using Ruby CLI
          try {
            const headerCommand = 'ruby "' + '#{@config.extension_root}' + '/core/lib/tsi_header_cli.rb" insert "' + '#{language}' + '" "' + classFilePath + '"';
            console.log('Header command:', headerCommand);
            const headerResult = execSync(headerCommand, { encoding: 'utf8', cwd: '#{@config.extension_root}', env: process.env });
            console.log('Header result:', headerResult);
            const headerResponse = JSON.parse(headerResult);
            
            if (headerResponse.success) {
              // Read the current file content
              const currentContent = fs.readFileSync(classFilePath, 'utf8');
              
              // Prepend the header to the content
              const newContent = headerResponse.header + currentContent;
              
              // Write back to file
              fs.writeFileSync(classFilePath, newContent);
              
              console.log('Header added successfully');
              console.log(JSON.stringify({ success: true, file: classFilePath }));
            } else {
              console.log('Header generation failed:', headerResponse.message);
              console.log(JSON.stringify({ success: false, error: 'Failed to generate header: ' + headerResponse.message }));
            }
          } catch (error) {
            console.log(JSON.stringify({ success: false, error: 'Header command failed: ' + error.message }));
          }
        } else {
          console.log(JSON.stringify({ success: false, error: 'No content or files returned' }));
        }
      } else {
        console.log(JSON.stringify({ success: false, error: result.message || result.error || 'Unknown error' }));
      }

      function getExtension(lang) {
        const extensions = {
          'java': 'java',
          'cpp': 'cpp',
          'csharp': 'cs',
          'python': 'py',
          'javascript': 'js',
          'kotlin': 'kt',
          'php': 'php',
          'typescript': 'ts',
          'ruby': 'rb',
          'go': 'go',
          'swift': 'swift',
          'dart': 'dart',
          'scala': 'scala'
        };
        return extensions[lang] || 'txt';
      }
    NODE_SCRIPT

    temp_script_path = create_temp_file("class_generator.js", node_script)

    cmd = ['node', temp_script_path.to_s]

    puts "Running Node.js class creation: #{cmd.join(' ')}" if @config.verbose

    stdout, stderr, status = Open3.capture3(*cmd, chdir: @config.extension_root.to_s)

    # Clean up temp script
    FileUtils.rm_f(temp_script_path) if temp_script_path.exist?

    puts "Node.js stdout: #{stdout}" if @config.verbose
    puts "Node.js stderr: #{stderr}" if @config.verbose && !stderr.empty?
    puts "Node.js exit code: #{status.exitstatus}" if @config.verbose

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