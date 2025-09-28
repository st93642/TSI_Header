/**
 * Ruby Project Creator
 * Creates Ruby-specific project files and structure
 */

const { generateTSIHeaderContent } = require('../headerUtils');

/**
 * Create Ruby-specific files
 */
async function createRubyFiles(vscode, projectName, projectUri) {
    await createGemfile(vscode, projectName, projectUri);
    await createRakefile(vscode, projectName, projectUri);
    await createBaseClassRb(vscode, projectName, projectUri);
}

/**
 * Create Gemfile for Ruby projects
 */
async function createGemfile(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'Gemfile');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('Gemfile', vscode);

    const content = `${headerContent}

source 'https://rubygems.org'

# Specify your gem's dependencies in ${projectName}.gemspec
gemspec

# Development dependencies
group :development do
  gem 'bundler', '~> 2.0'
  gem 'rake', '>= 12.3.3'
  gem 'rspec', '~> 3.0'
  gem 'rubocop', '>= 1.0', '< 2.0'
  gem 'yard', '~> 0.9'
end

# Test dependencies
group :test do
  gem 'simplecov', '~> 0.21'
  gem 'webmock', '~> 3.14'
end
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create Rakefile for Ruby projects
 */
async function createRakefile(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'Rakefile');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('Rakefile', vscode);

    const content = `${headerContent}

require 'bundler/gem_tasks'
require 'rspec/core/rake_task'
require 'rubocop/rake_task'

# Default task
task default: [:spec, :rubocop]

# RSpec task
RSpec::Core::RakeTask.new(:spec) do |task|
  task.pattern = 'spec/**/*_spec.rb'
  task.rspec_opts = '--format documentation --color'
end

# RuboCop task
RuboCop::RakeTask.new

# Test task
task test: :spec

# Documentation task
require 'yard'
YARD::Rake::YardocTask.new do |t|
  t.files = ['lib/**/*.rb']
  t.options = ['--readme', 'README.md']
end

# Clean task
task :clean do
  FileUtils.rm_rf('pkg')
  FileUtils.rm_rf('coverage')
  FileUtils.rm_rf('.yardoc')
end

desc 'Run all checks'
task ci: [:spec, :rubocop, :yard]

desc 'Release gem'
task release: [:ci, :build] do
  # Add release logic here
end
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create BaseClass.rb file for Ruby projects
 */
async function createBaseClassRb(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'lib', 'base_class.rb');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('base_class.rb', vscode);

    const content = `${headerContent}

# BaseClass - Foundation class for ${projectName}
#
# This class provides basic functionality and serves as a base
# for other classes in the ${projectName} project.
#
# Features:
# - Basic object initialization
# - String-based naming system
# - Unique ID generation
# - Display functionality
# - JSON serialization support
# - Method chaining support
# - Validation and error handling

require 'json'
require 'time'

class BaseClass
  # Class variable for ID generation
  @@next_id = 1

  # Accessors
  attr_reader :name, :id, :created_at
  attr_accessor :description

  # Constructor
  def initialize(name = 'DefaultObject', id = nil)
    @name = name.to_s.empty? ? 'UnnamedObject' : name.to_s
    @id = id || self.class.generate_next_id
    @created_at = Time.now
    @description = ''

    # Initialize the object
    initialize_object
  end

  # Class method to generate next ID
  def self.generate_next_id
    id = @@next_id
    @@next_id += 1
    id
  end

  # Class method to get next available ID
  def self.next_id
    @@next_id
  end

  # Instance methods

  # Set the object name with validation
  def name=(new_name)
    @name = new_name.to_s.empty? ? 'UnnamedObject' : new_name.to_s
  end

  # Display object information
  def display
    puts "\#{self.class.name} Object:"
    puts "  Name: \#{@name}"
    puts "  ID: \#{@id}"
    puts "  Description: \#{@description.empty? ? '(none)' : @description}"
    puts "  Created: \#{@created_at.strftime('%Y-%m-%d %H:%M:%S')}"
  end

  # Convert to hash for serialization
  def to_hash
    {
      'name' => @name,
      'id' => @id,
      'class' => self.class.name,
      'description' => @description,
      'created_at' => @created_at.iso8601
    }
  end

  # Convert to JSON string
  def to_json(*args)
    to_hash.to_json(*args)
  end

  # Create object from hash
  def self.from_hash(hash)
    obj = new(hash['name'], hash['id'])
    obj.description = hash['description'] || ''
    obj.instance_variable_set(:@created_at, Time.parse(hash['created_at'])) if hash['created_at']
    obj
  end

  # Create object from JSON string
  def self.from_json(json_string)
    hash = JSON.parse(json_string)
    from_hash(hash)
  end

  # Check if object is valid
  def valid?
    !@name.empty? && @id.is_a?(Integer) && @id > 0
  end

  # Get object information as string
  def info
    "\#{self.class.name}(name=\"\#{@name}\", id=\#{@id})"
  end

  # String representation
  def to_s
    info
  end

  # Inspect representation
  def inspect
    "\#<\#{self.class.name} \#{info}>"
  end

  # Equality comparison
  def ==(other)
    return false unless other.is_a?(BaseClass)
    @name == other.name && @id == other.id
  end

  # Hash for use in Hash and Set
  def hash
    [@name, @id].hash
  end

  # Eql? for Hash key comparison
  def eql?(other)
    self == other
  end

  # Method chaining support
  def with_name(name)
    self.name = name
    self
  end

  def with_description(description)
    self.description = description
    self
  end

  # Clone with modifications
  def clone_with(**attributes)
    cloned = self.clone
    attributes.each do |key, value|
      cloned.send("\#{key}=", value) if cloned.respond_to?("\#{key}=")
    end
    cloned
  end

  protected

  # Initialize hook for subclasses
  def initialize_object
    # Override in subclasses for custom initialization
  end

  private

  # Private helper methods
  def validate_name(name)
    name.to_s.strip.empty? ? 'UnnamedObject' : name.to_s.strip
  end
end

# Example usage (only when run directly)
if __FILE__ == $PROGRAM_NAME
  # Create some example objects
  obj1 = BaseClass.new('Example Object 1')
  obj2 = BaseClass.new('Example Object 2')

  puts 'Created objects:'
  obj1.display
  puts
  obj2.display
  puts

  puts 'JSON representation:'
  puts obj1.to_json
  puts

  puts 'String representations:'
  puts "to_s(): \#{obj1.to_s}"
  puts "inspect(): \#{obj1.inspect}"
  puts "equal?: \#{obj1 == obj2}"

  # Demonstrate method chaining
  puts
  puts 'Method chaining:'
  obj3 = BaseClass.new.with_name('Chained Object').with_description('Created with chaining')
  obj3.display
end`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

module.exports = {
    createRubyFiles
};