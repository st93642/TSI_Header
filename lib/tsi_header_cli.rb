#!/usr/bin/env ruby

require_relative 'tsi_header'
require 'json'

def main
  command = ARGV[0]
  language_id = ARGV[1]
  file_path = ARGV[2]

  case command
  when 'insert'
    handle_insert(language_id, file_path)
  when 'update'
    handle_update(language_id, file_path)
  else
    puts JSON.generate({ success: false, message: "Unknown command: #{command}" })
    exit 1
  end
end

def handle_insert(language_id, file_path)
  if TSIHeader::HeaderGenerator.supports_language?(language_id)
    # Read current file content
    current_text = File.read(file_path) rescue ""
    current_header = TSIHeader::HeaderExtractor.extract_header(current_text)
    
    mock_document = MockDocument.new(file_path)
    
    if current_header
      # Update existing header
      header_info = TSIHeader::HeaderParser.parse_header(current_header)
      new_header_info = TSIHeader::HeaderInfo.update_header(mock_document, header_info)
      new_header = TSIHeader::HeaderGenerator.render_header(language_id, new_header_info)
      
      puts JSON.generate({
        success: true,
        hasExistingHeader: true,
        header: new_header
      })
    else
      # Insert new header
      header_info = TSIHeader::HeaderInfo.new_header(mock_document)
      new_header = TSIHeader::HeaderGenerator.render_header(language_id, header_info)
      
      puts JSON.generate({
        success: true,
        hasExistingHeader: false,
        header: new_header
      })
    end
  else
    puts JSON.generate({
      success: false,
      message: "No header support for language #{language_id}"
    })
  end
end

def handle_update(language_id, file_path)
  if TSIHeader::HeaderGenerator.supports_language?(language_id)
    current_text = File.read(file_path) rescue ""
    current_header = TSIHeader::HeaderExtractor.extract_header(current_text)
    
    if current_header
      mock_document = MockDocument.new(file_path)
      header_info = TSIHeader::HeaderParser.parse_header(current_header)
      new_header_info = TSIHeader::HeaderInfo.update_header(mock_document, header_info)
      new_header = TSIHeader::HeaderGenerator.render_header(language_id, new_header_info)
      
      # Update the file with new header
      updated_content = current_text.sub(current_header, new_header.chomp)
      File.write(file_path, updated_content)
      
      puts JSON.generate({
        success: true,
        message: "Header updated successfully"
      })
    else
      puts JSON.generate({
        success: false,
        message: "No header found to update"
      })
    end
  else
    puts JSON.generate({
      success: false,
      message: "No header support for language #{language_id}"
    })
  end
end

class MockDocument
  attr_reader :file_name

  def initialize(file_path)
    @file_name = file_path
  end
end

main if __FILE__ == $0
