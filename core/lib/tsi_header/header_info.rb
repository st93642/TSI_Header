#!/usr/bin/env ruby



require 'time'

module TSIHeader
  class HeaderInfo
    attr_accessor :filename, :author, :created_by, :created_at, :updated_by, :updated_at

    def initialize(filename:, author:, created_by:, created_at:, updated_by:, updated_at:)
      @filename = filename
      @author = author
      @created_by = created_by
      @created_at = created_at
      @updated_by = updated_by
      @updated_at = updated_at
    end

    def self.new_header(document)
      user = Configuration.current_user
      email = Configuration.current_user_email
      now = Time.now

      new(
        filename: File.basename(document.file_name),
        author: email,  # Use email directly instead of constructing
        created_by: user,
        created_at: now,
        updated_by: user,
        updated_at: now
      )
    end

    def self.update_header(document, existing_header_info = nil)
      user = Configuration.current_user
      email = Configuration.current_user_email
      now = Time.now

      if existing_header_info
        existing_header_info.filename = File.basename(document.file_name)
        existing_header_info.author = email  # Use email directly
        # Update created_by if it's unknown or empty (fix for broken headers)
        if existing_header_info.created_by.nil? || existing_header_info.created_by.empty? || existing_header_info.created_by == "unknown"
          existing_header_info.created_by = user
        end
        existing_header_info.updated_by = user
        existing_header_info.updated_at = now
        existing_header_info
      else
        new_header(document)
      end
    end

    def formatted_created_at
      @created_at.strftime("%b %d %Y %H:%M")
    end

    def formatted_updated_at
      @updated_at.strftime("%b %d %Y %H:%M")
    end
  end
end