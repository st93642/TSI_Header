#!/usr/bin/env ruby

#*****************************************************************************#
#                                                                             #
#  header_info.rb                                         TTTTTTTT SSSSSSS II #
#                                                            TT    SS      II #
#  By: igors.oleinikovs@students.tsi.lv                      TT    SSSSSSS II #
#                                                            TT         SS II #
#  Created: Sep 23 2025 11:19 igors.oleinikovs               TT    SSSSSSS II #
#  Updated: Sep 23 2025 11:19 igors.oleinikovs                                #
#                                                                             #
#   Transport and Telecommunication Institute - Riga, Latvia                  #
#                       https://tsi.lv                                        #
#*****************************************************************************#

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