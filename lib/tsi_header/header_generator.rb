#!/usr/bin/env ruby

#*****************************************************************************#
#                                                                             #
#  header_generator.rb                                    TTTTTTTT SSSSSSS II #
#                                                            TT    SS      II #
#  By: st93642@students.tsi.lv                      TT    SSSSSSS II #
#                                                            TT         SS II #
#  Created: Sep 23 2025 11:19 st93642               TT    SSSSSSS II #
#  Updated: Sep 23 2025 11:19 st93642                                #
#                                                                             #
#   Transport and Telecommunication Institute - Riga, Latvia                  #
#                       https://tsi.lv                                        #
#*****************************************************************************#

module TSIHeader
  class HeaderGenerator
    def self.supports_language?(language_id)
      Delimiters.supports_language?(language_id)
    end

    def self.render_header(language_id, header_info)
      # Special handling for Python with frame format
      if language_id == 'python'
        return render_python_frame_header(header_info)
      end
      
      delimiters = Delimiters.for_language(language_id)
      
      # Handle languages without comment delimiters (plain text)
      if delimiters.empty?
        return render_plain_text_header(header_info)
      end
      
      top_left, top_right, left, right, bottom_left, bottom_right = delimiters

      # Calculate content width based on content delimiter lengths (left/right)
      total_width = 79
      content_width = total_width - left.length - right.length
      
      # Calculate star width based on top/bottom delimiter lengths
      star_width = total_width - top_left.length - top_right.length

      # Build the header line by line exactly like the example
      lines = []
      
      # Line 1: Top border
      lines << "#{top_left}#{'*' * star_width}#{top_right}"
      
      # Line 2: Empty line
      lines << "#{left}#{' ' * content_width}#{right}"
      
      # Line 3: Filename and TSI logo (right-aligned logo)
      filename_part = "  #{header_info.filename}"
      logo_part = "TTTTTTTT SSSSSSS II"
      # Calculate padding to right-align logo with 1 space from right wall
      logo_start_pos = content_width - logo_part.length - 1 # 1 space from right
      text_padding = logo_start_pos - filename_part.length
      if text_padding < 1
        text_padding = 1
        filename_part = filename_part[0...(logo_start_pos - 1)]
      end
      lines << "#{left}#{filename_part}#{' ' * text_padding}#{logo_part} #{right}"
      
      # Line 4: Second logo line (right-aligned)
      logo_part = "TT    SS      II"
      logo_start_pos = content_width - logo_part.length - 1
      lines << "#{left}#{' ' * logo_start_pos}#{logo_part} #{right}"
      
      # Line 5: Author line with logo (right-aligned logo)
      author_part = "  By: #{header_info.author}"
      logo_part = "TT    SSSSSSS II"
      logo_start_pos = content_width - logo_part.length - 1
      text_padding = logo_start_pos - author_part.length
      if text_padding < 1
        text_padding = 1
        author_part = author_part[0...(logo_start_pos - 1)]
      end
      lines << "#{left}#{author_part}#{' ' * text_padding}#{logo_part} #{right}"
      
      # Line 6: Third logo line (right-aligned)
      logo_part = "TT         SS II"
      logo_start_pos = content_width - logo_part.length - 1
      lines << "#{left}#{' ' * logo_start_pos}#{logo_part} #{right}"
      
      # Line 7: Created line with logo (right-aligned logo)
      created_part = "  Created: #{header_info.formatted_created_at} #{header_info.created_by}"
      logo_part = "TT    SSSSSSS II"
      logo_start_pos = content_width - logo_part.length - 1
      text_padding = logo_start_pos - created_part.length
      if text_padding < 1
        text_padding = 1
        created_part = created_part[0...(logo_start_pos - 1)]
      end
      lines << "#{left}#{created_part}#{' ' * text_padding}#{logo_part} #{right}"
      
      # Line 8: Updated line
      updated_part = "  Updated: #{header_info.formatted_updated_at} #{header_info.updated_by}"
      padding_needed = content_width - updated_part.length
      if padding_needed < 0
        padding_needed = 0
        updated_part = updated_part[0...(content_width - 1)]
      end
      lines << "#{left}#{updated_part}#{' ' * padding_needed}#{right}"
      
      # Line 9: Empty line
      lines << "#{left}#{' ' * content_width}#{right}"
      
      # Line 10: Institution name
      inst_line = "   Transport and Telecommunication Institute - Riga, Latvia"
      padding_needed = content_width - inst_line.length
      lines << "#{left}#{inst_line}#{' ' * padding_needed}#{right}"
      
      # Line 11: Institution URL
      url_line = "                       https://tsi.lv"
      padding_needed = content_width - url_line.length
      lines << "#{left}#{url_line}#{' ' * padding_needed}#{right}"
      
      # Line 12: Bottom border
      lines << "#{bottom_left}#{'*' * star_width}#{bottom_right}"
      
      # Join all lines with newlines and add final newline
      lines.join("\n") + "\n"
    end

    private

    def self.render_plain_text_header(header_info)
      lines = []
      
      # Line 1: Top border
      lines << "*" * 79
      
      # Line 2: Empty line
      lines << " " * 79
      
      # Line 3: Filename and TSI logo (right-aligned logo)
      filename_part = "  #{header_info.filename}"
      logo_part = "TTTTTTTT SSSSSSS II"
      # Calculate padding to right-align logo with 1 space from right wall
      content_width = 79
      logo_start_pos = content_width - logo_part.length - 1 # 1 space from right
      text_padding = logo_start_pos - filename_part.length
      if text_padding < 1
        text_padding = 1
        filename_part = filename_part[0...(logo_start_pos - 1)]
      end
      lines << "#{filename_part}#{' ' * text_padding}#{logo_part} "
      
      # Line 4: Second logo line (right-aligned)
      logo_part = "TT    SS      II"
      logo_start_pos = content_width - logo_part.length - 1
      lines << "#{' ' * logo_start_pos}#{logo_part} "
      
      # Line 5: Author line with logo (right-aligned logo)
      author_part = "  By: #{header_info.author}"
      logo_part = "TT    SSSSSSS II"
      logo_start_pos = content_width - logo_part.length - 1
      text_padding = logo_start_pos - author_part.length
      if text_padding < 1
        text_padding = 1
        author_part = author_part[0...(logo_start_pos - 1)]
      end
      lines << "#{author_part}#{' ' * text_padding}#{logo_part} "
      
      # Line 6: Third logo line (right-aligned)
      logo_part = "TT         SS II"
      logo_start_pos = content_width - logo_part.length - 1
      lines << "#{' ' * logo_start_pos}#{logo_part} "
      
      # Line 7: Created line with logo (right-aligned logo)
      created_part = "  Created: #{header_info.formatted_created_at} #{header_info.created_by}"
      logo_part = "TT    SSSSSSS II"
      logo_start_pos = content_width - logo_part.length - 1
      text_padding = logo_start_pos - created_part.length
      if text_padding < 1
        text_padding = 1
        created_part = created_part[0...(logo_start_pos - 1)]
      end
      lines << "#{created_part}#{' ' * text_padding}#{logo_part} "
      
      # Line 8: Updated line
      updated_part = "  Updated: #{header_info.formatted_updated_at} #{header_info.updated_by}"
      padding_needed = content_width - updated_part.length
      if padding_needed < 0
        padding_needed = 0
        updated_part = updated_part[0...(content_width - 1)]
      end
      lines << "#{updated_part}#{' ' * padding_needed}"
      
      # Line 9: Empty line
      lines << " " * 79
      
      # Line 10: Institution name
      inst_line = "   Transport and Telecommunication Institute - Riga, Latvia"
      padding_needed = content_width - inst_line.length
      lines << "#{inst_line}#{' ' * padding_needed}"
      
      # Line 11: Institution URL
      url_line = "                       https://tsi.lv"
      padding_needed = content_width - url_line.length
      lines << "#{url_line}#{' ' * padding_needed}"
      
      # Line 12: Bottom border
      lines << "*" * 79
      
      # Join all lines with newlines and add final newline
      lines.join("\n") + "\n"
    end

    def self.render_python_frame_header(header_info)
      lines = []
      
      # Line 1: Top border with frame
      lines << "#" + "*" * 77 + "#"
      
      # Line 2: Empty line with frame
      lines << "#" + " " * 77 + "#"
      
      # Line 3: Filename and TSI logo
      filename_part = "  #{header_info.filename}"
      logo_part = "TTTTTTTT SSSSSSS II"
      # Calculate padding to right-align logo with 1 space from right wall
      total_content_width = 77 # Total width minus frame characters
      logo_start_pos = total_content_width - logo_part.length - 1 # 1 space from right
      text_padding = logo_start_pos - filename_part.length
      if text_padding < 1
        text_padding = 1
        filename_part = filename_part[0...(logo_start_pos - 1)]
      end
      lines << "##{filename_part}#{' ' * text_padding}#{logo_part} #"
      
      # Line 4: Second logo line (right-aligned)
      logo_part = "TT    SS      II"
      logo_start_pos = total_content_width - logo_part.length - 1
      lines << "##{' ' * logo_start_pos}#{logo_part} #"
      
      # Line 5: Author line with logo (right-aligned logo)
      author_part = "  By: #{header_info.author}"
      logo_part = "TT    SSSSSSS II"
      logo_start_pos = total_content_width - logo_part.length - 1
      text_padding = logo_start_pos - author_part.length
      if text_padding < 1
        text_padding = 1
        author_part = author_part[0...(logo_start_pos - 1)]
      end
      lines << "##{author_part}#{' ' * text_padding}#{logo_part} #"
      
      # Line 6: Third logo line (right-aligned)
      logo_part = "TT         SS II"
      logo_start_pos = total_content_width - logo_part.length - 1
      lines << "##{' ' * logo_start_pos}#{logo_part} #"
      
      # Line 7: Created line with logo (right-aligned logo)
      created_part = "  Created: #{header_info.formatted_created_at} #{header_info.created_by}"
      logo_part = "TT    SSSSSSS II"
      logo_start_pos = total_content_width - logo_part.length - 1
      text_padding = logo_start_pos - created_part.length
      if text_padding < 1
        text_padding = 1
        created_part = created_part[0...(logo_start_pos - 1)]
      end
      lines << "##{created_part}#{' ' * text_padding}#{logo_part} #"
      
      # Line 8: Updated line
      updated_part = "  Updated: #{header_info.formatted_updated_at} #{header_info.updated_by}"
      padding_needed = 77 - updated_part.length
      if padding_needed < 0
        padding_needed = 0
        updated_part = updated_part[0..76]
      end
      lines << "##{updated_part}#{' ' * padding_needed}#"
      
      # Line 9: Empty line with frame
      lines << "#" + " " * 77 + "#"
      
      # Line 10: Institution name
      inst_line = "   Transport and Telecommunication Institute - Riga, Latvia"
      padding_needed = 77 - inst_line.length
      lines << "##{inst_line}#{' ' * padding_needed}#"
      
      # Line 11: Institution URL
      url_line = "                       https://tsi.lv"
      padding_needed = 77 - url_line.length
      lines << "##{url_line}#{' ' * padding_needed}#"
      
      # Line 12: Bottom border with frame
      lines << "#" + "*" * 77 + "#"
      
      # Join all lines with newlines and add final newline
      lines.join("\n") + "\n"
    end
  end
end