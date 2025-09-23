require 'time'

module TSIHeader
  class HeaderParser
  def self.parse_header(header_text)
    lines = header_text.split("\n")
    
    # Extract filename from line 2 (0-indexed)
    filename_line = lines[2] || ""
    filename_match = filename_line.match(/\s+(\S+\.\w+)/)
    filename = filename_match ? filename_match[1] : "unknown"

    # Extract author from line 4
    author_line = lines[4] || ""
    author_match = author_line.match(/By:\s+(\S+@\S+)/)
    author = author_match ? author_match[1] : "unknown@students.tsi.lv"

    # Extract created info from line 6 (0-indexed)
    created_line = lines[6] || ""
    # Format: "/*  Created: Sep 23 2025 12:30 Igors Oleinikovs                        TT    SSSSSSS II */"
    created_match = created_line.match(/Created:\s+(\w{3}\s+\d{1,2}\s+\d{4}\s+\d{2}:\d{2})\s+([a-zA-Z0-9._\s-]+?)\s+TT/)
    if created_match
      created_at_str = created_match[1]
      created_by = created_match[2].strip
      created_at = parse_date(created_at_str)
    else
      created_at = Time.now
      created_by = "unknown"
    end

    # Extract updated info from line 7 (0-indexed)
    updated_line = lines[7] || ""
    # Format: "/*  Updated: Sep 23 2025 12:31 Igors Oleinikovs                              */"
    updated_match = updated_line.match(/Updated:\s+(\w{3}\s+\d{1,2}\s+\d{4}\s+\d{2}:\d{2})\s+([a-zA-Z0-9._\s-]+?)\s+\*\//)
    if updated_match
      updated_at_str = updated_match[1]
      updated_by = updated_match[2].strip
      updated_at = parse_date(updated_at_str)
    else
      updated_at = Time.now
      updated_by = "unknown"
    end

    HeaderInfo.new(
      filename: filename,
      author: author,
      created_by: created_by,
      created_at: created_at,
      updated_by: updated_by,
      updated_at: updated_at
    )
  end

  private

    def self.parse_date(date_str)
      # Parse "Sep 23 2025 10:02" format
      Time.strptime(date_str.strip, "%b %d %Y %H:%M")
    rescue ArgumentError
      # Fallback if parsing fails
      Time.now
    end
  end
end