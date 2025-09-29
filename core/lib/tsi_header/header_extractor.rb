module TSIHeader
  class HeaderExtractor
    # More flexible pattern to match TSI header
    # Look for the TSI header pattern that contains "Transport and Telecommunication Institute"
    HEADER_PATTERN = /\/\*+\*\/\s*\n(?:\/\*.*?\*\/\s*\n){10}\/\*+\*\//

    def self.extract_header(text)
      # First try the exact pattern (12 lines of 79 chars each)
      exact_match = text.match(/^(.{79}(\r?\n)){11}.{79}/)
      return exact_match[0].gsub(/\r\n/, "\n") if exact_match

      # If exact match fails, try the more flexible pattern
      lines = text.split(/\r?\n/)
      header_lines = []
      in_header = false

      lines.each do |line|
        if line =~ /\/\*+\*\/$/ && !in_header
          # Start of header
          in_header = true
          header_lines << line
        elsif in_header
          header_lines << line
          # Check if this is the end of header
          if line =~ /\/\*+\*\/$/ && header_lines.length >= 10
            # Found complete header
            return header_lines.join("\n") + "\n"
          end
        end
      end

      nil
    end

    def self.has_header?(text)
      !extract_header(text).nil?
    end
  end
end