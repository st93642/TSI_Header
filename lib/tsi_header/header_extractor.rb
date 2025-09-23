module TSIHeader
  class HeaderExtractor
    # Pattern to match TSI header (12 lines, 79 characters each)
    HEADER_PATTERN = /^(.{79}(\r?\n)){11}.{79}/

    def self.extract_header(text)
      match = text.match(HEADER_PATTERN)
      match ? match[0].gsub(/\r\n/, "\n") : nil
    end

    def self.has_header?(text)
      !extract_header(text).nil?
    end
  end
end