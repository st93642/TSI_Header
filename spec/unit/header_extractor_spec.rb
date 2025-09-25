# frozen_string_literal: true
require 'rspec'
require_relative '../../lib/tsi_header/header_extractor'

describe TSIHeader::HeaderExtractor do
  let(:valid_header) { "*" * 79 + "\n" + (" " * 79 + "\n") * 10 + "*" * 79 }
  let(:invalid_header) { "random text\n" * 12 }

  it 'extracts header from valid text' do
    text = valid_header
    expect(described_class.extract_header(text)).to be_a(String)
    expect(described_class.has_header?(text)).to be true
  end

  it 'returns nil for invalid header' do
    text = invalid_header
    expect(described_class.extract_header(text)).to be_nil
    expect(described_class.has_header?(text)).to be false
  end
end
