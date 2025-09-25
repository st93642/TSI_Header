# frozen_string_literal: true
require 'rspec'
require_relative '../../lib/tsi_header/delimiters'

describe TSIHeader::Delimiters do
  describe '.for_language' do
    it 'returns delimiters for known language' do
      expect(TSIHeader::Delimiters.for_language('c')).to be_a(Array)
      expect(TSIHeader::Delimiters.for_language('c').length).to eq(6)
    end
    it 'returns default delimiters for unknown language' do
      expect(TSIHeader::Delimiters.for_language('unknown')).to eq(TSIHeader::Delimiters::SLASHES)
    end
  end

  describe '.supports_language?' do
    it 'returns true for supported language' do
      expect(TSIHeader::Delimiters.supports_language?('python')).to be true
    end
    it 'returns false for unsupported language' do
      expect(TSIHeader::Delimiters.supports_language?('foobar')).to be false
    end
  end
end
