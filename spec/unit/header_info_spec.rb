# frozen_string_literal: true
require 'rspec'
require_relative '../../lib/tsi_header/header_info'
require_relative '../../lib/tsi_header/configuration'

describe TSIHeader::HeaderInfo do
  let(:filename) { 'test_file.c' }
  let(:author) { 'test@students.tsi.lv' }
  let(:created_by) { 'testuser' }
  let(:created_at) { Time.now }
  let(:updated_by) { 'testuser' }
  let(:updated_at) { Time.now }

  it 'initializes with correct attributes' do
    info = described_class.new(
      filename: filename,
      author: author,
      created_by: created_by,
      created_at: created_at,
      updated_by: updated_by,
      updated_at: updated_at
    )
    expect(info.filename).to eq(filename)
    expect(info.author).to eq(author)
    expect(info.created_by).to eq(created_by)
    expect(info.updated_by).to eq(updated_by)
  end

  it 'formats created and updated dates' do
    info = described_class.new(
      filename: filename,
      author: author,
      created_by: created_by,
      created_at: Time.new(2025,9,25,10,0),
      updated_by: updated_by,
      updated_at: Time.new(2025,9,25,12,0)
    )
    expect(info.formatted_created_at).to eq('Sep 25 2025 10:00')
    expect(info.formatted_updated_at).to eq('Sep 25 2025 12:00')
  end

  it 'creates new header from document' do
    doc = Struct.new(:file_name).new('myfile.c')
    info = described_class.new_header(doc)
    expect(info.filename).to eq('myfile.c')
    expect(info.author).to match(/@students\.tsi\.lv|@tsi\.lv/)
  end

  it 'updates header info' do
    doc = Struct.new(:file_name).new('myfile.c')
    info = described_class.new_header(doc)
    updated = described_class.update_header(doc, info)
    expect(updated.filename).to eq('myfile.c')
    expect(updated.updated_by).to eq(info.updated_by)
  end
end
