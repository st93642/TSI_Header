# frozen_string_literal: true
require 'rspec'
require_relative '../../lib/tsi_header/header_generator'
require_relative '../../lib/tsi_header/header_info'

describe TSIHeader::HeaderGenerator do
  let(:header_info) do
    TSIHeader::HeaderInfo.new(
      filename: 'test.c',
      author: 'test@students.tsi.lv',
      created_by: 'testuser',
      created_at: Time.new(2025,9,25,10,0),
      updated_by: 'testuser',
      updated_at: Time.new(2025,9,25,12,0)
    )
  end

  it 'supports known languages' do
    expect(described_class.supports_language?('c')).to be true
    expect(described_class.supports_language?('python')).to be true
  end

  it 'does not support unknown languages' do
    expect(described_class.supports_language?('foobar')).to be false
  end

  it 'renders header for C language with correct formatting' do
    header = described_class.render_header('c', header_info)
    lines = header.split("\n")
    expect(lines.length).to eq(12)
    expect(lines.all? { |l| l.length == 79 }).to be true
    expect(header).to include('TTTTTTTT SSSSSSS II')
    expect(header).to include('Transport and Telecommunication Institute')
    expect(header).to include('https://tsi.lv')
    expect(header).to include('test.c')
  end

  it 'renders header for Python with correct formatting' do
    header = described_class.render_header('python', header_info)
    lines = header.split("\n")
    expect(lines.length).to eq(12)
    expect(lines.all? { |l| l.length == 79 }).to be true
    expect(header).to include('TTTTTTTT SSSSSSS II')
    expect(header).to include('Transport and Telecommunication Institute')
    expect(header).to include('https://tsi.lv')
    expect(header).to include('test.c')
  end
end
