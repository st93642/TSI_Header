# frozen_string_literal: true
require 'rspec'
require_relative '../../lib/tsi_header/header_parser'
require_relative '../../lib/tsi_header/header_info'

describe TSIHeader::HeaderParser do
  let(:header_text) do
    "/*" + "*" * 77 + "*/\n" +
    "/*" + " " * 77 + "*/\n" +
    "/*  test.c" + " " * 54 + "TTTTTTTT SSSSSSS II */\n" +
    "/*" + " " * 62 + "TT    SS      II */\n" +
    "/*  By: test@students.tsi.lv" + " " * 38 + "TT    SSSSSSS II */\n" +
    "/*" + " " * 62 + "TT         SS II */\n" +
    "/*  Created: Sep 25 2025 10:00 testuser" + " " * 24 + "TT    SSSSSSS II */\n" +
    "/*  Updated: Sep 25 2025 12:00 testuser" + " " * 27 + "*/\n" +
    "/*" + " " * 77 + "*/\n" +
    "/*   Transport and Telecommunication Institute - Riga, Latvia" + " " * 8 + "*/\n" +
    "/*                       https://tsi.lv" + " " * 32 + "*/\n" +
    "/*" + "*" * 77 + "*/"
  end

  it 'parses header and extracts correct info' do
    info = described_class.parse_header(header_text)
    expect(info.filename).to eq('test.c')
    expect(info.author).to eq('test@students.tsi.lv')
    expect(info.created_by).to eq('testuser')
    expect(info.updated_by).to eq('testuser')
    expect(info.formatted_created_at).to eq('Sep 25 2025 10:00')
    expect(info.formatted_updated_at).to eq('Sep 25 2025 12:00')
  end
end
