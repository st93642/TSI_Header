# frozen_string_literal: true
require 'rspec'
require_relative '../../lib/tsi_header_cli'
require_relative '../../lib/tsi_header'

describe 'TSIHeader CLI Integration' do
  it 'runs CLI and generates header for a file' do
    # Simulate CLI call with valid command
    output = `ruby #{File.expand_path('../../lib/tsi_header_cli.rb', __dir__)} insert c test.c`
    response = JSON.parse(output)
    expect(response['success']).to be true
    expect(response['header']).to include('TTTTTTTT SSSSSSS II')
    expect(response['header']).to include('test.c')
    expect(response['header']).to include('Transport and Telecommunication Institute')
  end
end
