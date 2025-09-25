# frozen_string_literal: true
require 'rspec'
require_relative '../../lib/tsi_header/configuration'

describe TSIHeader::Configuration do
  describe '.current_user' do
    it 'returns a username from ENV, config, or git' do
      expect(TSIHeader::Configuration.current_user).to be_a(String)
      expect(TSIHeader::Configuration.current_user).not_to be_empty
    end
  end

  describe '.current_user_email' do
    it 'returns an email from ENV, config, or git' do
      expect(TSIHeader::Configuration.current_user_email).to match(/@students\.tsi\.lv|@tsi\.lv/)
    end
  end

  describe '.institution_name' do
    it 'returns the institution name' do
      expect(TSIHeader::Configuration.institution_name).to include('Transport and Telecommunication Institute')
    end
  end

  describe '.institution_url' do
    it 'returns the institution url' do
      expect(TSIHeader::Configuration.institution_url).to eq('https://tsi.lv')
    end
  end
end
