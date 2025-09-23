#!/usr/bin/env ruby

#*****************************************************************************#
#                                                                             #
#  configuration.rb                                       TTTTTTTT SSSSSSS II #
#                                                            TT    SS      II #
#  By: igors.oleinikovs@students.tsi.lv                      TT    SSSSSSS II #
#                                                            TT         SS II #
#  Created: Sep 23 2025 11:19 igors.oleinikovs               TT    SSSSSSS II #
#  Updated: Sep 23 2025 11:19 igors.oleinikovs                                #
#                                                                             #
#   Transport and Telecommunication Institute - Riga, Latvia                  #
#                       https://tsi.lv                                        #
#*****************************************************************************#

module TSIHeader
  class Configuration
    class << self
      def current_user
        # Try multiple sources in order
        user = get_env_var('TSI_USERNAME') || 
               get_configuration('tsiheader.username') || 
               read_git_config_file('user.name') ||
               get_git_user_name
        
        if user.nil? || user.empty?
          raise "Please configure your username in VS Code settings (tsiheader.username) or set git config user.name"
        end
        user
      end

      def current_user_email
        # Try multiple sources in order  
        email = get_env_var('TSI_EMAIL') || 
                get_configuration('tsiheader.email') || 
                read_git_config_file('user.email') ||
                get_git_user_email
        
        if email.nil? || email.empty?
          raise "Please configure your email in VS Code settings (tsiheader.email) or set git config user.email"
        end
        email
      end

      def email_domain
        'students.tsi.lv'
      end

      def institution_name
        'Transport and Telecommunication Institute - Riga, Latvia'
      end

      def institution_url
        'https://tsi.lv'
      end

      private

      def get_env_var(name)
        value = ENV[name]
        return nil if value.nil? || value.empty?
        value
      end

      def get_configuration(key)
        # This would interface with VS Code's configuration system
        # For now, return nil to use defaults
        nil
      end

      def read_git_config_file(key)
        # Try to read git config file directly
        config_paths = [
          File.expand_path('~/.gitconfig'),
          File.expand_path('~/.config/git/config')
        ]
        
        config_paths.each do |config_path|
          next unless File.exist?(config_path)
          
          begin
            content = File.read(config_path)
            # Look for the key in [user] section
            if content =~ /\[user\](.*?)(?=\n\[|\z)/m
              user_section = $1
              # Look for the specific key (name or email)
              field_name = key.split('.').last
              if user_section =~ /^\s*#{field_name}\s*=\s*(.+?)$/m
                return $1.strip
              end
            end
          rescue => e
            # Continue to next config file
            next
          end
        end
        
        nil
      end

      def get_git_user_name
        # Try to get username from git config
        begin
          name = `git config user.name 2>/dev/null`.strip
          return name.empty? ? nil : name
        rescue => e
          # Git not available or config not set
          nil
        end
      end

      def get_git_user_email
        # Try to get email from git config
        begin
          email = `git config user.email 2>/dev/null`.strip
          return email.empty? ? nil : email
        rescue => e
          # Git not available or config not set
          nil
        end
      end
    end
  end
end