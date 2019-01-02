# Substitute the following strings when process a YAML files.
#   The home directory
#     ~
#     ${home}
#     {$HOME}
#
require 'fileutils'

module Emend 
  module Substitute
    def self.expand_path(text)
      unless text.nil?
        expanded_text = ''
        words = text.split(' ')
        words.each do |w|
          if w.start_with?('~')
            expanded_text.concat File.expand_path(w)
          else
            expanded_text.concat w
          end
          expanded_text.concat ' '
        end
        expanded_text.strip
      end
    end
  end
end
