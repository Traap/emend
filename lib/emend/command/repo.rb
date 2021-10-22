# frozen_string_literal: true

module Emend
  # This class clones a repository.
  class Repo < Command
    def install_artifact
      puts 'Cloning repositories'
      @data.each do |n|
        n['paths'].each do |p|
          @command = "git clone #{n['url']}/#{p['source']} #{p['target']}"
          do_command false
        end
      end
      puts ''
    end
  end
end
