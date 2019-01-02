module Emend
  class Repo < Command

    def initialize(data, options)
      super(data, options)
    end

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
