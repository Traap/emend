module Emend
  class SymLink < Command

    def initialize(data, options)
      super(data, options)
    end

    def remove_artifact
      puts 'Deleting symbolic links'
      @data.each do |n|
        n['symlink'].each do |s|
          slash = s['directory'] ? '/' : ''
          @command = "rm -frv #{s['link']}#{slash}"
          do_command false
        end
      end
      puts ''
    end

    def install_artifact
      puts 'Making symbolic links'
      @data.each do |n|
        n['symlink'].each do |s|
          @command = "ln -s #{s['file']} #{s['link']}"
          do_command false
        end
      end
      puts ''
    end

  end
end
