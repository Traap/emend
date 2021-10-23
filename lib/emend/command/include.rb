# frozen_string_literal: true

module Emend
  # This class includes an app, a bundle, or a file command.
  class Include < Command
    def install_artifact
      @data.each do |k, v|
        case k
        when 'app'
          include_this_file v, '--app'
        when 'bundle'
          include_this_file v, '--bundle'
        when 'file'
          include_this_file v, '--file'
        end
      end
    end

    def include_this_file(name, opt)
      opt_and_files = "#{opt}=#{name.map(&:values).join(',')}"
      @command = "emend #{opt_and_files}"
      @command.concat ' --verbose'  if @options.verbose
      @command.concat ' --nodryrun' unless @options.dryrun
      puts "Including #{opt_and_files}"
      puts
      do_command true
    end
  end
end
