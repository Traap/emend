module Emend
  # ----------------------------------------------------------------------------
  class ShellError < StandardError; end
  # ----------------------------------------------------------------------------
  class Command
    def initialize(data, options)
      @data = data
      @options = options
      @command = nil
    end

    def install_artifact; end

    def remove_artifact; end

    protected

    def do_command(included_file)
      echo_command if included_file || @options.verbose || @options.dryrun
      run_command  if included_file || !@options.dryrun
    end

    private

    def echo_command
      puts @command
    end

    def run_command
      status = system(Emend::Substitute.expand_path(@command))
    rescue ShellError
      abort "System command failed: #{status}"
    end
  end

  # ----------------------------------------------------------------------------
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

  # ----------------------------------------------------------------------------
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

  # ----------------------------------------------------------------------------
  class Install < Command
    def initialize(data, options)
      super(data, options)
    end

    def install_artifact
      puts 'Installing programs'
      @data.each do |n|
        n['os'].each do |o|
          next unless install_on_this_os? o['name']
          o['command'].each do |c|
            sudo = sudo_or_nil c
            @command = "#{sudo}#{c['program']} #{c['argument']}"
            do_command false
          end
        end
      end
      puts ''
    end

    def sudo_or_nil(cmd)
      return nil unless cmd['sudo']
      %w[cygwin mingw32].include?(RbConfig::CONFIG['host_os']) ? nil : 'sudo '
    end

    def install_on_this_os?(opsys)
      return true if opsys == 'any'
      return true if RbConfig::CONFIG['host_os'].start_with? opsys
    end
  end

  # ----------------------------------------------------------------------------
  class Include < Command
    def initialize(data, options)
      super(data, options)
    end

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
  # ----------------------------------------------------------------------------
end
# ------------------------------------------------------------------------------
