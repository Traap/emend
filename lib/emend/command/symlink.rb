# frozen_string_literal: true

module Emend
  # This class creates symbolic links.
  class SymLink < Command
    def remove_artifact
      puts 'Deleting symbolic links'
      @data.each do |node|
        node['symlink'].each do |link|
          slash = symlink['directory'] ? '/' : ''
          @command = "rm -frv #{link['link']}#{slash}"
          do_command false
        end
      end
      puts ''
    end

    def install_artifact
      puts 'Making symbolic links'
      @data.each do |node|
        node['symlink'].each do |link|
          my_do_command link
        end
      end
      puts ''
    end

    # --------------------------------------------------------------------------
    # my_do_command, my_echo_command, and my_run_command are defined to replace
    # do_command, echo_command, and run_command provided by Emend::Command base
    # class.
    #
    # I have added these methods so that I can directly call FileUtils.ln_s
    # to create symbolic links instead of calling the system command with
    # a string representing the operating system command.  I've done this
    # because Ruby's FileUtils.ln_s command is operating system aware.  Linux
    # systems will use the ln command and Windows systems will use the mklink
    # command under the hood.
    #
    def my_do_command(link)
      my_echo_command(link) if @options.verbose || @options.dryrun
      my_run_command(link)  unless @options.dryrun
    end

    def my_echo_command(link)
      @command = "ln -s #{link['file']} #{link['link']}"
      puts @command
    end

    def my_run_command(cmd)
      file = Emend::Substitute.expand_path (cmd['file']).to_s
      link = Emend::Substitute.expand_path (cmd['link']).to_s
      options = { force: true }
      # options = {:force => true, :verbose => true}
      FileUtils.ln_s file, link, options
    rescue StandardError => e
      puts e.message
    end
  end
end
