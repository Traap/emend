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
          my_do_command s 
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
    def my_do_command(s)
      my_echo_command(s) if @options.verbose || @options.dryrun
      my_run_command(s)  if !@options.dryrun
    end

    def my_echo_command(s)
      @command = "ln -s #{s['file']} #{s['link']}"
      puts @command
    end

    def my_run_command(s)
      begin 
        file = Emend::Substitute.expand_path "#{s['file']}" 
        link = Emend::Substitute.expand_path "#{s['link']}"
        options = {:force => true}
        # options = {:force => true, :verbose => true}
        FileUtils.ln_s file, link, options 
      rescue StandardError => e
        puts e.message
      end
    end

  end
end
