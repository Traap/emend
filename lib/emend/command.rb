# frozen_string_literal: true

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
end
# ------------------------------------------------------------------------------
