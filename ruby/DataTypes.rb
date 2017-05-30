#!/usr/local/bin/ruby
# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause
# ------------------------------------------------------------------------------

require 'open3'
require 'yaml'

# ------------------------------------------------------------------------------

class ShellError < StandardError; end

# ------------------------------------------------------------------------------
class Command
  def initialize(data, options)
    @data = data
    @options = options 
    @command = nil
  end
  
  def install_artifact; end

  def remove_artifact; end

  def do_command
    echo_command if @options.verbose || @options.dryrun
    run_command  if !@options.dryrun
  end

  def echo_command
    puts @command
  end

  def run_command
    begin
      status = system(@command)
    rescue ShellError
      abort "System command failed: #{status}"
    end
  end

end # End Command

# ------------------------------------------------------------------------------
class SymLink < Command
  def initialize(data, options)
    super(data, options)
  end

  def remove_artifact 
    puts "Deleting symbolic links"
    @data.each do |n|
      n['symlink'].each do |s|
       @command = "rm -frv #{s['link']}"
       do_command
      end
    end
    puts ""
  end

  def install_artifact 
    puts "Making symbolic links"
    @data.each do |n|
      n['symlink'].each do |s|
        @command = "ln -s #{s['file']} #{s['link']}"
        do_command
      end
    end
    puts ""
  end
end # End SymLink

# ------------------------------------------------------------------------------
class Repo < Command
  def initialize(data, options)
    super(data, options)
  end

  def install_artifact 
    puts "Cloning repositories"
    @data.each do |n|
      n['paths'].each do |p|
        @command = "git clone #{n['url']}/#{p['source']} #{p['target']}"
        do_command
      end
    end
    puts ""
  end
end # End Repo

# ------------------------------------------------------------------------------
class Install < Command
  def initialize(data, options)
    super(data, options)
  end
  
  def install_artifact
    puts "Installing programs"
    @data.each do |n|
      n['command'].each do |c|
        if c['sudo'] then
          @command = "sudo #{c['program']} #{c['argument']}"
        else
          @command ="#{c['program']} #{c['argument']}"
        end
        do_command
      end
    end
    puts ""
  end
end # End Install
# ------------------------------------------------------------------------------
