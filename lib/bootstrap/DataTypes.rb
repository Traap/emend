#!/usr/local/bin/ruby
# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause
# ------------------------------------------------------------------------------

require 'open3'
require 'rbconfig'
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
       do_command false
      end
    end
    puts ""
  end

  def install_artifact 
    puts "Making symbolic links"
    @data.each do |n|
      n['symlink'].each do |s|
        @command = "ln -s #{s['file']} #{s['link']}"
        do_command false
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
        do_command false
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
      n['os'].each do |o|
        if install_on_this_os? o['name'] then
          o['command'].each do |c|
            if c['sudo'] then
              @command = "sudo #{c['program']} #{c['argument']}"
            else
              @command ="#{c['program']} #{c['argument']}"
            end
            do_command false
          end
        end
      end
    end
    puts ""
  end

  def install_on_this_os?(os)
    return true if os == "any"
    return true if RbConfig::CONFIG["host_os"].start_with? os
  end

end # End Install

# ------------------------------------------------------------------------------
class Include < Command
  def initialize(data, options)
    super(data, options)
  end

  def install_artifact 
    @data.each do |n|
      n['file'].each do |f|
        @command ="ruby ruby/bootstrap.rb --file " + f['name']
        @command.concat " --verbose"  if @options.verbose
        @command.concat " --nodryrun" if !@options.dryrun
        puts "Including file " + f['name']
        puts
        do_command true
      end
    end
  end
end # End Repo

# ------------------------------------------------------------------------------