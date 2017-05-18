#!/usr/local/bin/ruby
# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause
# ------------------------------------------------------------------------------

require 'pp'
require 'yaml'

require_relative 'DataTypes'
require_relative 'Options'

# ------------------------------------------------------------------------------
class Workflow 
  def initialize options
    @options = options
    @yaml_file = nil
    @symlink = nil
    @repo = nil
    @install = nil
  end

# ------------------------------------------------------------------------------
  def parse_yaml_file(yaml_file)
    puts "Parsing #{yaml_file}"
    @yaml_file = YAML.load(File.open(yaml_file))
    @yaml_file.each do |k,v|
      v.each do |n|
        case k
        when "symlinks"
          @symlink = SymLink.new(v)
        when "repos"
          @repo = Repo.new(v)
        when "installations"
          @install = Install.new(v)
        else
          puts "#{k} is not supported."
        end
      end
    end
  end

# ------------------------------------------------------------------------------
  def orchestrate
    puts "orchestrate"

    @options.filename.each do |f|
      parse_yaml_file f

      if @symlink 
        @symlink.delete_symlinks
      end
      if @repo
        @repo.clone_repository
      end 
      if @symlink 
        @symlink.make_symlinks
      end
      if @install
        @install.install_programs
      end
    end
  end

# ------------------------------------------------------------------------------
end # End Workflow
# ------------------------------------------------------------------------------
