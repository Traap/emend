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
    @commands = []
  end

# ------------------------------------------------------------------------------
  def orchestrate
    @options.filename.each do |f|
      parse_yaml_file f
      @commands.each do |c| 
        c.remove_artifact
        c.install_artifact
      end
    end
  end

# ------------------------------------------------------------------------------
  def parse_yaml_file(yaml_file)
    puts "Parsing #{yaml_file}"
    @commands = []
    @yaml_file = YAML.load(File.open(yaml_file))
    @yaml_file.each do |k,v|
      v.each do |n|
        case k
        when "symlinks"
          @commands << SymLink.new(v, @options)
        when "repos"
          @commands << Repo.new(v, @options)
        when "installations"
          puts "installations #{v}\n"
          @commands << Install.new(v, @options)
        else
          puts "#{k} is not supported."
        end
      end
    end
  end

# ------------------------------------------------------------------------------
end # End Workflow
# ------------------------------------------------------------------------------
