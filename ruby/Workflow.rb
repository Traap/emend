#!/usr/local/bin/ruby
# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause

require 'yaml'
require_relative 'DataTypes'

class Workflow 
  def initialize(yaml_file)
    @yaml_file = YAML.load(File.open(yaml_file))
    @yaml_file.each do |k,v|
      v.each do |n|
        case k
        when "symlinks"
          @symlink = SymLink.new(v)
        when "repos"
          @repo = Repos.new(v)
        when "installations"
          @install = Install.new(v)
        else
          puts "#{k} is not supported."
        end
      end
    end
  end

  def orchestrate
    @symlink.delete_symlinks
    @repo.clone_repository
    @symlink.make_symlinks
    @install.install_programs
  end

end

