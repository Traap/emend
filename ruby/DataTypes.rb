#!/usr/local/bin/ruby
# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause

require 'yaml'

# ------------------------------------------------------------------------------
class SymLink
  def initialize(data)
    @data = data
  end

  def delete_symlinks
    puts "Deleting symbolic links"
    @data.each do |n|
       puts "rm -frv #{n['link']}"
    end
    puts ""
  end

  def make_symlinks
    puts "Making symbolic links"
    @data.each do |n|
      puts "rm -frv #{n['link']}"
    end
    puts ""
  end
end

# ------------------------------------------------------------------------------
class Repos
  def initialize(data)
    @data = data
  end

  def clone_repository
    puts "Cloning repositories"
    @data.each do |n|
      n['paths'].each do |p|
        puts "git clone #{n['url']}/#{p['source']} #{p['target']}"
      end
    end
    puts ""
  end
end

# ------------------------------------------------------------------------------
class Install
  def initialize(data)
    @data = data
  end
  
  def install_programs
    puts "Installing programs"
    @data.each do |n|
      n['command'].each do |c|
        if c['sudo'] then
          puts "sudo #{c['program']} #{c['argument']}"
        else
          puts "#{c['program']} #{c['argument']}"
        end
      end
    end
    puts ""
  end
end

# ------------------------------------------------------------------------------
