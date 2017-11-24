# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause
# ------------------------------------------------------------------------------
module Emend 
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
          @commands << Install.new(v, @options)
        when "includes"
          @commands << Include.new(n, @options)
        else
          puts "#{k} is not supported."
        end
      end
    end
  end

# ------------------------------------------------------------------------------
end # End Workflow
# ------------------------------------------------------------------------------
end # module
# ------------------------------------------------------------------------------
