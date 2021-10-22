# frozen_string_literal: true

# {{{ Copyright (c) Gary Allan Howard aka Traap.
#
# License BSD-3-Clause
#
# -------------------------------------------------------------------------- }}}
# {{{ Orchestrate the workflow.

module Emend
  # Workflow
  class Workflow
    # {{{ Initialize

    def initialize(options)
      @options = options
      @yaml_file = nil
      @commands = []
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Orchestrate

    def orchestrate
      @options.filename.each do |f|
        parse_yaml_file f
        @commands.each do |c|
          c.remove_artifact
          c.install_artifact
        end
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Parse the YAML file.

    def parse_yaml_file(yaml_file)
      puts "Parsing #{yaml_file}"
      @commands = []
      @yaml_file = YAML.safe_load(File.open(yaml_file))
      parse_each_command_type
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Parse each command type.

    def parse_each_command_type
      @yaml_file.each do |key, value|
        parse_each_node(key, value)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Parse each node.

    def parse_each_node(key, value)
      value.each do |node|
        make_the_command(node, key, value)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Make the commands.

    def make_the_command(node, key, value)
      command = nil
      command << SymLink.new(value, @options) if key.eql? 'symlinks'
      command << Repo.new(value, @options)    if key.eql? 'repos'
      command << Install.new(value, @options) if key.eql? 'installations'
      command << Include.new(node, @options)  if key.eql? 'includes'
      if command.nil?
        puts "#{key} is not supported."
      else
        @commands << command
      end
    end

    # ---------------------------------------------------------------------- }}}
  end
end
# -------------------------------------------------------------------------- }}}
