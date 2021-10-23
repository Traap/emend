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
      @yaml_file.each do |key, value|
        value.each do |node|
          case key
          when 'symlinks'
            @commands << SymLink.new(value, @options)
          when 'repos'
            @commands << Repo.new(value, @options)
          when 'installations'
            @commands << Install.new(value, @options)
          when 'includes'
            @commands << Include.new(node, @options)
          else
            puts "#{key} is not supported."
          end
        end
      end
    end

    # ---------------------------------------------------------------------- }}}
  end
end
# -------------------------------------------------------------------------- }}}
