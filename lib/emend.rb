# frozen_string_literal: true

# {{{ Copyright (c) Gary Allan Howard aka Traap.
#
# License BSD-3-Clause
#
# -------------------------------------------------------------------------- }}}
# {{{ Program purpose.
#
# This program is used to emend a development environment.  emending
# consists of three parts, namely: 1) setup symbolic links to files or
# directories that are under version control,  2) clone GitHub.com repositories
# that are needed to personalize programs such as vim or bash, 3) Install
# programs.
#
# A YAML file is used to define SymLinks, Repositories, and Commands to run.
# Orchestration orders the activities 1) deleting symbolic links, cloning
# repositories, making symbolic links, and installing programs.
#
# -------------------------------------------------------------------------- }}}
# {{{ Required items.

require 'emend/initialize'

# -------------------------------------------------------------------------- }}}
# {{{ Main logic.

module Emend
  # Command Line Interface.
  class CLI
    def execute(args)
      options = CommandLineOptions.parse args
      workflow = Workflow.new(options)
      workflow.orchestrate
    end
  end
end

# -------------------------------------------------------------------------- }}}
