# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause
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
# ------------------------------------------------------------------------------

require 'emend/initialize'

# ------------------------------------------------------------------------------
module Emend
  class CLI
    def execute(args)
      options = CommandLineOptions.parse args
      if options
        workflow = Workflow.new(options)
        workflow.orchestrate
      end
    end
  end
end
# ------------------------------------------------------------------------------
