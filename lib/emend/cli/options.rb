# frozen_string_literal: true

module Emend
  # Options the user has chosen.
  class Options
    attr_accessor :dryrun, :filename, :verbose, :version

    def initialize
      @dryrun = true
      @filename = []
      @verbose = false
      @version = Emend::VERSION
    end
  end
end
