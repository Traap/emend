#!/usr/local/bin/ruby
# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause

require 'optparse'
require 'ostruct'
require 'pp'

# ------------------------------------------------------------------------------
class CommandLineOptions
  Version = '1.0.0'

  attr_accessor :verbose, :dryrun, :filename
  attr_reader :parser, :options

# ------------------------------------------------------------------------------
  def initialize
    self.verbose = false
    self.dryrun = true 
    self.filename = []
  end

# ------------------------------------------------------------------------------
  def self.parse args
    @options = CommandLineOptions.new
    option_parser.parse! args
    @options
  end

# ------------------------------------------------------------------------------
  def self.option_parser
    @parser ||= OptionParser.new do |parser|
      parser.banner = "Usage: example.rb [options]"
      parser.separator ""
      parser.separator "Specific options:"

      help_option parser
      dryrun_option parser
      file_list_option parser
      verbose_option parser
      version_option parser
    end
  end

# ------------------------------------------------------------------------------
  def self.help_option parser
    parser.on_tail("-h", "--help", "Show this message") do
      puts parser
      exit
    end
  end

# ------------------------------------------------------------------------------
  def self.dryrun_option parser
    parser.on("-n", "--nodryrun", "No Dryrun") do |d|
      @options.dryrun ^= d
    end
  end

# ------------------------------------------------------------------------------
  def self.verbose_option parser
    parser.on("-v", "--verbose", "Verbose") do |v|
      @options.verbose = v
    end
  end

# ------------------------------------------------------------------------------
  def self.file_list_option parser
    parser.on("-f", "--file x,y,x MADATORY", Array, "Filename") do |f|
      @options.filename = f
    end
  end

# ------------------------------------------------------------------------------
  def self.version_option parser
    parser.on_tail("--version", "Show version") do
      puts Version
      exit
    end
  end

# ------------------------------------------------------------------------------
end # class CommandLineOptions