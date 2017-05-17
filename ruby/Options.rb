#!/usr/local/bin/ruby
# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause

require 'optparse'

options = {}

OptionParser.new do |opts|
  opts.banner = "Usage: example.rb [options]"

  opts.on("-v", "--verbose", "Verbose") do |v|
   options[:verbose] = v
  end

  opts.on("-d", "--dryrun", "Dryrun") do |d|
   options[:dryrun] = d
  end

  opts.on("-f", "--files", "Filename[s]") do |f|
   options[:filenames] = f
  end

  opts.on("-h", "--help", "Prints this help") do
    puts opts
  end 
    
end.parse!

p options
p ARGV
