require 'optparse'
require 'ostruct'
require 'pp'

Options = Struct.new(:filename, :dryrun, :verbose)

class Parser
  def self.parse(options)
    args = Options.new(options)

    opt_parser = OptionParser.new do |opts|
      opts.banner = "Usage: example.rb [options]"

      opts.on("-v", "--verbose", "Verbose") do |v|
        args.verbose = v
      end

      opts.on("-d", "--dryrun", "Dryrun") do |d|
        args.dryrun = d
      end

      opts.on("-f", "--files", "Filename[s]") do |f|
        args.filenames = f
      end

      opts.on("-h", "--help", "Prints this help") do
        puts opts
        exit
      end
    end

    opt_parser.parse!(options)
    return args
  end
end


 parser = Parser.new
 options  = parser.parse(ARGV)
 pp options
 pp ARGV
