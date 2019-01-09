require 'rspec'
require 'emend'
# ------------------------------------------------------------------------------
# verbose options
# [-v | --verbose]
# ------------------------------------------------------------------------------
describe 'Emend Verbose' do

  describe 'no -v' do
    it 'has not been used.' do
      options = Emend::Options.new
      expect(options.verbose).to be(false)
    end
  end

  describe '-v' do
    it 'has been used from the command line.' do
      ARGV.replace ['-v']
      options = Emend::CommandLineOptions.parse(ARGV)
      expect(options.verbose).to be(true)
    end
  end

  describe '--verbose' do
    it 'has been used from the command line.' do
      ARGV.replace ['--verbose']
      options = Emend::CommandLineOptions.parse(ARGV)
      expect(options.verbose).to be(true)
    end
  end

end
