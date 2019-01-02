require 'rspec'
require 'emend'
# ------------------------------------------------------------------------------
# nodryrun options
# [-v | --verbose]
# ------------------------------------------------------------------------------
describe 'Emend NoDryRun' do

  describe 'no -n' do
    it 'has not been used.' do
      options = Emend::Options.new
      expect(options.dryrun).to be(true)
    end
  end

  describe '-n' do
    it 'has been used from the command line.' do
      ARGV.replace ['-n']
      options = Emend::CommandLineOptions.parse(ARGV)
      expect(options.dryrun).to be(false)
    end
  end

  describe '--nodryrun' do
    it 'has been used from the command line.' do
      ARGV.replace ['--nodryrun']
      options = Emend::CommandLineOptions.parse(ARGV)
      expect(options.dryrun).to be(false)
    end
  end

end
