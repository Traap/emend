require 'rspec'
require 'emend'
# ------------------------------------------------------------------------------
# version options
# [--version]
# ------------------------------------------------------------------------------
describe 'Emend Version' do

  before(:all) do
    @version = '1.2.19'
  end

  describe '--version' do
    it "was not used. However the version number must match #{Emend::VERSION}" do
      expect(Emend::VERSION).to eql(@version)
    end
  end

  describe '--version' do
    it 'has been used from the command line.' do
      ARGV.replace ['--version']
      options = Emend::CommandLineOptions.parse(ARGV)
      expect(options.verbose).to be(false) 
    end
  end

  describe 'Version' do 
    it 'has a version number' do
      expect(Emend::VERSION).not_to be nil
    end

    it "version number must match #{Emend::VERSION}" do
      expect(Emend::VERSION).to eql(@version)
    end
  end


end
