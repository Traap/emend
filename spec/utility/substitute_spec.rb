require 'rspec'
require 'emend'

# ------------------------------------------------------------------------------
describe 'YAML Home Substitutions' do

  before(:all) do
    @options = Emend::Options.new
    @home = Dir.home()
    @file = "/foo/bar/baz"
  end

  it 'can expand(~) to full path.' do
    tst = Emend::Substitute.expand_path('~')
    expect(tst).to eq(@home)
  end
  
  it 'A fully qualified file name has been expanded.' do
    tst = Emend::Substitute.expand_path('~/foo/bar/baz')
    val = @home + '/foo/bar/baz'
    expect(tst).to eq(val)
  end

end
