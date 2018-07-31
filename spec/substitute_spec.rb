require 'rspec'
require_relative '../lib/emend/substitute'
require_relative '../lib/emend/version'
# ------------------------------------------------------------------------------
describe 'emend' do
  it 'the linux home (~) has been expanded.' do
    tst = Emend::Substitute.expand_path('~')
    val = Dir.home()
    expect(tst).to eq(val)
  end

  it 'A fully qualified file name has been expanded.' do
    tst = Emend::Substitute.expand_path('~/foo/bar/baz')
    val = Dir.home() + '/foo/bar/baz'
    expect(tst).to eq(val)
  end

  it 'A reminder to update the version number.' do
    expect(Emend::VERSION).to eq('1.1.11')
  end
end
