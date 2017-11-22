require "test_helper"

class BootstrapTest < Test::Unit::TestCase 
  def test_that_it_has_a_version_number
    refute_nil ::Bootstrap::VERSION
  end

end
