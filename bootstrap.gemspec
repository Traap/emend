lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "bootstrap/version"

Gem::Specification.new do |s|
  s.name        = "bootstrap"
  s.version     = BBootstrap::VERSION
  s.date        = "2017-08-27"
  s.authors     = ["Gary A. Howard"]
  s.email       = "gary.a.howard@mac.com"

  s.summary     = "Bootstrap a development environment."
  s.description = "Bootstrap process YAML files to configure your computer."
  s.homepage    = "https://github.com/Traap/bootstrap"
  s.license     = "BSD-3-Clause"


  s.files         = `git ls-files -z`.split("\x0").reject do |f|
    f.match(%r{^(test|spec|features)/})
  end

  # s.files       = ["lib/bootstrap.rb",
  #                  "lib/bootstrap/datatypes.rb",
  #                  "lib/bootstrap/options.rb",
  #                  "lib/bootstrap/version.rb",
  #                  "lib/bootstrap/workflow.rb"]

  s.executables << "bootstrap"
  s.require_paths = ["lib"]

  s.add_development_dependency "bundler", "~> 1.16"
  s.add_development_dependency "rake", "~> 10.0"
  s.add_development_dependency "test-unit", "~> 3.2", ">= 3.2.3"
end

