$LOAD_PATH.unshift File.expand_path('lib', __dir__)
require 'emend/version'

Gem::Specification.new do |s|
  s.name          = 'emend'
  s.version       = Emend::VERSION
  s.summary       = 'emend a development environment.'
  s.description   = 'emend processes YAML files to configure your computer.'
  s.authors       = ['Gary A. Howard']
  s.email         = ['gary.a.howard@mac.com']
  s.homepage      = 'https://github.com/Traap/emend'
  s.license       = 'BSD-3-Clause'

  s.require_paths = ['lib']
  s.files         = Dir['lib/**/*']
  s.test_files    = Dir['text/**/*.rb']

  s.executables   = %w[emend]

  s.required_ruby_version = '>= 3.0'

  s.add_development_dependency 'bundler', '>= 2.1.0'
  s.add_development_dependency 'rake', '>= 12.3.3'
  s.add_development_dependency 'require_all'
  s.add_development_dependency 'rspec', '>= 3.10.0'
end
