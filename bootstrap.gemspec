Gem::Specification.new do |s|
  s.name        = 'bootstrap'
  s.version     = '1.1.4'
  s.date        = '2017-08-27'
  s.summary     = "Bootstrap a development environment."
  s.description = "Bootstrap process YAML files to configure your computer."
  s.authors     = ["Gary A. Howard"]
  s.email       = 'gary.a.howard@mac.com'
  s.files       = ["lib/bootstrap.rb", 
                   "lib/bootstrap/DataTypes.rb",
                   "lib/bootstrap/Options.rb",
                   "lib/bootstrap/Workflow.rb"]
  s.homepage    = 'https://github.com/Traap/bootstrap'
  s.license     = 'BSD-3-Clause'
  s.executables << 'bootstrap'
end
