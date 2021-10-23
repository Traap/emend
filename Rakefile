# frozen_string_literal: true

require 'bundler/gem_tasks'
require 'rspec/core/rake_task'

# ------------------------------------------------------------------------------
# Run rspecs
# ------------------------------------------------------------------------------

begin
  RSpec::Core::RakeTask.new(:spec)
  task default: :spec
rescue StandardError
  puts 'RSpec is not supported on this system.'
end

# ------------------------------------------------------------------------------
# Build Ember.
# ------------------------------------------------------------------------------

namespace :build do
  task :emend do
    system 'bundle install'
    system 'bundle exec rake'
    system 'bundle exec rake install'
  end
end

# ------------------------------------------------------------------------------
