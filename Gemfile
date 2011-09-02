# This Gemfile manages gems for the Sinatra examples in this directory.  It
# does not manage gems for Sinatra applications in subdirectories.

source "http://rubygems.org"

group :core do
  gem 'json', :git => "https://github.com/MagLev/json.git"
  gem 'rack-contrib'
end

group :service do
  gem 'sinatra', '~> 1.2.0'
end

group :demo do
  gem 'activemodel', '~> 3.0'
end

group :test do
  gem 'rack-contrib'
  gem 'mocha'
  gem 'minitest'
end
