Gem::Specification.new do |s|
  s.name         = 'maglev-webtools'
  s.version      = '0.2'
  s.date         = Date.today.to_s
  s.summary      = "The MagLev WebTools suite"
  s.description  = "A suite of tools for inspecting, interfacing and working with MagLev. It consists of a number of Rack middlewares to record runtime information in the stone, an associated set of Sinatra middlewares for exposing said information through a JSON interface, and a number of Sinatra web-applications that take advantage of that interface to build a few simple browser-based development tools."
  s.authors      = ["Peter McLain", "Monty Williams", "Tim Felgentreff"]
  s.email        = "tfelgentreff@vmware.com"
  s.files        = Dir.glob("lib/**/*") + Dir.glob("public/**/*") + Dir.glob("views/**/*") << "LICENSE.txt" << "README.rdoc"
  s.homepage     = "http://github.com/MagLev/webtools"
  s.license      = 'MIT'
  s.require_path = 'lib'
  s.executables  = ["webtools"]

  s.post_install_message = "NOTE: If you want to run the Sinatra applications, you will have to install sinatra >= 1.2.0.

It is not part of the default dependencies, so the gem doesn't have to pull in too many dependencies for deployment installations (i.e. recording information about your production system for later analysis).
"

  s.add_dependency 'json', '~> 1.5'
  s.add_dependency 'rack', '~> 1.1'
  s.add_dependency 'rack-contrib', '~> 1.1'
  s.add_development_dependency 'sinatra', '~> 1.2.0'
  s.add_development_dependency 'activemodel', '~> 3.0'
  s.add_development_dependency 'mocha', '~> 0.9'
  s.add_development_dependency 'minitest', '~> 2.4'
end
