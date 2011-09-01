module WebTools
  module Support; end
  module Middleware; end

  path = File.expand_path("..", __FILE__)
  autoload :Browser, File.join(path, "browser.rb")
  autoload :Debugger, File.join(path, "debugger.rb")
end
