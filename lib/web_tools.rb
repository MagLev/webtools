module WebTools
  module Support; end
  module Middleware; end
end

path = File.expand_path("../web_tools", __FILE__)
autoload :"WebTools::Browser", File.join(path, "browser.rb")
autoload :"WebTools::Debugger", File.join(path, "debugger.rb")
