module WebTools
  module Support; end
  module Middleware; end

  class Tool < Sinatra::Base
    include WebTools::Support::ServiceHelper
    use Rack::JSONP

    def self.inherited(subclass)
      super
      self.subclasses << subclass
    end

    def self.subclasses
      @subclasses ||= []
    end
  end

  path = File.expand_path("../web_tools", __FILE__)
  autoload :Browser, File.join(path, "browser.rb")
  autoload :Debugger, File.join(path, "debugger.rb")
  autoload :Info, File.join(path, "info.rb")
  autoload :UI, File.join(path, "ui.rb")
end
