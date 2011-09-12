module WebTools
  module Support; end
  module Middleware; end

  path = File.expand_path("../web_tools", __FILE__)
  autoload :Tool, File.join(path, "tool.rb")
  autoload :CodeBrowser, File.join(path, "code_browser.rb")
  autoload :Debugger, File.join(path, "debugger.rb")
  autoload :VersionReport, File.join(path, "version_report.rb")
  autoload :ObjectLog, File.join(path, "object_log.rb")
  autoload :MethodList, File.join(path, "method_list.rb")
  autoload :SessionList, File.join(path, "session_list.rb")
  autoload :Workspace, File.join(path, "workspace.rb")
  autoload :UI, File.join(path, "ui.rb")
end
