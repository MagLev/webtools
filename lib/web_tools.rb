module WebTools
  module Support; end
  module Middleware; end

  path = File.expand_path("../web_tools", __FILE__)
  require File.join(path, "tool.rb")
  require File.join(path, "code_browser.rb")
  require File.join(path, "debugger.rb")
  require File.join(path, "method_list.rb")
  require File.join(path, "workspace.rb")
  require File.join(path, "ui.rb")

  if defined? Maglev
    require File.join(path, "shared_page_cache.rb")
    require File.join(path, "statistics.rb")
    require File.join(path, "version_report.rb")
    require File.join(path, "object_log.rb")
    require File.join(path, "session_list.rb")
  end
end
