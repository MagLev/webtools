require 'sinatra/base'
require 'web_tools'
# require 'web_tools/support/app_model'
# require 'web_tools/support/service_helper'

class WebTools::Statistics < WebTools::Tool
  def self.description
    "Load and view statmonitor files"
  end

  get '/' do
    json("files" => [])
  end
end
