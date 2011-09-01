require 'sinatra/base'
require 'json/pure'

require 'web_tools'
require 'web_tools/support/smalltalk_extensions'
require 'web_tools/support/app_model'

class WebTools::UI < Sinatra::Base
  use WebTools::Browser
  use WebTools::Debugger
  use WebTools::Info

  before do
    @location = env["SCRIPT_NAME"]
  end

  get '/browser' do
    @scripts = %w[webtools/browser CodeMirror/js/codemirror]
    erb :browser
  end

  get '/version' do
    @scripts = %w[webtools/version]
    erb :version
  end

  get '/sessions' do
    @scripts = %w[webtools/sessions]
    erb :sessions
  end
end
