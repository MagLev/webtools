require 'sinatra/base'
require 'json/pure'
require 'web_tools'
require 'web_tools/support/app_model'
require 'web_tools/support/service_helper'

class WebTools::Info < Sinatra::Base
  include WebTools::Support::ServiceHelper

  before do
    @ts = Time.now
    @stack = nil
    @model = WebTools::AppModel.new
  end

  get '/version' do
    content_type :json
    prepare_data @model.version_report
  end

  get '/sessions' do
    content_type :json
    prepare_data @model.session_report
  end

  get '/tools' do
    content_type :json
    prepare_data(@model.tools)
  end
end
