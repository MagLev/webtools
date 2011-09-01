require 'sinatra/base'
require 'json/pure'

require 'web_tools'
require 'web_tools/support/code_browser'
require 'web_tools/support/service_helper'

class WebTools::Browser < Sinatra::Base
  include WebTools::Support::ServiceHelper

  before do
    @ts = Time.now
    @stack = nil
    @browser = WebTools::CodeBrowser.new
    content_type :json
  end

  get '/modulelist' do
    prepare_data(WebTools::CodeBrowser.class_and_module_list)
  end

  get '/module/:name' do
    prepare_data(@browser.select_module(params[:name]))
  end

  get '/module/:module_name/constant/:const_name' do
    prepare_data(@browser.select_constant(params[:module_name],
                                          params[:const_name]))
  end

  get '/module/:module_name/method' do
    flag = params[:is_instance_method] == 'true' ? true : false
    prepare_data(@browser.select_method(params[:module_name],
                                        params[:method_name],
                                        flag))
  end

  get '/objectspace/:object_id' do
    prepare_data(@browser.object_info(params[:object_id]))
  end

  get '/transaction/abort' do
    Maglev.abort_transaction
    prepare_data(WebTools::CodeBrowser.class_and_module_list)
  end
end

