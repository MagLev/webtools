require 'sinatra/base'
require 'web_tools'
require 'web_tools/support/service_helper'

class WebTools::UI < Sinatra::Base
  PUBLIC_FOLDER = File.expand_path("../../../public", __FILE__)
  include WebTools::Support::ServiceHelper

  helpers do
    def static(file) File.read("#{PUBLIC_FOLDER}/#{file}.html") end
  end

  get '/' do
    static :index
  end

  get '/tools' do
    json "tools" => (WebTools::Tool.subclasses.reject(&:dont_show?).sort_by(&:name).map do |cls|
      { "file" => cls.file_name,
        "name" => cls.display_name,
        "description" => cls.description }
    end)
  end

  get '/*' do
    "#{params[:splat]} not implemented"
  end
end
