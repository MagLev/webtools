require 'sinatra/base'
require 'web_tools'

class WebTools::UI < Sinatra::Base
  before do
    @location = env["SCRIPT_NAME"]
    @javascripts = %w[
                     https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js
                     https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.16/jquery-ui.min.js
                   ]
    @stylesheets = %w[reset.css webtools.css]
  end

  helpers do
    def static(file) File.read("#{settings.public}/#{file}.html") end
  end

  get '/' do
    static :index
  end

  get '/tools' do
    Tool.subclasses
  end

  get '/browser' do
    @javascripts += %w[webtools/browser.js CodeMirror/js/codemirror.js]
    erb :browser
  end

  get '/debugger' do
    @stylesheets += %w[
      http://alexgorbatchev.com/pub/sh/current/styles/shCore.css
      http://alexgorbatchev.com/pub/sh/current/styles/shThemeDefault.css
      stylesheets/webtools-debugger.css]
    @javascripts += %w[
      https://raw.github.com/alexgorbatchev/SyntaxHighlighter/master/scripts/XRegExp.js
      https://raw.github.com/alexgorbatchev/SyntaxHighlighter/master/scripts/shCore.js
      http://alexgorbatchev.com/pub/sh/current/scripts/shBrushRuby.js
      webtools/debugger.js]
    erb :debugger
  end

  get '/info/version' do
    @javascripts += %w[webtools/version.js]
    @stylesheets = []
    erb :version
  end

  get '/info/sessions' do
    @javascripts += %w[webtools/sessions.js]
    @stylesheets = []
    erb :sessions
  end

  private

  def erb(*args, &block)
    @stylesheets = @stylesheets.map do |path|
      unless path =~ /^((http)|\/)/
        "#{@location}/stylesheets/#{path}"
      else
        path
      end
    end

    @javascripts = @javascripts.map do |path|
      unless path =~ /^((http)|\/)/
        "#{@location}/javascript/#{path}"
      else
        path
      end
    end
    super
  end
end
