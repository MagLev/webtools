require 'web_tools'

app = Rack::Builder.new do
  WebTools::UI.set :root, File.dirname(__FILE__)
  ui = WebTools::UI.new
  map('/webtools/') { run ui }
end
run app
