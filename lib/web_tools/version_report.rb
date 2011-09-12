require 'sinatra/base'
require 'web_tools'
require 'web_tools/support/service_helper'

class WebTools::VersionReport < WebTools::Tool

  def self.description
    'Information about the Stone and Gem processes and their host(s)'
  end

  get '/' do
    json("stone" => stone_version_report,
         "gem" => gem_version_report)
  end

  def stone_version_report
    results = { }
    rpt = Maglev::System.stone_version_report
    rpt.keys.each { |k| results[k] = rpt.at(k) }
    results
  end

  def gem_version_report
    results = { }
    rpt = Maglev::System.gem_version_report
    rpt.keys.each { |k| results[k] = rpt.at(k) }
    results
  end
end
