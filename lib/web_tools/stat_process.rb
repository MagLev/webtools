require 'sinatra/base'
require 'web_tools'
require 'web_tools/support/app_model'
require 'web_tools/support/service_helper'
require 'maglev/objectlog'

class WebTools::ObjectLog < WebTools::Tool
  get '/' do
    list = ObjectLog.to_a.reverse.collect do |entry|
      label = "#{entry.label}"
      object_str = "#{entry.object}"
      label = label.split.first if label == object_str
      { "oop" => entry.object_id,
        "stamp" => entry.timestamp.to_s,
        "pid" => entry.pid.to_s,
        "label" => label,
        "type" => %w[Fatal Error Warn Info Debug Trace Transcript][entry.priority],
        "tag" => "#{entry.tag if entry.tagged?}",
        "object" => object_str,
        "hasContinuation" => entry.has_cc? }
    end
    json("list" => list)
  end
end
