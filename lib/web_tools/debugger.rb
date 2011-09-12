require 'web_tools'
require 'maglev/debugger'

class WebTools::Debugger < WebTools::Tool
  before do
    @entry = ObjectLog.to_ary.detect {|o| o.object_id == params["oop"].to_i }
    @process = (@entry.continuation unless @entry.nil?)
  end

  get '/' do
    return {} unless @process
    json("label" => @entry.label,
         "stack" => @process.report)
  end

  get '/frame' do
    return {} unless @process
    frame = @process.stack[params["frame"].to_i - 1]
    return {} if frame.nil?
    json("method" => method_data_from_frame(frame),
         "variables" => variables_data_from_frame(frame))
  end

  get '/step' do
    json({"foo" => "bar"})
  end

  def method_data_from_frame(frame)
    if frame.method.in_class.namespace
      dict = frame.method.in_class.namespace.my_class.name
    end
    { "source" => frame.method.source,
      "stepPoints" => frame.method.step_offsets,
      "sends" => frame.method.send_offsets,
      "nowAt" => frame.step_offset,
      "dictionaryName" => dict || "",
      "className" => frame.method.in_class.name,
      "isMeta" => frame.method.in_class.singleton_class? }
  end

  def variables_data_from_frame(frame)
    list = [{ "name" => "self",
              "string" => frame.self.inspect,
              "oop" => frame.self.object_id }]
    if frame.self != frame.receiver
      list << { "name" => "receiver",
        "string" => frame.receiver.inspect,
        "oop" => frame.receiver.object_id }
    end
    frame.args_and_temps.each do |k, v|
      list << { "name" => k.to_s,
        "string" => v.inspect,
        "oop" => v.object_id }
    end
    list
  end
end
