require 'web_tools'

class WebTools::Debugger < WebTools::Tool
  before do
    @entry = system.object_by_id(params["oop"].to_i).reflectee
    @process = reflect(@entry.thread) if @entry
  end

  get '/' do
    return {} unless @entry
    json("label" => @entry.exception.message,
         "stack" => str_report_for(@process))
  end

  get '/frame' do
    return {} unless @process
    frame = @process.stack[params["frame"].to_i - 1]
    return {} if frame.nil?
    json("method" => method_data_from_frame(frame),
         "variables" => variables_data_from_frame(frame))
  end

  post '/step' do
    return {} unless @process
    steplevel = params["level"].to_i
    stacksize = @process.stack.size

    if params["level"]
      @process.step(steplevel)
    else
      @process.run
      sleep 0.2 until @process.reflectee.stop?
      unless @process.alive? # No exception, return result
        # TODO
        nil.pause
      end
    end

    new_stack = @process.stack
    frame = new_stack.first

    data = { "label" => @process.name,
      "method" => method_data_from_frame(frame),
      "variables" => variables_data_from_frame(frame) }
    if new_stack.size != stacksize # Stack changed, reload
      data["stack"] = str_report_for @process
    end

    json(data)
  end

  post '/restart' do
    return {} unless @process
    frame = @process.stack[params["frame"].to_i - 1]
    return {} if frame.nil?
    frame.restart
    new_frame = @process.stack.first

    json("label" => @process.name,
         "stack" => str_report_for(@process),
         "method" => method_data_from_frame(new_frame),
         "variables" => variables_data_from_frame(new_frame))
  end

  def method_data_from_frame(frame)
    dict = frame.method.defining_class.nesting[1]
    dict = dict ? dict.name : ""
    { "source" => frame.method.source,
      "stepPoints" => frame.method.step_offsets,
      "sends" => frame.method.send_offsets,
      "nowAt" => frame.step_offset,
      "dictionaryName" => dict || "",
      "className" => frame.method.defining_class.name,
      "isMeta" => frame.method.defining_class.singleton_class? }
  rescue Exception
    return { "source" => "", "stepPoints" => "", "sends" => "", "nowAt" => 0,
      "dictionaryName" => "", "className" => "", "isMeta" => "" }
  end

  def variables_data_from_frame(frame)
    list = [{ "name" => "self",
              "string" => frame.self.name,
              "oop" => frame.self.reflectee.object_id }]
    if frame.self != frame.receiver
      list << { "name" => "receiver",
        "string" => frame.receiver.name,
        "oop" => frame.receiver.reflectee.object_id }
    end
    frame.arguments.each do |k, v|
      list << { "name" => k.to_s,
        "string" => v.name,
        "oop" => v.reflectee.object_id }
    end
    frame.locals.each do |k, v|
      list << { "name" => k.to_s,
        "string" => v.name,
        "oop" => v.reflectee.object_id }
    end
    list
  rescue Exception
    return []
  end

  def str_report_for(process_mirror)
    process_mirror.stack.collect do |f|
      str = ""
      if f.method
        separator = f.method.defining_class.singleton_class? ? '.' : '#'
        str = "#{f.method.defining_class.name}#{separator}"
      end
      str += (f.name || "block in #{f.selector}")
    end
  end
end
