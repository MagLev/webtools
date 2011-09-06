require 'sinatra'
require 'web_tools'
require 'web_tools/support/service_helper'
require 'rack/contrib/jsonp'
require 'maglev/debugger'
require 'maglev/method_source'

module WebTools
  class Debugger < Sinatra::Base
    include WebTools::Support::ServiceHelper
    use Rack::JSONP

    def respond_json(obj)
      content_type :json
      result = case obj
               when Maglev::Debugger::Wrapper
                 obj.to_hash
               else
                 if obj.is_a? Array and
                     obj.all? {|e| e.is_a? Maglev::Debugger::Wrapper}
                   obj.collect(&:to_hash)
                 else
                   case obj
                   when Array, Hash
                     obj
                   else
                     details_for(obj)
                   end
                 end
               end
      body(result.to_json)
    end

    def process
      Maglev::Debugger::Process.new(ObjectSpace._id2ref(params[:oop].to_i))
    end

    def frames
      params.has_key?("all") ? process.frames : process.ruby_frames
    end

    def frame
      f = frames[params[:idx].to_i]
      f.debug_info!
      f
    end

    def objects
      instance_variables_for(object)
    end

    def object
      ctxt = frame.debug_info[:context]
      obj = nil
      variable_list = params[:splat].first.split("/objects/")
      variable_list.each do |name|
        name = name[0...(-"/objects".size)] if name.end_with? "/objects"
        obj = ctxt[name.to_sym]
        ctxt = instance_variables_for(obj)
      end
      obj
    end

    def instance_variables_for(object)
      object.instance_variables.inject({}) do |hash, var|
        hash[var.to_sym] = object.instance_variable_get(var)
        hash
      end
    end

    def details_for(object)
      { "instance_variables" => instance_variables_for(object),
        "self" => object,
        "inspect" => object.inspect,
        "class" => object.class,
        "do-it" => nil,
        "do-it-result" => nil }
    end

    before do
      Maglev.abort_transaction
    end

    # => list of errors
    get "/process" do
      errors = ObjectLog.errors.collect {|e| Maglev::Debugger::Process.new(e) }
      processes = Thread.list.select(&:stop?).collect do |e|
        Maglev::Debugger::Process.new(e)
      end
      respond_json (errors + processes)
    end

    get "/process/:oop" do
      respond_json process
    end

    get "/process/:oop/frames" do
      respond_json frames
    end

    get "/process/:oop/frames/:idx" do
      respond_json frame
    end

    get "/process/:oop/frames/:idx/objects" do
      respond_json frame.debug_info[:context]
    end

    get "/process/:oop/frames/:idx/objects/*" do
      if params[:splat].first.end_with? "objects"
        respond_json objects
      else
        respond_json details_for(object)
      end
    end

    delete "/process/:oop" do
      respond_json process.delete
    end

    delete "/process/:oop/frames/:idx" do
      respond_json frame.delete
    end

    post "/process/:oop" do
      p = process
      return 404 if p.is_a? Maglev::Debugger::ObjectLogError
      if params["running"] == "true" and p.thread.alive? and p.thread.stop?
        p.thread.wakeup
        p.thread.join
        if (result = p.thread[:result]).is_a? Maglev::Debugger::Process
          p.thread.kill
          raise result.exception
        else
          result
        end
      else
        404
      end
    end

    put "/process/:oop/frames/:idx" do
      current_frame = frame.to_hash
      # TODO: Really check the posted document
      if doIt = params["do-it"]
        begin
          result = frame.context_eval(doIt)
        rescue Exception => e
          result = e
        end
        current_frame[:"do-it"] = doIt
        current_frame[:"do-it-result"] = details_for(result)
        respond_json current_frame
      elsif params["index"] == "1" and params[:idx] == "0"
        until frame.method_name != current_frame[:method_name]
          frame.step(:into) # Step into until we find the next ruby frame
        end
        respond_json frame
      elsif di = params["debug_info"]
        if di["stepOffset"] && di["stepOffset"] != current_frame[:debug_info][:stepOffset]
          return 404 unless params[:idx] == "0"
          while di["stepOffset"].to_i > frame[:debug_info][:stepOffset].to_i
            frame.step(:over)
          end
          respond_json frame
        elsif di["source"] && di["source"] != current_frame[:debug_info][:source]
          klass = current_frame[:defining_class]
          frame_above = frames[params[:idx].to_i + 1]
          # Pop to one frame before the modified one
          frame_above.delete
          # Recompile the method
          klass.set_method_source(current_frame[:method_name], di["source"])
          # Step into the modified method
          process.frames.first.step(:into)
          params[:idx] = 0
          respond_json frame # Retreive the frame again, to show the updated source
        end
      else
        status 404
        respond_json frame
      end
    end

    put "/process/:oop/frames/:idx/objects/*" do
      return 404 if params[:splat].first.end_with? "objects"

      if doIt = params["do-it"]
        begin
          result = object.instance_eval(doIt)
        rescue Exception => e
          result = e
        end
        return_value = details_for(object)
        return_value["do-it"] = doIt
        return_value["do-it-result"] = details_for(result)
        respond_json return_value
      else
        404
      end
    end

    # XXX: Remove me, please
    options '*' do
      [200,
       { 'Access-Control-Allow-Origin' => '*',
         'Access-Control-Allow-Methods' => 'PUT, POST, GET, DELETE, OPTIONS',
         'Access-Control-Max-Age' => '1000',
         'Access-Control-Allow-Headers' => '*' },
       [""]]
    end
  end
end
