require 'sinatra'
require 'maglev/debugger'
require 'json/pure'
require 'rack/contrib/jsonp'

module WebTools
  class DebuggerApp < Sinatra::Base
    enable :sessions
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
                     instance_variables_for(obj)
                   end
                 end
               end
      body(result.to_json)
    end

    def process
      Maglev::Debugger::Process.new(ObjectSpace._id2ref(params[:oop].to_i))
    end

    def frame
      l = params.has_key?("all") ? process.frames : process.ruby_frames
      l[params[:idx].to_i].tap {|o| o.debug_info! }
    end

    def objects
      ctxt = frame.debug_info[:context]
      variable_list = params[:splat].first.split("/objects/")
      variable_list.each do |name|
        name = name[0...(-"/objects".size)] if name.end_with? "/objects"
        ctxt = instance_variables_for(ctxt[name.to_sym])
      end
      ctxt
    end

    def instance_variables_for(object)
      hash = object.instance_variables.inject({}) do |hash, var|
        hash[var.to_sym] = object.instance_variable_get(var)
        hash
      end
      hash[:"(__self__)"] = object
      hash[:"(__class__)"] = object.class
      hash
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
      if params.has_key?("all")
        respond_json process.frames
      else
        respond_json process.ruby_frames
      end
    end

    get "/process/:oop/frames/:idx" do
      respond_json frame
    end

    get "/process/:oop/frames/:idx/objects" do
      respond_json frame.debug_info[:context]
    end

    get "/process/:oop/frames/:idx/objects/*" do
      if params[:splat]
        respond_json objects
      else
        respond_json objects[:"(__self__)"]
      end
    end

    delete "/process/:oop" do
      respond_json process.delete
    end

    delete "/process/:oop/frames/:idx" do
      respond_json frame.delete
    end

    put "/process/:oop" do
      p = process
      if p.is_a? Maglev::Debugger::ObjectLogError or !p.thread.alive?
        return status 404
      end
      p.thread.wakeup
      p.thread.join
      if (result = p.thread[:result]).is_a? Maglev::Debugger::Process
        p.thread.kill
        raise result.exception
      else
        result
      end
    end

    post "/process/:oop/frames/:idx" do
      respond_json frame.context_eval(request.POST["do-it"])
    end

    post "/process/:oop/frames/:idx/objects/*" do
      respond_json objects[:"(__self__)"].instance_eval(request.POST["do-it"])
    end
  end
end
