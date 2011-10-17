require 'sinatra/base'
require 'web_tools'

module WebTools
  class Workspace < Tool

    def self.description
      'Code workspace'
    end

    # post '/deleteProcess' do
    #   return {} unless params["oop"]
    #   ObjectLog.delete(ObjectSpace._id2ref(params["oop"].to_i))
    #   json({})
    # end

    post '/evaluate' do
      eval_thread = Thread.new do
        begin
          value = eval(params["text"])
          result = { "klass" => value.class.inspect,
            "string" => value.inspect }
          if value.is_a? Module
            result["dict"] = ""
            result["name"] = value.inspect
            result["cat"]  = ""
          end
          return result
        rescue SyntaxError => e
          return { "errorType" => "compileError",
            "errorDetails" => [[1031, 1, e.message, nil, nil]] }
        rescue Exception => e
          Thread.current[:error] = e
          Thread.current[:copy] = reflect(Thread.current).copy_active_thread
          return Thread.current
        end
      end
      eval_thread.join
      m = reflect(eval_thread)
      if (rval = m.return_value) == eval_thread
        json("errorType" => eval_thread[:error].class.inspect,
             "description" => eval_thread[:error].message,
             "oop" => eval_thread[:copy].object_id)
      else
        json(rval)
      end
    end

    post '/saveMethod' do
      klass = reflect(Object).constant(params["klass"])
      source = params["source"]
      if params["isMeta"] == "true"
        unless source =~ /^\s*def\s+self\./
          # Compiling sth for the singleton_class without a self., so we
          # compile it on the singleton_class object
          klass = klass.singleton_class
        end
      end
      begin
        klass.method(params["selector"]).source = source
        return json({"selector" => m.name, "warnings" => nil})
      rescue SyntaxError => e
        # Magic values taken from a Smalltalk CompileError
        return json("compileError" => [[1031, 1, e.message, nil, nil]])
      end
    end
  end
end
