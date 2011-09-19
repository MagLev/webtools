require 'sinatra/base'
require 'web_tools'
require 'maglev/reflection'
require 'maglev/debugger'

module WebTools
  class Workspace < Tool

    def self.description
      'Code workspace'
    end

    post '/deleteProcess' do
      return {} unless params["oop"]
      ObjectLog.delete(ObjectSpace._id2ref(params["oop"].to_i))
      json({})
    end

    post '/evaluate' do
      begin
        eval_result = Maglev::Debugger.debug do
          begin
            value = eval(params["text"])
            result = { "klass" => value.class.inspect,
              "string" => value.inspect }
            if value.is_a? Module
              result["dict"] = value.namespace.my_class.to_s
              result["name"] = value.inspect
              result["cat"]  = ""
            end
            result
          rescue SyntaxError => e
            { "errorType" => "compileError",
              "errorDetails" => [[1031, 1, e.message, nil, nil]] }
          end
        end
        return json(eval_result)
      rescue Exception => ex
        entry = ::ObjectLog.to_ary.reverse.detect {|e| e.label == ex.message }
        return json("errorType" => ex.class.inspect,
                    "description" => ex.message,
                    "oop" => entry.object_id)
      end
    end

    post '/saveMethod' do
      klass = Object.find_in_namespace(params["klass"])
      source = params["source"]
      if params["isMeta"] == "true"
        unless source =~ /^\s*def\s+self\./
          # Compiling sth for the singleton_class without a self., so we
          # compile it on the singleton_class object
          klass = klass.singleton_class
        end
      end
      begin
        m = klass.compile_method(source, params["selector"])
        return json({"selector" => m.name, "warnings" => nil})
      rescue SyntaxError => e
        # Magic values taken from a Smalltalk CompileError
        return json("compileError" => [[1031, 1, e.message, nil, nil]])
      end
    end
  end
end
