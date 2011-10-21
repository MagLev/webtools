require 'sinatra/base'
require 'web_tools'

module WebTools
  class Workspace < Tool
    def self.description
      'Code workspace'
    end

    post '/deleteProcess' do
      return {} unless params["oop"]
      Support::ErrorLog.delete(system.object_by_id(params["oop"].to_i))
      json({})
    end

    post '/evaluate' do
      response = nil
      client = Thread.start do
        begin
          value = eval(params["text"])
          result = { "klass" => value.class.inspect,
            "string" => value.inspect }
          if value.is_a? Module
            result["dict"] = ""
            result["name"] = value.inspect
            result["cat"]  = ""
          end
          response = json(result)
        rescue SyntaxError => e
          response = json("errorType" => "compileError",
                          "errorDetails" => [[1031, 1, e.message, nil, nil]])
        rescue Exception => e
          entry = Support::ErrorLog.add :thread => Thread.current,
                                        :exception => e
          response = json("errorType" => entry.exception.class.inspect,
                          "description" => entry.exception.message,
                          "oop" => entry.object_id)
          Thread.stop
        end
      end

      sleep 0.2 until client.stop?
      response
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
