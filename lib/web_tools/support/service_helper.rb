require 'json/pure' unless defined? JSON
require 'rack/contrib/jsonp'

module WebTools::Support::ServiceHelper
  def self.included(base)
    base.set :show_exceptions, false
    base.set :raise_errors, false
    base.set :method_override, true
    base.set :logging, true
    base.use Rack::JSONP

    base.error do
      excep = request.env['sinatra.error']
      puts
      p excep.message
      puts excep.backtrace
      json('_stack' => excep.backtrace.join("<br>"),
           '_error' => excep.message)
    end

    base.before do
      @ts = Time.now
    end

    base.helpers do
      def non_meta_name(str)
        if str =~ /^#<Class:.*>$/
          str["#<Class:".length..-2]
        else
          str
        end
      end

      def json(obj)
        content_type :json
        obj.to_hash.tap do |o|
          o["_time"] = ((Time.now - @ts) * 1000).to_i
        end.to_json
      end

      def reflect(obj)
        system.reflect(obj)
      end

      def system
        @reflection ||= MirrorAPI::Reflection.new
      end
    end
  end
end
