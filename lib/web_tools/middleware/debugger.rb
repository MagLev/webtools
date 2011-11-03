require 'web_tools'
require 'maglev/debugger'

module WebTools::Middleware
  class Debugger
    attr_reader :production_mode

    def initialize(app, *args)
      @app = app
      self.production_mode = true if args.include?(:production)
      self.production_mode ||= false if args.include?(:development)
      self.production_mode ||= (ENV["RACK_ENV"] == :production)
    end

    def production_mode=(bool)
      @production_mode = bool
      if bool
        @debugger = self
      else
        @debugger = InProcessDebugger.new(@app)
      end
    end

    def call(env)
      dup._call(env)
    end

    def _call(env)
      @debugger.wrap_call(env) do
        Maglev::Debugger.debug { @app.call(env) }
      end
    end

    def wrap_call(env)
      yield
    end

    class InProcessDebugger
      def initialize(app)
        @app = app
        @debugger_app = WebTools::Debugger.new
        load_template
      end

      def load_template
        file = File.expand_path(__FILE__)
        _, @template = ::IO.read(file).gsub("\r\n", "\n").split(/^__END__$/, 2)
        ERB.new(@template).def_method(self.class, 'render_template(title, path)')
      end

      def debugger_active?
        defined?(@@debugger_active) && !!@@debugger_active &&
          defined?(@@debugged_process) && @@debugged_process.alive?
      end

      def wrap_call(env)
        if debugger_active?
          if env["PATH_INFO"] == "/"
            [500, {"Content-Type" => "text/html"}, info_message(env)]
          else
            response = @debugger_app.call(env)
            response[1]['Access-Control-Allow-Origin'] = "*"
            response
          end
        else
          application_call(env, yield)
        end
      end

      def application_call(env, result)
        if result.is_a? Maglev::Debugger::Process
          raise result.exception if result[:skip_debugger]
          result[:skip_debugger] = false
          @@debugged_path = env["PATH_INFO"]
          @@debugged_process = result.thread
          @@debugger_active = true
          @@debugged_exception = result.exception.message
          [500, {"Content-Type" => "text/html"}, info_message(env)]
        else
          result
        end
      end

      def info_message(env)
        [render_template(@@debugged_exception,
                         env['rack.url_scheme'].to_s + "://" +
                         env['HTTP_HOST'].to_s +
                         env['SCRIPT_NAME'].to_s + "/process/" +
                         @@debugged_process.object_id.to_s)]
      end
    end
  end
end

__END__
<!doctype html system>
<html>
<head>
  <title>Maglev Debugger</title>
</head>
<body>
  <h4><%= title %></h4>
  There was an error, and execution has been suspended.
  You can interact with this debugging service to inspect the problem.
  <br>
  The entry point is at:
  <br>&nbsp;&nbsp;&nbsp;&nbsp;
  <a href="<%= path %>"><%= path %></a>
  <br>
  Once you're done debugging, you can click
  <form style="display:inline" name="resumeForm" method="POST" action="<%= path %>">
    <input type="hidden" name="running" VALUE="true">
    <a href="#" onClick="document.resumeForm.submit(); return false">here</a>
  </form>
  to resume the original process.
</body>
</html>
