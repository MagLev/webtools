module WebTools::Support::ServiceHelper
  def self.included(base)
    base.set :show_exceptions, true
    base.set :raise_errors, false

    base.error do
      excep = request.env['sinatra.error']
      { '_stack' => excep.backtrace.join("<br>") }.to_json
    end
  end

  # Returns a JSON string that contains the data under the "data" key.
  # Adds other keys (_time, _stack) if appropriate.
  def prepare_data(data)
    raise "Expecting Hash" unless Hash === data
    data['_time'] = ((Time.now - @ts) * 1_000).to_i
    data['_stack'] = @stack
    data.to_json
  end
end
