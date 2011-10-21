require 'web_tools'
require 'sinatra/base'
require 'web_tools/support/service_helper'
require 'web_tools/support/error_log'

module WebTools
  class Tool < Sinatra::Base
    include WebTools::Support::ServiceHelper

    before do
      params.each do |k,v|
        params[k] = Rack::Utils.unescape(v)
        params[k] = false if v == "false"
        params[k] = nil if v == "null"
        params.delete(k) if v == "null" || v.empty?
      end
    end

    class << self
      def inherited(subclass)
        super
        self.subclasses << subclass
      end

      def subclasses
        @subclasses ||= []
      end

      def display_name
        name.split("::").last
      end

      def file_name
        display_name + ".html"
      end

      alias description name
    end
  end
end
