require 'web_tools'
require 'sinatra/base'
require 'web_tools/support/service_helper'

module WebTools
  class Tool < Sinatra::Base
    include WebTools::Support::ServiceHelper

    before do
      params.each do |k,v|
        params.delete(k) if v == "null" || v.empty?
        params[k] = false if v == "false"
        params[k] = nil if v == "null"
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
