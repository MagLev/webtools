require 'web_tools'
require 'sinatra/base'
require 'web_tools/support/service_helper'

module WebTools
  class Tool < Sinatra::Base
    include WebTools::Support::ServiceHelper

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
