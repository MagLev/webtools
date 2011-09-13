require 'sinatra/base'
require 'maglev/reflection'
require 'web_tools'
require 'web_tools/support/code_browser'
require 'web_tools/support/service_helper'

module WebTools
  class CodeBrowser < Tool

    def self.description
      'Browse Namespaces, Classes, and Methods'
    end

    get '/' do # Tool>>json in ST
      json("response" => response_for_class,
           "dictList" => dict_list,
           "packageList" => ["no packages"],
           "classCatList" => ["undefined"],
           "classList" => class_list,
           "classDef" => "",
           "superList" => super_list,
           "methodFilterList" => [],
           "methodList" => method_list,
           "implList" => implementor_list,
           "method" => method)
    end

    post '/removeClass' do
      parent = Object.find_in_namespace(params["dict"] || "Object")
      parent.remove_const(params["klass"])
      json({})
    end

    post '/removeMethod' do
      parent = Object.find_in_namespace(params["dict"] || "Object")
      klass = parent.const_get(params["klass"])
      klass = klass.singleton_class if params["isMeta"]
      klass.remove_method(params["name"].to_sym)
      json({})
    end

    def response_for_class
      return nil if params["selector"]
      return nil unless params["request"]
      begin
        result = eval(params["request"])
        if result.is_a? Module
          return nil unless result.namespace
          response = { "dict" => result.namespace.my_class.inspect,
            "classCat" => nil,
            "klass" => result.inspect,
            "isMeta" => false,
            "superClass" => result.inspect,
            "methodCat" => nil,
            "selector" => nil,
            "implementor" => nil,
            "action" => "klass" }
          response.each {|k,v| params[k] = v }
          response
        else
          nil
        end
      rescue Exception => e
        { "action" => "klass",
          "error" => [e.class.inspect, e.message] }
      end
    end

    def dict_list
      names = []
      Object.each_module do |klass|
        if klass.namespace.nil? || klass.namespace.my_class == Object
          names << klass.inspect
        end
      end
      names.compact.uniq.sort
    end

    # Returns a sorted list of class and module names in the Ruby Namespace
    #
    # @return [Array] sorted list of class and module names found in the Ruby
    #         namespace hierarchy.
    def class_list
      @klass = Object.find_in_namespace(params["klass"]) if params["klass"]
      @klass = @klass.singleton_class if @klass && params["isMeta"]
      names = []
      dict = Object.find_in_namespace(params["dict"]) if params["dict"]
      (dict || Object).each_module { |klass|  names << klass.inspect }
      names.sort
    end

    def super_list
      return [] if @klass.nil?
      if params["superClass"]
        super_class = non_meta_name(params["superClass"])
        @super_class = Object.find_in_namespace(super_class)
        if super_class != params["superClass"]
          @super_class = @super_class.singleton_class
        end
      else
        @super_class = @klass
      end
      @super_list = @klass.ancestors.reverse
    end

    def implementor_list
      selected = params["implementor"]
      return [] if @super_list.nil? || @super_list.empty?
      list = @super_list.select do |c|
        c.instance_methods(false).include?(@selector)
      end
      @implementor = list.detect {|e| e.inspect == selected } || @klass
      list.collect(&:name)
    end

    def method
      return nil unless @implementor && @selector
      @method = @implementor.instance_method(@selector)
      { "dictionaryName" => "",
        "className" => @klass.inspect,
        "isMeta" => params["isMeta"],
        "category" => "",
        "source" => @method.source,
        "stepPoints" => @method.step_offsets,
        "sends" => @method.send_offsets }
    end

    def method_list
      selected = params["selector"]
      return [] if @super_list.nil? || @super_list.empty?
      list = @super_class.instance_methods(false)
      @selector = selected if list.include? selected
      list.sort
    end
  end
end
