require 'sinatra/base'
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
           "packageList" => [],
           "classCatList" => class_cat_list,
           "classList" => class_list,
           "classDef" => "",
           "superList" => super_list,
           "methodFilterList" => [],
           "methodList" => method_list,
           "implList" => implementor_list,
           "method" => method)
    end

    post '/removeClass' do
      parent = reflect(Object).constant(params["dict"] || "Object").value
      parent.constant(params["klass"]).delete
      json({})
    end

    post '/removeMethod' do
      parent = reflect(Object).constant(params["dict"] || "Object").value
      klass = parent.constant(params["klass"]).value
      klass = klass.singleton_class if params["isMeta"]
      klass.method(params["name"]).delete
      json({})
    end

    def response_for_class
      # return nil if params["selector"]
      # return nil unless params["request"]
      # begin
      #   result = eval(params["request"])
      #   if result.is_a? Module
      #     return nil unless result.namespace
      #     response = { "dict" => result.namespace.my_class.inspect,
      #       "classCat" => nil,
      #       "klass" => result.inspect,
      #       "isMeta" => false,
      #       "superClass" => result.inspect,
      #       "methodCat" => nil,
      #       "selector" => nil,
      #       "implementor" => nil,
      #       "action" => "klass" }
      #     response.each {|k,v| params[k] = v }
      #     response
      #   else
      #     nil
      #   end
      # rescue Exception => e
      #   { "action" => "klass",
      #     "error" => [e.class.inspect, e.message] }
      # end
    end

    def dict_list
      return [] unless params['isDictsTab']
      pkgs = ["Object"]
      reflect(Object).nested_classes.each do |k|
        pkgs << k.name if k.nested_classes.any?
        @dict = k if @dict.nil? && params['dict'] == k.name
      end
      pkgs.compact.uniq.sort
    end

    def class_cat_list
      names = []
      filter = (@dict || @package || reflect(Object))
      filter.nested_classes do |klass|
        path = klass.name.gsub("::", "-")
        names << path
        if @class_category.nil? && params['classCat'] == path
          @class_category = klass
        end
      end
      @class_category ||= filter
      names.compact.uniq.sort
    end

    # Returns a sorted list of class and module names in the Ruby Namespace
    #
    # @return [Array] sorted list of class and module names found in the Ruby
    #         namespace hierarchy.
    def class_list
      @klass = reflect(Object).constant(params["klass"]).value if params["klass"]
      @klass = @klass.singleton_class if @klass && params["isMeta"]
      @class_category.nested_classes.collect(&:name).sort
    end

    def super_list
      return [] if @klass.nil?
      if params["superClass"]
        super_class = non_meta_name(params["superClass"])
        @super_class = reflect(Object).constant(super_class).value
        if super_class != params["superClass"]
          @super_class = @super_class.singleton_class
        end
      else
        @super_class = @klass
      end
      @super_list = @klass.ancestors.reverse
      @super_list.collect(&:name)
    end

    def implementor_list
      selected = params["implementor"]
      return [] if @super_list.nil? || @super_list.empty?
      list = @super_list.select do |c|
        c.methods.include?(@selector)
      end
      @implementor = list.detect {|e| e.name == selected } || @klass
      list.collect(&:name)
    end

    def method
      return nil unless @implementor && @selector
      nil.pause if Module === @implementor
      @method = @implementor.method(@selector)
      { "dictionaryName" => "",
        "className" => @klass.name,
        "isMeta" => params["isMeta"],
        "category" => "",
        "source" => @method.source,
        "stepPoints" => @method.step_offsets,
        "sends" => @method.send_offsets }
    end

    def method_list
      selected = params["selector"]
      return [] if @super_list.nil? || @super_list.empty?
      list = @super_class.methods
      @selector = selected if list.include? selected
      list.sort
    end
  end
end
