require 'sinatra/base'
require 'web_tools'
require 'web_tools/support/app_model'
require 'web_tools/support/service_helper'

class WebTools::MethodList < WebTools::Tool
  def self.description
    super
  end

  get '/' do
    json(execute(params["type"]))
  end

  def execute(meth)
    callables = { "method" => method(:find_method),
      "implementors" => method(:implementors),
      "senders" => method(:senders),
      "referencesToGlobal" => method(:references_to_global) }
    callable = callables[meth]
    return {"a" => "AAA"} if callable.nil?
    callable[]
  end

  def find_method
    name = params["klass"]
    name = name["#<Class:".length..-2] if is_meta = !!(name =~ /^#<Class:.*>$/)
    klass = Object.find_in_namespace(name)
    klass = klass.singleton_class if is_meta
    meth = klass.instance_method(params["selector"])
    { "dictionaryName" => params["dict"],
      "className" => klass.inspect,
      "isMeta" => is_meta,
      "source" => meth.source,
      "stepPoints" => meth.step_offsets,
      "sends" => meth.send_offsets }
  end

  def implementors
    return {} unless params["find"]
    methods(params["find"].implementors)
  end

  def senders
    return {} unless params["find"]
    methods(params["find"].senders)
  end

  def references_to_global
    return {} # Not supported on Ruby, too dynamic
  end

  def methods(list)
    list = list.collect do |meth|
      klass = meth.in_class
      namespace = klass.namespace
      dict = namespace.my_class.inspect if namespace
      { "dict" => dict || "",
        "klassCat" => "",
        "klass" => klass.inspect,
        "category" => "",
        "selector" => meth.name }
    end
    { "list" => list }
  end
end
