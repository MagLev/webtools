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
    name = non_meta_name(params["klass"])
    is_meta = !(name == params["klass"])
    klass = reflect(Object).constant(name).value
    klass = klass.singleton_class if is_meta
    meth = klass.method(params["selector"])
    { "dictionaryName" => params["dict"],
      "className" => klass.name,
      "isMeta" => is_meta,
      "source" => meth.source,
      "stepPoints" => meth.step_offsets,
      "sends" => meth.send_offsets }
  end

  def implementors
    return {} unless params["find"]
    methods(system.implementations_of(params["find"]))
  end

  def senders
    return {} unless params["find"]
    methods(system.senders_of(params["find"]))
  end

  def references_to_global
    return {} # Not supported on Ruby, too dynamic
  end

  def methods(list)
    list = list.collect do |meth|
      klass = meth.defining_class
      nesting = klass.nesting
      dict = nesting[1] ? nesting[1].name : "" # [klass, parent, ...]
      { "dict" => dict,
        "klassCat" => "",
        "klass" => klass.name,
        "category" => "",
        "selector" => meth.selector }
    end
    { "list" => list }
  end
end
