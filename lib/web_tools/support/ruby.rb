module WebTools
  module Support
    # Provide information about the Ruby environment
    module Ruby
      MODULE_MOD_FNS = Module.instance_methods(false)

      def module_info_for(name)
        mod = find_in_namespace name
        { :constants        => mod.constants.sort,
          :class_methods    => module_fns_for(mod),
          :instance_methods => mod.instance_methods(false).sort }
      end
      module_function :module_info_for

      def module_fns_for(mod)
        res = []
        begin
          sclass = mod.__singleton_class
          res = sclass.instance_methods(false) - MODULE_MOD_FNS
        rescue => e
          # nothing
        end
        res
      end
      module_function :module_fns_for

      # Get the source code for the named class and method
      #
      # The success of this method depends on being called from a Ruby stack,
      # not a Smalltalk stack.
      #
      # @param [String] The name of the class
      # @param [String] The method name
      # @param [Boolean] if true, then look for a class method, otherwise look
      #         for an instance method.
      def source_for(class_name, method_name, instance_method=true)
        klass = find_in_namespace(class_name)
        gsnmeth = klass.gs_method_for(method_name, instance_method)
        src = gsnmeth.__source_string
        file,line = gsnmeth.__source_location
        file.nil? ? "#{src}\n# No file information available." : "#{src}\n##{file}:#{line}"
      end
      module_function :source_for

      # Return a display string suitable for rendering a constant.
      # Currently, it just returns the results of #inspect on the object.
      #
      # TODO
      #   1. If the const value is a proc, then return "Proc: " + the source code.
      #
      # @param [String,Symbol] class_name The name of the class to query.
      # @param [String,Symbol] const_name The name of the constant.
      # @return [String] A description of the constant.
      #
      def const_info_for(class_name, const_name)
        klass = find_in_namespace class_name
        const = klass.const_get const_name
        const.inspect
      end
      module_function :const_info_for

      # def logit(msg)
      #   File.open("/tmp/log", "a+") do |f|
      #     f.puts msg
      #     f.flush
      #   end
      # end
      # module_function :logit
    end
  end
end
