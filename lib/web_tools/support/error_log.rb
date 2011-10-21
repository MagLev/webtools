# A singleton to hold on to the debuggable processes

module WebTools
  module Support
    module ErrorLog
      extend self
      extend Enumerable

      Entry = Struct.new :exception, :thread

      def add(hash)
        list << Entry.new(hash[:exception], hash[:thread])
        list.last
      end

      def delete(entry)
        list.delete entry
      end

      def list
        defined?(@@list) ? @@list : @@list = []
      end

      def to_a
        list
      end

      def each(&block)
        list.each(&block)
      end
    end
  end
end
