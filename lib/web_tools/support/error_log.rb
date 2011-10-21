# A singleton to hold on to the debuggable processes

module WebTools
  module Support
    module ErrorLog
      extend self
      extend Enumerable

      class Entry
        attr_reader :exception, :continuation

        def initialize(e, c)
          @exception = e
          @continuation = c
        end

        def process
          c = @continuation
          @process ||= Thread.new { c.call(nil) }
        end
      end

      def add(hash)
        list << Entry.new(hash[:exception], hash[:continuation])
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
