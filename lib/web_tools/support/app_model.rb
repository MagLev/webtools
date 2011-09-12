
require 'web_tools/support/code_browser'
require 'web_tools/support/smalltalk_extensions'

# This module emulates all of the API from the Smalltalk side of things
module WebTools
  module Support
    # This is a ViewModel for the WebTools Application.
    #
    # All of the methods that return "Objects" should return a Hash.  Keys
    # beginning with '_' are reserved for metadata applied by the GUI.
    module AppModel
      extend self

      VERSION_HEADERS = [['Attribute', 'The attribute.'],
                         ['Stone', 'The value for the stone process.'],
                         ['WebTools', 'The value for the WebTools vm process (if different than the Stone''s value)']]


      # Returns a hash of configuration parameters for the stone and the gem.
      # The has has three keys:
      #  + :timestamp => when the report was generated
      #  + :headers   => array of [name, description] pairs for the fields
      #  + :report    => An array of data.  Each entry is an array of the field data.
      #
      def version_report
        stone_rpt = stone_version_report
        gem_rpt = gem_version_report
        data = { }
        (stone_rpt.keys + gem_rpt.keys).each do |k|
          g = stone_rpt[k] == gem_rpt[k] ? '' : gem_rpt[k]
          data[k] = [stone_rpt[k], g]
        end
        { :timestamp => Time.now.asctime,
          :headers   => VERSION_HEADERS,
          :report    => data }
      end

      # An array of [name, description] entries for the fields in the session report.
      SESSION_FIELDS =
        [['User', 'The UserProfile of the session, or nil if the UserProfile is recently created and not visible from this session''s transactional view, or the session is no longer active.'],
         ['PID', 'The process ID of the Gem process of the session.'],
         ['Host', 'The hostname of the machine running the Gem process (a String, limited to 127 bytes).'],
         ['Prim #', 'Primitive number in which the Gem is executing (if it is executing in a long primitive such as MFC).'],
         ['View Age', 'Time since the session''s most recent beginTransaction, commitTransaction, or abortTransaction.'],
         ['State', 'The session state (an enum from SesTaskStatusEType in session.ht).'],
         ['Trans', 'One of the following: ''none'' if the session is in transactionless mode, ''out'' if it is not in a transaction, and ''in'' if it is in a transaction.'],
         ['Oldest CR?', 'A Boolean whose value is true if the session is currently referencing the oldest commit record, and false if it is not.'],
         ['Serial', 'The session''s serial number. A serial number will not be reused until the stone is restarted.'],
         ['Session', "The session's sessionId."],
         ['GCI IP', 'A String containing the ip address of host running the GCI process. If the GCI application is linked (using libgcilnk*.so or gcilnk*.dll) this ip address is the address of the machine running the gem process.'],
         ['Priority', 'The priority of the session where 0 is lowest, 2 is normal, and 4 is highest. Session priority is used by the stone to order requests for service by sessions.'],
         ['Host ID', 'Unique host ID of the host where the session is running.'],
         ['Quiet', 'Time since the session''s most recent request to stone.'],
         ['Age', 'Time since the session logged in.'],
         ['CRB', 'Commit Record Backlog: number of commits which have occurred since the session obtained its view.']]

      # Returns a hash of configuration parameters for the stone and the gem.
      # The has has three keys:
      #  + :timestamp => when the report was generated
      #  + :headers   => array of [name, description] pairs for the fields
      #  + :report    => An array of data.  Each entry is an array of the field data.
      #
      def session_report
        ts = Time.now
        now = ts.to_i
        session_info = Maglev::System.current_session_ids.map do |id|
          sess_desc = Maglev::System.description_of_session id
          sess_desc[0] = sess_desc[0].instance_variable_get(:@_st_userId) # UserProfile
          sess_desc[3] = '' if sess_desc[3] == 0                          # Primitive?
          sess_desc[4] = format_secs(now - sess_desc[4])                  # View Age
          sess_desc[6] = ['none', 'out', 'in'][sess_desc[6] + 1]          # Transaction
          sess_desc[13] = format_secs(now - sess_desc[13])                # Quiet
          sess_desc[14] = format_secs(now - sess_desc[14])                # Age
          sess_desc
          # sess_cache_slot = Maglev::System.cache_slot_for_sessionid id
        end
        session_info
      end
    end
  end
end
