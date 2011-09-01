
require 'web_tools/support/code_browser'

# This module emulates all of the API from the Smalltalk side of things
module WebTools

  # This is a ViewModel for the WebTools Application.
  #
  # All of the methods that return "Objects" should return a Hash.  Keys
  # beginning with '_' are reserved for metadata applied by the GUI.
  class AppModel
    def initialize
    end

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
      { :timestamp => ts.asctime,
        :headers   => SESSION_FIELDS,
        :report    => session_info }
    end


    SECS_PER_DAY  = 86400
    SECS_PER_HOUR = 3600
    SECS_PER_MIN  = 60
    SECS_PER_SEC  = 1

    # Format number of seconds like "3 days 12:07:58"
    def format_secs(seconds)
      splits = []
      [SECS_PER_DAY, SECS_PER_HOUR, SECS_PER_MIN, SECS_PER_SEC].each do |x|
        splits << seconds / x
        seconds = seconds % x
      end
      days = splits.shift

      ts = "%02d:%02d:%02d" % splits
      days > 0 ? "#{days} #{days == 1 ? 'day' : 'days'} #{ts}" : ts
    end

    def stone_version_report
      results = { }
      rpt = Maglev::System.stone_version_report
      rpt.keys.each { |k| results[k] = rpt.at(k) }
      results
    end

    def gem_version_report
      results = { }
      rpt = Maglev::System.gem_version_report
      rpt.keys.each { |k| results[k] = rpt.at(k) }
      results
    end
  end
end
