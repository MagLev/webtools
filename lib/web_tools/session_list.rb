require 'sinatra/base'
require 'web_tools'
require 'web_tools/support/service_helper'

class WebTools::SessionList < WebTools::Tool
  def self.description
    'Information about current sessions and other processes'
  end

  get '/' do
    json("labels" => SessionListLabels,
         "sessions" => session_list,
         "other" => non_session_list)
  end

  get '/statsForSlot' do
    slot = params["slot"]
    statistics = Maglev::System.cache_statistics(slot.to_i).to_a
    descriptions = Maglev::System.cache_statistics_description_for_type statistics[3]
    result = []
    bytes = []
    descriptions.size.times do |idx|
      name = descriptions[idx]
      metadata = Maglev::System::StatStatistic.cache_stat_descriptions["name"] ||
        ['' '' '' '' '']
      result.push("name" => name,
                  "type" => metadata[1],
                  "level" => metadata[2],
                  "units" => metadata[3],
                  "isOs" => metadata[4],
                  "value" => statistics[idx])
    end
    topFour = result[0..3]
    result = result[4..-1].sort_by {|a| a["name"] }
    json("stats" => topFour + result)
  end

  get '/cacheDescription' do
    name = params["name"]
    cache = Maglev::System::StatStatistic.cache_stat_descriptions
    details = cache["name"] || ["No description available for #{name}"]
    json("description" => details.first)
  end

  # Returns a hash of configuration parameters for the stone and the gem.
  # The has has three keys:
  #  + :timestamp => when the report was generated
  #  + :headers   => array of [name, description] pairs for the fields
  #  + :report    => An array of data.  Each entry is an array of the field data.
  #
  def session_list
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

  def non_session_list
    begin
      sessions = Maglev::System.current_session_ids.to_a
      slots = []
      1000.times do |slot| # Any number will do, theres a limited number of slots
        ary = Maglev::System.cache_statistics(slot).to_a
        unless sessions.include?(ary[2])
          slots << [slot, ary.first]
        end
      end
    rescue Exception
      return slots
    end
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

  # See System class>>#'descriptionOfSession:'
  SessionListLabels = [['User', 'UserProfile of the session, or nil if the UserProfile is recently created and not visible from this session''s transactional view, or the session is no longer active.'],
                       ['PID', 'The process ID of the Gem process of the session.'],
                       ['Host', 'The hostname of the machine running the Gem process (a String, limited to 127 bytes).'],
                       ['Prim', 'Primitive number in which the Gem is executing (if it is executing in a long primitive such as MFC).'],
                       ['View Age', 'Time since the session''s most recent beginTransaction, commitTransaction, or abortTransaction.'],
                       ['State', 'The session state (an enum from SesTaskStatusEType in session.ht).'],
                       ['Trans', 'One of the following: ''none'' if the session is in transactionless mode, ''out'' if it is not in a transaction, and ''in'' if it is in a transaction.'],
                       ['Oldest CR', 'A Boolean whose value is true if the session is currently referencing the oldest commit record, and false if it is not.'],
                       ['Serial', 'The session''s serial number. A serial number will not be reused until the stone is restarted.'],
                       ['Session', "The session''s sessionId. The configured maximum is #{Maglev::System.max_session_id} for this stone."],
                       ['GCI IP', 'A String containing the ip address of host running the GCI process. If the GCI application is linked (using libgcilnk*.so or gcilnk*.dll) this ip address is the address of the machine running the gem process.'],
                       ['Priority', 'The priority of the session where 0 is lowest, 2 is normal, and 4 is highest. Session priority is used by the stone to order requests for service by sessions.'],
                       ['Host ID', 'Unique host ID of the host where the session is running.'],
                       ['Quiet', 'Time since the session''s most recent request to stone.'],
                       ['Age', 'Time since the session logged in.'],
                       ['CRB', 'Commit Record Backlog: number of commits which have occurred since the session obtained its view.'],
                       ['Slot', 'The session''s cache process slot number if it is connected to the same shared page cache as Server. A return of nil indicates the session could not be located (so the gem is likely on another host).']]
end
