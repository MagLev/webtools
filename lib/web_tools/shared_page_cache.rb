require 'sinatra/base'
require 'web_tools'
# require 'web_tools/support/app_model'
# require 'web_tools/support/service_helper'

class WebTools::SharedPageCache < WebTools::Tool
  def self.description
    "Information about the Shared Page Cache"
  end

  get '/' do
    values = Maglev::System.cache_statistics(0) # SPC Monitor
    descrs = Maglev::System.cache_statistics_description_for_type(values[3])

    stats = Hash[descrs.zip(values)]
    size = stats["FrameCount"].to_f
    free = stats["FreeFrameCount"]
    globalDirty = stats["GlobalDirtyPageCount"]
    localDirty = stats["LocalDirtyPageCount"]
    json("objectTable" => (stats["TotalOtPages"] / size * 100).round,
         "bitmap" => (stats["TotalBitmapPages"] / size * 100).round,
         "commitRecord" => (stats["TotalCrPages"] / size * 100).round,
         "other" => (stats["TotalOtherPages"] / size * 100).round,
         "data" => (stats["TotalDataPages"] / size * 100).round,
         "free" => (free / size * 100).round,
         "globalDirty" => (globalDirty / size * 100).round,
         "localDirty" => (localDirty / size * 100).round,
         "clean" => ((size - localDirty - globalDirty - free) / size * 100).round)
  end
end
