doit
Tool subclass: 'SharedPageCache'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: WebTools
	options: #()

%

! Remove existing behavior from SharedPageCache
doit
SharedPageCache removeAllMethods.
SharedPageCache class removeAllMethods.
%
! ------------------- Class methods for SharedPageCache
category: 'other'
set compile_env: 0
classmethod: SharedPageCache
description

	^'Information about the Shared Page Cache'.
%
category: 'other'
set compile_env: 0
classmethod: SharedPageCache
displayName

	^'Shared Page Cache'.
%
category: 'other'
set compile_env: 0
classmethod: SharedPageCache
sortOrder

	^7.
%
! ------------------- Instance methods for SharedPageCache
category: 'other'
set compile_env: 0
method: SharedPageCache
json

	| names stats size free globalDirty localDirty |
	names := System cacheStatisticsDescriptionForMonitor.
	stats := System sharedPageCacheMonitorCacheStatistics.
	size 		:= stats at: (names indexOf: 'FrameCount').
	free 		:= stats at: (names indexOf: 'FreeFrameCount').
	globalDirty := stats at: (names indexOf: 'GlobalDirtyPageCount').
	localDirty 	:= stats at: (names indexOf: 'LocalDirtyPageCount').
	^Dictionary new
		at: 'objectTable' 	put: ((stats at: (names indexOf: 'TotalOtPages'		)) / size * 100.0) rounded;
		at: 'bitmap' 		put: ((stats at: (names indexOf: 'TotalBitmapPages'	)) / size * 100.0) rounded;
		at: 'commitRecord' 	put: ((stats at: (names indexOf: 'TotalCrPages'		)) / size * 100.0) rounded;
		at: 'other' 		put: ((stats at: (names indexOf: 'TotalOtherPages'	)) / size * 100.0) rounded;
		at: 'data' 			put: ((stats at: (names indexOf: 'TotalDataPages'	)) / size * 100.0) rounded;
		at: 'free' 			put: (free / size * 100.0) rounded;
		at: 'globalDirty' 	put: (globalDirty / size * 100.0) rounded;
		at: 'localDirty' 	put: (localDirty / size * 100.0) rounded;
		at: 'clean' 		put: (size - localDirty - localDirty - free / size * 100.0) rounded;
		yourself.
%
doit
SharedPageCache category: 'WebTools'
%
