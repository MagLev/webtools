doit
Tool subclass: 'Statistics'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: WebTools
  options: #()

%

! Remove existing behavior from Statistics
doit
Statistics removeAllMethods.
Statistics class removeAllMethods.
%
! ------------------- Class methods for Statistics
category: 'other'
set compile_env: 0
classmethod: Statistics
description

	^'Load and view statmonitor files'.
%
category: 'other'
set compile_env: 0
classmethod: Statistics
displayName

	^'Statistics'.
%
category: 'other'
set compile_env: 0
classmethod: Statistics
sortOrder

	^4.
%
! ------------------- Instance methods for Statistics
category: 'Public'
set compile_env: 0
method: Statistics
json

	^Dictionary new
		at: 'files' put: Statmonitor files;
		yourself.
%
category: 'Public'
set compile_env: 0
method: Statistics
process

	| path oop file process stats nonZero zero |
	path := arguments at: 'path' ifAbsent: [self error: 'Path is missing!'].
	oop := (arguments at: 'processOop' ifAbsent: [self error: 'Process is missing!']) asNumber.
	file := Statmonitor fileAt: path.
	process := file processes
		detect: [:each | each asOop = oop]
		ifNone: [self error: 'Process not found!'].
	stats := process statistics.
	stats := stats asSortedCollection: [:a :b  | a name <= b name].
	zero := stats select: [:each | each min = 0 and: [each max = 0]].
	nonZero := stats reject: [:each | each min = 0 and: [each max = 0]].
	stats := nonZero asArray , zero asArray collect: [:each | 
		Dictionary new
			at: 'oop'		put: each asOop;
			at: 'name' 	put: each name;
			at: 'type'	put: each type;
			at: 'units'	put: each units;
			at: 'min'		put: each min;
			at: 'max'		put: each max;
			at: 'mean'	put: each mean;
			at: 'rate'	put: each rate;
			at: 'descr'	put: each description;
			yourself.
	].
	^Dictionary new
		at: 'statistics' put: stats;
		yourself.
%
category: 'Public'
set compile_env: 0
method: Statistics
processes

	| path file processes statNames |
	path := arguments at: 'path' ifAbsent: [self error: 'Path is missing!'].
	file := Statmonitor fileAt: path.
	statNames := Set new.
	file processTypeList do: [:eachType | 
		eachType statistics do: [:each | statNames add: each name].
	].
	processes := file processes collect: [:each | 
		Dictionary new
			at: 'oop'			put: each asOop;
			at: 'type' 		put: each type name;
			at: 'pid'			put: each processID;
			at: 'session'	put: (each sessionID <= 0 ifTrue: [''] ifFalse: [each sessionID]);
			at: 'name'		put: each name;
			at: 'count'		put: each count;
			at: 'start'		put: each beginTime printString;
			at: 'sort'			put: each beginTime;
			at: 'prim'		put: each primitives;
			at: 'crb'			put: each maxCRB;
			yourself.
	].
	processes := processes asSortedCollection: [:a :b | (a at: 'sort') <= (b at: 'sort')].
	processes do: [:each | each removeKey: 'sort'].
	^Dictionary new
		at: 'processes' 	put: processes;
		at: 'statNames'	put: statNames asSortedCollection;
		yourself.
%
category: 'Public'
set compile_env: 0
method: Statistics
readFile

	| path |
	path := arguments at: 'path' ifAbsent: [self error: 'Path is missing!'].
	(GsFile existsOnServer: path) ifFalse: [self error: 'File not found!'].
	[Statmonitor path: path] forkAt: Processor userBackgroundPriority.
	(Delay forMilliseconds: 50) wait.
	^self json.
%
category: 'Public'
set compile_env: 0
method: Statistics
removeFile

	| path |
	path := arguments at: 'path' ifAbsent: [self error: 'Path is missing!'].
	(Statmonitor fileAt: path) remove.
	^self json.
%
category: 'Public'
set compile_env: 0
method: Statistics
spcPageTypes

	| path file spcMon flotData options |
	path := arguments at: 'path' ifAbsent: [self error: 'Path is missing!'].
	file := Statmonitor fileAt: path.
	spcMon := file sharedPageCacheMonitor.
	flotData := Array new
		add: (Dictionary new
			at: 'data' 		put: (spcMon statNamed: 'TotalOtherPages') flotData;
			at: 'label' 		put: 'Other Pages';
			yourself);
		add: (Dictionary new
			at: 'data' 		put: (spcMon statNamed: 'TotalDataPages') flotData;
			at: 'label' 		put: 'Data Pages';
			yourself);
		add: (Dictionary new
			at: 'data' 		put: (spcMon statNamed: 'TotalCrPages') flotData;
			at: 'label' 		put: 'Commit Record Pages';
			yourself);
		add: (Dictionary new
			at: 'data' 		put: (spcMon statNamed: 'TotalOtPages') flotData;
			at: 'label' 		put: 'Object Table Pages';
			yourself);
		add: (Dictionary new
			at: 'data' 		put: (spcMon statNamed: 'TotalBitmapPages') flotData;
			at: 'label' 		put: 'Bitmap Pages';
			yourself);
		yourself.
	flotData do: [:each | 
		each
			at: 'clickable'	put: false;
			at: 'hoverable'	put: false;
			yourself.
	].
	flotData last at: 'hoverable' put: true.
	options := Dictionary new
		at: 'xaxis' 	put: (Dictionary new at: 'mode' put: 'time'; yourself);
		at: 'grid'		put: (Dictionary new at: 'clickable' put: false; at: 'hoverable' put: true; yourself);
		at: 'legend'	put: (Dictionary new at: 'position' put: 'se'; yourself);
		at: 'series'	put: (Dictionary new at: 'stack' put: true; at: 'lines' put: (Dictionary new at: 'fill' put: true; yourself); yourself);
		yourself.
	^Dictionary new
		at: 'statName'		put: 'spcPageTypes';
		at: 'processOop'	put: spcMon asOop;
		at: 'flotData' 		put: flotData;
		at: 'options'		put: options;
		yourself.
%
category: 'Public'
set compile_env: 0
method: Statistics
statistic

	| path oop statName file process statistic flotData options |
	path := arguments at: 'path' ifAbsent: [self error: 'Path is missing!'].
	oop := (arguments at: 'processOop' ifAbsent: [self error: 'Process is missing!']) asNumber.
	statName := arguments at: 'statName' ifAbsent: [self error: 'Statistic name is missing!'].
	file := Statmonitor fileAt: path.
	process := file processes
		detect: [:each | each asOop = oop]
		ifNone: [self error: 'Process not found!'].
	statistic := process statistics 
		detect: [:each | each name = statName]
		ifNone: [self error: 'Statistic not found!'].
	flotData := Dictionary new
		at: 'data' put: statistic flotData;
		at: 'label' put: statName;
		yourself.
	options := Dictionary new
		at: 'xaxis' 	put: (Dictionary new at: 'mode' put: 'time'; yourself);
		at: 'grid'		put: (Dictionary new at: 'clickable' put: false; at: 'hoverable' put: true; yourself);
		yourself.
	^Dictionary new
		at: 'path'			put: path;
		at: 'processOop' 	put: oop;
		at: 'statName'		put: statName;
		at: 'flotData'		put: (Array with: flotData);
		at: 'options'		put: options;
		yourself.
%
doit
Statistics category: 'WebTools'
%
