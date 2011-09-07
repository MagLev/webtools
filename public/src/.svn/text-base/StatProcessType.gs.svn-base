doit
Object subclass: 'StatProcessType'
  instVarNames: #( statmon name id
                    stats processes)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: WebTools
  options: #()

%

! Remove existing behavior from StatProcessType
doit
StatProcessType removeAllMethods.
StatProcessType class removeAllMethods.
%
! ------------------- Class methods for StatProcessType
category: 'instance creation'
set compile_env: 0
classmethod: StatProcessType
fromString: aString statmon: aStatmonitor

	^self new
		initialize: aString;
		statmon: aStatmonitor;
		yourself.
%
! ------------------- Instance methods for StatProcessType
category: 'accessing'
set compile_env: 0
method: StatProcessType
firstBeginTime

	| result |
	processes do: [:each | 
		| next |
		next := each beginTime.
		(result isNil or: [next < result]) ifTrue: [result := next].
	].
	^result.
	
%
category: 'testing'
set compile_env: 0
method: StatProcessType
hasAnySelections

	^processes anySatisfy: [:each | each hasAnySelections].
%
category: 'accessing'
set compile_env: 0
method: StatProcessType
id

	^id.
	
%
category: 'initialize'
set compile_env: 0
method: StatProcessType
initialize: aString

	| stream next offset |
	stream := ReadStreamLegacy on: aString.
	name := stream skipSeparators; nextWord.
	(next := stream nextWord) = '(' ifFalse: [self error: 'Invalid StatType format: ' , aString printString].
	stats := OrderedCollection new.
	offset := 0.
	[
		(next := stream nextWord) = ')'.
	] whileFalse: [
		offset := offset + 1.
		stats add: (StatStatistic new
			name: next;
			processType: self;
			offset: offset;
			yourself).
	].
	id := self mapID: stream nextWord asNumber.
	processes := Dictionary new.
%
category: 'testing'
set compile_env: 0
method: StatProcessType
isGem

	^name = 'Gem'.
%
category: 'testing'
set compile_env: 0
method: StatProcessType
isSharedPageCacheMonitor

	^name = 'Shrpc'.
%
category: 'testing'
set compile_env: 0
method: StatProcessType
isStone

	^name = 'Stn'.
%
category: 'accessing'
set compile_env: 0
method: StatProcessType
lastEndTime

	| result |
	processes do: [:each | 
		| next |
		next := each endTime.
		(result isNil or: [result < next]) ifTrue: [result := next].
	].
	^result.
	
%
category: 'initialize'
set compile_env: 0
method: StatProcessType
mapID: anInteger

	^anInteger.
"
	^(Dictionary new
		at: 0 put: 1;
		at: 1 put: 2;
		at: 2 put: 4;
		at: 3 put: 8;
		at: 4 put: 16;
		at: 5 put: 32;
		yourself)
			at: anInteger
			ifAbsent: [anInteger].
"
%
category: 'accessing'
set compile_env: 0
method: StatProcessType
name

	^name.
	
%
category: 'accessing'
set compile_env: 0
method: StatProcessType
numberOfProcesses

	^processes size.

%
category: 'accessing'
set compile_env: 0
method: StatProcessType
numberOfSamples

	^stats size.

%
category: 'printing'
set compile_env: 0
method: StatProcessType
printOn: aStream

	aStream nextPutAll: name.
%
category: 'accessing'
set compile_env: 0
method: StatProcessType
processes

	^processes asSortedCollection: [:a :b | a name <= b name].
%
category: 'private'
set compile_env: 0
method: StatProcessType
sanitize

	| process |
	(name = 'Stn' and: [processes includesKey: 'ta1v6stone']) ifTrue: [
		process := processes at: 'ta1v6stone'.
		processes removeKey: 'ta1v6stone'.
		processes at: 'gs32stone' put: process.
	].
	(name = 'SolarisSystem' and: [processes includesKey: 'psun2001']) ifTrue: [
		process := processes at: 'psun2001'.
		processes removeKey: 'psun2001'.
		processes at: 'psun2001' put: process.
	].
	processes do: [:each | each sanitize].
%
category: 'accessing'
set compile_env: 0
method: StatProcessType
statistics

	^stats "asSortedCollection: [:a :b | a offset <= b offset]".
%
category: 'accessing'
set compile_env: 0
method: StatProcessType
statmon

	^statmon.
%
category: 'accessing'
set compile_env: 0
method: StatProcessType
statmon: aStatmonitor

	statmon := aStatmonitor.
%
category: 'initialize'
set compile_env: 0
method: StatProcessType
summarizeLine: anArray

	| processName process list |
	processName := anArray at: 3.
	process := processes
		at: processName 
		ifAbsent: [nil].
	process notNil ifTrue: [
		process updateFrom: anArray.
		^self.
	].
	"OrderedCollection of StatSummary instances; copied so each process has its own"
	list := self statistics collect: [:each | each copy].
	list := list copyFrom: 6 to: list size.
	process := StatProcess new 
		type: self; 
		stats: list;
		initializeFrom: anArray;
		yourself.
	processes 
		at: processName 
		put: process.
%
doit
StatProcessType category: 'Statmonitor'
%
