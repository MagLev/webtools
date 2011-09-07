doit
Object subclass: 'StatProcess'
  instVarNames: #( type name processID
                    sessionID stats timeList primitives
                    crb)
  classVars: #( PrimitiveMap)
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: WebTools
  options: #()

%

! Remove existing behavior from StatProcess
doit
StatProcess removeAllMethods.
StatProcess class removeAllMethods.
%
! ------------------- Class methods for StatProcess
! ------------------- Instance methods for StatProcess
category: 'accessors'
set compile_env: 0
method: StatProcess
beginSeconds

	^timeList first.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
beginTime

	^self statmonitor timeStampFromSeconds: self beginSeconds.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
count

	^timeList size.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
crb

	^crb.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
endSeconds

	^timeList last.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
endTime

	^self statmonitor timeStampFromSeconds: self endSeconds.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
hasAnySelections

	^stats anySatisfy: [:each | each isSelected].
%
category: 'initialize'
set compile_env: 0
method: StatProcess
initializeFrom: anArray 

	| bytes |
	crb := Array new.
	self initializePrimitiveMap.
	bytes := ByteArray new: 4.
	timeList := OrderedCollection with: (anArray at: 2).
	name := anArray at: 3.
	processID := anArray at: 4.
	bytes unsigned32At: 1 put: (anArray at: 5).
	sessionID := bytes signed32At: 1.
	1 to: stats size do: [:i | 
		(stats at: i) 
			process: self;
			initialize: anArray;
			yourself.
	].
%
category: 'initialize'
set compile_env: 0
method: StatProcess
initializePrimitiveMap

	PrimitiveMap isNil ifTrue: [
		PrimitiveMap := Dictionary new
			at: 392 put: 'audit';
			at: 394 put: 'backup';
			at: 395 put: 'restore';
			at: 876 put: 'MFC';
			at: 896 put: 'list';
			at: 899 put: 'refs';
			yourself.
	].
%
category: 'accessors'
set compile_env: 0
method: StatProcess
maxCRB

	crb isEmpty ifTrue: [^0].
	^crb 
		inject: crb first value
		into: [:max :each | max max: each value].
%
category: 'accessors'
set compile_env: 0
method: StatProcess
name

	^name.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
name: aString

	name := aString.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
primitives

	primitives notNil ifTrue: [^primitives].
	primitives := Array new.
	stats do: [:eachStat | 
		eachStat name = 'PrimitiveNumber' ifTrue: [
			eachStat data asIdentitySet do: [:each | 
				0 < each ifTrue: [
					primitives add: each.
				].
			].
		].
	].
	primitives := primitives asSortedCollection asArray collect: [:each | 
		PrimitiveMap at: each ifAbsent: [each].
	].
	^primitives.
%
category: 'other'
set compile_env: 0
method: StatProcess
printOn: aStream

	| time |
	time := self statmonitor timeStampFromSeconds: timeList first.
	time := time printString.
	time := time copyFrom: 12 to: 16.
	aStream 
		nextPutAll: name;
		nextPutAll: ' (';
		nextPutAll: time;
		nextPutAll: ')';
		yourself.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
processID

	^processID.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
samplesSize

	^self count.
	
%
category: 'private'
set compile_env: 0
method: StatProcess
sanitize

	"timeList := timeList collect: [:each | each + 18000]."	"Add five hours to get from ART to PST"
	type name = 'Stn' ifTrue: [
		name := 'gs32stone'.
	].
	type name = 'SolarisSystem' ifTrue: [
		name := 'hamburg'.
	].
%
category: 'accessors'
set compile_env: 0
method: StatProcess
seconds

	^self endSeconds - self beginSeconds + 1.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
sessionID

	^sessionID.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
statistics
	^stats
%
category: 'accessors'
set compile_env: 0
method: StatProcess
statmon

	^type statmon.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
statmonitor

	^type statmon.
%
category: 'other'
set compile_env: 0
method: StatProcess
statNamed: aString

	^self statistics
		detect: [:each | each name = aString]
		ifNone: [nil].
%
category: 'initialize'
set compile_env: 0
method: StatProcess
stats: anObject
	"OrderedCollection of StatSummary instances; copied so each process has its own"

	stats := anObject
%
category: 'accessors'
set compile_env: 0
method: StatProcess
timeList

	^timeList.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
type

	^type.
%
category: 'accessors'
set compile_env: 0
method: StatProcess
type: aStatType

	type := aStatType.
%
category: 'initialize'
set compile_env: 0
method: StatProcess
updateFrom: anArray

	timeList add: (anArray at: 2).
	1 to: stats size do: [:i | 
		(stats at: i) updateFrom: anArray.
	].
%
doit
StatProcess category: 'Statmonitor'
%
