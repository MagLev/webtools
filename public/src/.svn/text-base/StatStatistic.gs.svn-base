doit
Object subclass: 'StatStatistic'
	instVarNames: #( name processType type
	                  offset process isCounter level
	                  units description data min
	                  minTime max maxTime sum)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: WebTools
	options: #()

%

! Remove existing behavior from StatStatistic
doit
StatStatistic removeAllMethods.
StatStatistic class removeAllMethods.
%
! ------------------- Class methods for StatStatistic
category: 'cache'
set compile_env: 0
classmethod: StatStatistic
cacheStatDescriptions
"
StatStatistic cacheStatDescriptions.

A Dictionary where the key is a String (the name of a statistic) and the value is an Array:
1.	description
2.	type - is one of the following: 'counter' 'uvalue' 'svalue' 'float'
3.	level - is one of the following: 'common' 'advanced' 'wizard'
4.	units - is a string that describes what the stat measures. Try to use the same unit string of other stats.
5.	isOs - is 'true' if the statistic comes from the operating system and is 'false' if not."

	^WebTools
		at: #'CacheStat-Descriptions'
		ifAbsentPut: [self _stats].
%
category: 'cache'
set compile_env: 0
classmethod: StatStatistic
clearCache
"
	StatStatistic clearCache.
"
	WebTools 
		removeKey: #'CacheStat-Descriptions' 	ifAbsent: [];
		yourself.
%
category: 'cache'
set compile_env: 0
classmethod: StatStatistic
_stats

	| gsFile stream line stats string i |
	gsFile := GsFile openReadOnServer: '$GEMSTONE/bin/vsd'.
	[
		[
			line := gsFile nextLine.
			line notNil and: [line ~= 'array set statDocs {
'].
		] whileTrue: [].
		stream := WriteStream on: String new.
		[
			(line := gsFile nextLine) = 'array set vsdhelp {
'.
		] whileFalse: [
			stream nextPutAll: line.
		].
	] ensure: [
		gsFile close.
	].
	stream := ReadStream on: stream contents.
	stats := Dictionary new.
	[
		string := stream upTo: $}.
		0 < (i := string indexOf: ${).
	] whileTrue: [
		| array |
		string := string copyFrom: i + 1 to: string size.
		array := Array new: 5.
		stats at: string put: array.
		string := stream upTo: $"; upTo: $".
		string := string collect: [:each | each = $\ ifTrue: [Character lf] ifFalse: [each]].
		array at: 1 put: string.
	].
	1 to: 47 do: [:i | 
		stats at: 'SessionStat' 		, i printString put: (stats at: 'SessionStat0'	) copy.
		stats at: 'VMStat' 			, i printString put: (stats at: 'VMStat0'		) copy.
		stats at: 'GlobalStat' 		, i printString put: (stats at: 'GlobalStat0'		) copy.
	].
	0 to: 10 do: [:i | 
		stats at: 'sharedCounter' 	, i printString put: (stats at: 'sharedCounter'	) copy.
	].
	[
		stream atEnd or: [(line := stream nextLine) = 'array set statDefinitions {'].
	] whileFalse: [].
	[
		string := (stream upTo: ${) trimSeparators.
		string isEmpty.
	] whileTrue: [
		| array values |
		array := stats 
			at: (stream upTo: $})
			ifAbsentPut: [#('' nil nil nil nil) copy].
		values := ReadStream on: (stream upTo: ${; upTo: $}).
		2 to: 5 do: [:i | 
			string := values upTo: ((values peekFor: $")
				ifTrue: [$"]
				ifFalse: [$ ]).
			array at: i put: string asSymbol.
		].
	].
	stats do: [:each | 
		each
			at: 5 
			put: (each at: 5) = #'true'.
	].
	stream := nil.
	^stats.
%
! ------------------- Instance methods for StatStatistic
category: 'accessors'
set compile_env: 0
method: StatStatistic
data

	^data.
	
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
description

	^description.
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
flotData

	| tzOffset timeList result |
	tzOffset := processType statmon timeZoneOffset.
	timeList := process timeList.
	result := OrderedCollection new.
	type == #'counter' ifTrue: [
		| priorTime priorValue newTime newValue rate |
		priorTime := timeList at: 1.
		priorValue := data at: 1.
		2 to: (timeList size min: data size) do: [:i | 
			newTime := timeList at: i.
			newValue := data at: i.
			rate := (newValue - priorValue) / (newTime - priorTime).
			result add: (Array with: newTime + tzOffset * 1000 with: rate).
			priorTime := newTime.
			priorValue := newValue.
		].
	] ifFalse: [
		1 to: (timeList size min: data size) do: [:i | 
			result add: (Array with: (timeList at: i) + tzOffset * 1000 with: (data at: i)).
		].
	].
	^result.
%
category: 'initialize'
set compile_env: 0
method: StatStatistic
initialize: anArray

	| struct value |
	struct := self class cacheStatDescriptions
		at: name
		ifAbsent: [nil].
	isCounter := false.
	level := ''.
	units := ''.
	struct notNil ifTrue: [
		description := struct at: 1.
		type := struct at: 2.
		isCounter := type == #'counter'.
		level := struct at: 3.
		units := struct at: 4.
	].
	value := self valueOf: (anArray at: offset).
	data := OrderedCollection with: value.
	minTime := anArray at: 2.
	min := value.
	max := value.
	sum := value.
	maxTime := minTime.
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
isCounter

	^isCounter.
	
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
level

	^level.
	
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
max

	^max.
	
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
maxTime

	^self statmon timeStampFromSeconds: maxTime.
	
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
mean

	^sum / data size.
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
min

	^min.
	
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
minTime

	^self statmon timeStampFromSeconds: minTime.
	
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
name

	^name.
%
category: 'initialize'
set compile_env: 0
method: StatStatistic
name: anObject
	name := anObject
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
offset

	^offset.
	
%
category: 'initialize'
set compile_env: 0
method: StatStatistic
offset: anObject
	offset := anObject
%
category: 'printing'
set compile_env: 0
method: StatStatistic
printOn: aStream

	aStream nextPutAll: name.
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
process

	^process.
%
category: 'initialize'
set compile_env: 0
method: StatStatistic
process: anObject
	process := anObject
%
category: 'initialize'
set compile_env: 0
method: StatStatistic
processType: anObject
	processType := anObject
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
rate

	^data last - data first / process seconds.
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
statmon

	^process statmon.
	
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
type

	^type.
%
category: 'accessors'
set compile_env: 0
method: StatStatistic
units

	^units.
	
%
category: 'initialize'
set compile_env: 0
method: StatStatistic
updateFrom: anArray

	| value |
	offset <= 3 ifTrue: [^self].
	anArray size < offset ifTrue: [^self].
	value := self valueOf: (anArray at: offset).
	data add: value.
	data last < min ifTrue: [
		min := value.
		minTime := anArray at: 2.
	].
	max < data last ifTrue: [
		max := value.
		maxTime := anArray at: 2.
	].
	sum := sum + value.
%
category: 'initialize'
set compile_env: 0
method: StatStatistic
valueOf: anObject

	| bytes |
	type == #'float' ifTrue: [
		bytes := ByteArray new: 4.
		bytes unsigned32At: 1 put: anObject.
		^bytes floatAt: 1.
	].
	(type == #'svalue' and: [16rEFFFFFFF < anObject]) ifTrue: [
		bytes := ByteArray new: 4.
		bytes unsigned32At: 1 put: anObject.
		^bytes signed32At: 1.
	].
	(type == #'uvalue' and: [anObject < 0]) ifTrue: [
		bytes := ByteArray new: 4.
		bytes signed32At: 1 put: anObject.
		^bytes unsigned32At: 1.
	].
	(type == #'counter' and: [anObject < 0]) ifTrue: [
		bytes := ByteArray new: 4.
		bytes signed32At: 1 put: anObject.
		^bytes unsigned32At: 1.
	].
	^anObject.
%
doit
StatStatistic category: 'Statmonitor'
%
