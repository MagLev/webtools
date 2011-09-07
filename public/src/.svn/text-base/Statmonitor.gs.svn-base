doit
Object subclass: 'Statmonitor'
  instVarNames: #( path level platform
                    gemStoneVersion machine time processTypes)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: WebTools
  options: #()

%

! Remove existing behavior from Statmonitor
doit
Statmonitor removeAllMethods.
Statmonitor class removeAllMethods.
%
! ------------------- Class methods for Statmonitor
category: 'cache'
set compile_env: 0
classmethod: Statmonitor
clearCache

	WebTools 
		removeKey: #'Statmon-Cache' 				ifAbsent: [];
		yourself.
%
category: 'cache'
set compile_env: 0
classmethod: Statmonitor
fileAt: aString

	^self files
		detect: [:each | each path = aString]
		ifNone: [self error: 'File not found!'].
%
category: 'cache'
set compile_env: 0
classmethod: Statmonitor
files

	^WebTools 
		at: #'Statmon-Cache' 
		ifAbsentPut: [IdentitySet new].
%
category: 'Instance Creation'
set compile_env: 0
classmethod: Statmonitor
path: aString

	(self files anySatisfy: [:each | each path = aString]) ifTrue: [^self].
	(GsFile existsOnServer: aString) ifFalse: [self error: aString printString , ' not found on server'].
	^self basicNew
		initialize: aString;
		yourself.
%
category: 'cache'
set compile_env: 0
classmethod: Statmonitor
remove: aStatmonitor

	self files remove: aStatmonitor.
	
%
! ------------------- Instance methods for Statmonitor
category: 'initialize'
set compile_env: 0
method: Statmonitor
atEnd

	^self streamCache isEmpty and: [self stream atEnd].
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
commit

	System commitTransaction ifFalse: [self error: 'Commit failed!'].
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
doAnalysis

	self
		findCRB;
		yourself.
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
findCRB

	| type stone oldestCrSession timeList dict commitRecordCount |
	type := Statmonitor files asArray first processTypeList detect: [:each | each isStone].
	stone := type processes first.
	oldestCrSession := stone statistics 
		detect: [:each | each name = 'OldestCrSession']
		ifNone: [^self].
	timeList := oldestCrSession process timeList.
	commitRecordCount := stone statistics detect: [:each | each name = 'CommitRecordCount'].
	dict := IntegerKeyValueDictionary new.
	1 to: oldestCrSession data size do: [:i | 
		| session time crb |
		session := oldestCrSession data at: i.
		time := timeList at: i.
		crb := commitRecordCount data at: i.
		0 < session ifTrue: [
			| list |
			list := dict at: session ifAbsentPut: [Array new].
			list add: time -> crb.
		].
	].
	type := Statmonitor files asArray first processTypeList detect: [:each | each isGem].
	type processes do: [:eachGem | 
		| crbData |
		crbData := dict at: eachGem sessionID ifAbsent: [#()].
		crbData do: [:eachAssoc | 
			| index |
			index := eachGem timeList indexOf: eachAssoc key.
			0 < index ifTrue: [
				eachGem crb add: eachAssoc.
			].
		].
	].
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
gemStoneVersion

	^gemStoneVersion.
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
headerAt: aString

	| line |
	(line := self headerLine) key = aString ifFalse: [self error: aString , ' not found in header'].
	^line value.
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
headerLine

	| line key value |
	line := self nextLine subStrings: $".
	key := ((line at: 1) subStrings: $=) first trimBlanks.
	value := line at: 2.
	^key -> value.

%
category: 'initialize'
set compile_env: 0
method: Statmonitor
initialize: aString

	| myStream |
	path := aString.
	self class files add: self.
	self commit.
	myStream := (aString copyFrom: aString size - 2 to: aString size) = '.gz'
		ifTrue: [GsFile openReadOnServerCompressed: aString]
		ifFalse: [GsFile openReadOnServer: aString].
	self stream: myStream.
	self streamCache size: 0.
	[
		self
			readHeader;
			readData;
			doAnalysis;
			yourself.
	] ensure: [
		myStream close.
		self stream: nil.
	].
	self commit.
%
category: 'public'
set compile_env: 0
method: Statmonitor
jsonKeys

	^#(path level platform gemStoneVersion machine time).
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
level

	^level.
	
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
machine

	^machine.
	
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
nextLine

	^self upTo: Character lf.
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
nextRow

	| line list listIndex value i j string |
	(line := self nextLine) isEmpty ifTrue: [^#()].
	i := line indexOf: $  startingAt: 1.
	i := line indexOf: $  startingAt: i + 1.
	j := line indexOf: $  startingAt: i + 1.
	string := line copyFrom: i + 1 to: j - 1.
	list := Array new.
	listIndex := 1.
	value := 0.
	1 to: line size do: [:i | 
		| char |
		(char := line at: i) == $  ifTrue: [
			list at: listIndex put: value.
			listIndex := listIndex + 1.
			value := 0.
		] ifFalse: [
			value := value * 10 + char codePoint - 48 "codePoint for zero"
		].
	].
	list add: value.
	list at: 3 put: string.
	^list.
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
nextRowA

	| line list listIndex value |
	(line := self nextLine) isEmpty ifTrue: [^#()].
	list := Array new.
	listIndex := 1.
	value := 0.
	1 to: line size do: [:i | 
		| char |
		(char := line at: i) == $  ifTrue: [
			list at: listIndex put: value.
			listIndex := listIndex + 1.
			value := listIndex == 3 ifTrue: [String new] ifFalse: [0].
		] ifFalse: [
			listIndex == 3 
				ifTrue: [value add: char]
				ifFalse: [value := value * 10 + char codePoint - 48 "codePoint for zero"]
		].
	].
	list add: value.
	^list.
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
path

	^path.
	
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
platform

	^platform.
	
%
category: 'public'
set compile_env: 0
method: Statmonitor
printOn: aStream

	aStream nextPutAll: path.
	
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
processes

	| list |
	list := OrderedCollection new.
	self processTypeList do: [:eachType | 
		eachType processes do: [:each | 
			list add: each.
		].
	].
	^list.
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
processesWithNonZero: aString

	^self processes select: [:each | 
		| stat |
		stat := each statNamed: aString.
		stat notNil and: [0 < stat max].
	].
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
processTypeList

	^processTypes asSortedCollection: [:a :b | a id <= b id].
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
processWithPid: anInteger

	^self processes
		detect: [:each | each processID == anInteger]
		ifNone: [nil].
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
readData

	| i |
	i := 0.
	[
		self atEnd not.
	] whileTrue: [
		| list |
		(i := i + 1) \\ 500 == 0 ifTrue: [self commit].
		i \\ 100 == 0 ifTrue: [Processor yield].
		(list := self nextRow) notEmpty ifTrue: [
			(processTypes at: list first) summarizeLine: list.
		].
	].
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
readHeader

	| line list flag |
	level					:= self headerAt: 'STATMON'.
	platform 			:= self headerAt: 'Platform'.
	gemStoneVersion := self headerAt: 'GemStoneVersion'.
	machine 			:= self headerAt: 'Machine'.
	time 					:= self headerAt: 'Time'.
	(line := self nextLine) = 'StatTypes = [' ifFalse: [self error: 'StatTypes not found'].
	list := (self upTo: $]) subStrings: $,.
	flag := self atEnd.
	list := list reject: [:each | each isEmpty].
	list := list collect: [:each | 
		StatProcessType 
			fromString: each
			statmon: self.
	].
	processTypes := Dictionary new.
	list do: [:each | 
		processTypes 
			at: each id
			put: each.
	].
	self 
		nextLine;		"]"
		nextLine;		"%%%%Generated by the statistics monitor"
		nextLine;		"ENDHEADER"
		yourself.
%
category: 'public'
set compile_env: 0
method: Statmonitor
remove

	self class remove: self.
	
%
category: 'private'
set compile_env: 0
method: Statmonitor
sanitize

	path := path copyFrom: path size - 20 to: path size.
	machine := 'hamburg sun4u (Solaris 2.9 Generic_118558-28)'.
	time := (time copyFrom: 1 to: 25) , 'PST'.
	processTypes do: [:each | each sanitize].
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
sharedPageCacheMonitor

	^(processTypes detect: [:each | each isSharedPageCacheMonitor]) processes first.
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
stone

	^(processTypes detect: [:each | each isStone]) processes first.
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
stream

	^SessionTemps current at: #'Statmonitor-stream'.
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
stream: aStream

	SessionTemps current
		at: #'Statmonitor-stream'
		put: aStream.
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
streamCache

	^SessionTemps current 
		at: #'Statmonitor-streamCache'
		ifAbsentPut: [String new].
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
time

	^time.
	
%
category: 'public'
set compile_env: 0
method: Statmonitor
timeStampFromSeconds: anInteger

	^DateAndTime 
		posixSeconds: anInteger 
		offset: (Duration seconds: self timeZoneOffset).
%
category: 'accessors'
set compile_env: 0
method: Statmonitor
timeZoneOffset

	| code |
	code := time copyFrom: time size - 2 to: time size.
	code = 'ART' ifTrue: [^-10800].	"3 hours"
	^-28800	"8 hours"
%
category: 'initialize'
set compile_env: 0
method: Statmonitor
upTo: aCharacter

	| cache i result stream |
	cache := self streamCache.
	0 < (i := cache indexOf: aCharacter) ifTrue: [
		result := cache copyFrom: 1 to: i - 1.
		i == cache size ifTrue: [
			cache size: 0.
		] ifFalse: [
			cache copyFrom: i + 1 to: cache size into: cache startingAt: 1.
			cache size: cache size - i.
		].
		^result.
	].
	(stream := self stream) atEnd ifTrue: [
		result := cache copy.
		cache size: 0.
		^result.
	].
	cache addAll: (stream next: 15000).
	^self upTo: aCharacter.
%
doit
Statmonitor category: 'Statmonitor'
%
