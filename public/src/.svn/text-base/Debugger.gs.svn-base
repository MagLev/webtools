doit
Tool subclass: 'Debugger'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: WebTools
	options: #()

%

! Remove existing behavior from Debugger
doit
Debugger removeAllMethods.
Debugger class removeAllMethods.
%
! ------------------- Class methods for Debugger
! ------------------- Instance methods for Debugger
category: 'Public'
set compile_env: 0
method: Debugger
frame

	| result entry process frameData |
	result := Dictionary new.
	(entry := self _objectLogEntry) isNil ifTrue: [^result].
	(process := entry continuation) isNil ifTrue: [^result].
	(frameData := process _localFrameContentsAt: (arguments at: 'frame') asNumber) isNil ifTrue: [^result].
	^result 
		at: 'method'	put: (self _methodFromFrameData: frameData);
		at: 'variables'	put: (self _variablesFromFrameData: frameData);
		yourself.
%
category: 'Public'
set compile_env: 0
method: Debugger
json

	| entry process stack |
	(entry := self _objectLogEntry) isNil ifTrue: [^Dictionary new].
	(process := entry continuation) isNil ifTrue: [^Dictionary new].
	stack := process _reportOfSize: process stackDepth.
	^Dictionary new
		at: 'label'	put: entry label;
		at: 'stack' 	put: stack;
		yourself.
%
category: 'Public'
set compile_env: 0
method: Debugger
step

	^Dictionary new
		at: 'foo' put: 'bar';
		yourself.
%
category: 'Public'
set compile_env: 0
method: Debugger
_methodFromFrameData: anArray

	| result method inClass |
	result := Dictionary new.
	(method := anArray at: 1) isNil ifTrue: [^result].
	result
		at: 'source' 		put: method sourceString;
		at: 'stepPoints' 	put: method _sourceOffsets;
		at: 'sends' 			put: method _sourceOffsetsOfSends;
		at: 'nowAt' 		put: (method homeMethod _stepPointForMeth: method ip: (anArray at: 2));
		yourself.
	(inClass := method inClass) notNil ifTrue: [
		| dictionary |
		dictionary := System myUserProfile symbolList 
			detect: [:each | each includes: inClass thisClass] 
			ifNone: [nil].
		dictionary notNil ifTrue: [dictionary := dictionary name].
		result
			at: 'dictionaryName' put: dictionary;
			at: 'className' put: inClass thisClass name;
			at: 'isMeta' put: inClass isMeta;
			yourself.
	].
	^result.
%
category: 'Public'
set compile_env: 0
method: Debugger
_objectLogEntry

	| objectLogEntryClass oop |
	objectLogEntryClass := System myUserProfile symbolList objectNamed: #'ObjectLogEntry'.
	objectLogEntryClass isNil ifTrue: [^nil].
	oop := (arguments at: 'oop') asNumber.
	^objectLogEntryClass objectLog
		detect: [:each | each asOop = oop]
		ifNone: [nil].
%
category: 'Public'
set compile_env: 0
method: Debugger
_variablesFromFrameData: anArray

	| list names |
	list := Array new.
	list add: (Dictionary new
		at: 'name' put: 'self';
		at: 'string' put: (anArray at: 8) printString;
		at: 'oop' put: (anArray at: 8) asOop;
		yourself).
	((anArray at: 8) ~~ (anArray at: 10)) ifTrue: [
		list add: (Dictionary new
			at: 'name' put: 'receiver';
			at: 'string' put: (anArray at: 10) printString;
			at: 'oop' put: (anArray at: 10) asOop;
			yourself).
	].
	names := anArray at: 9.
	1 to: names size do: [:i | 
		| name |
		(name := names at: i) first ~= $. ifTrue: [
			list add: (Dictionary new
				at: 'name' put: name;
				at: 'string' put: (anArray at: 10 + i) printString;
				at: 'oop' put: (anArray at: 10 + i) asOop;
				yourself).
		].
	].
	^list.
%
doit
Debugger category: 'WebTools'
%
