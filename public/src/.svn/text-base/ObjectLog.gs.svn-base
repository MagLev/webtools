doit
Tool subclass: 'ObjectLog'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: WebTools
	options: #()

%

! Remove existing behavior from ObjectLog
doit
ObjectLog removeAllMethods.
ObjectLog class removeAllMethods.
%
! ------------------- Class methods for ObjectLog
category: 'other'
set compile_env: 0
classmethod: ObjectLog
description

	^'Object Log'.
%
category: 'other'
set compile_env: 0
classmethod: ObjectLog
displayName

	^'Object Log'.
%
category: 'other'
set compile_env: 0
classmethod: ObjectLog
sortOrder

	^6.
%
! ------------------- Instance methods for ObjectLog
category: 'Public'
set compile_env: 0
method: ObjectLog
json

	| objectLogEntryClass list |
	objectLogEntryClass := System myUserProfile symbolList objectNamed: #'ObjectLogEntry'.
	objectLogEntryClass isNil ifTrue: [^Dictionary new].
	list := objectLogEntryClass objectLog reverse collect: [:each | 
		| label objectString |
		label := each label.
		label isNil ifTrue: [label := ''].
		(label isKindOf: String) ifFalse: [label := label printString].
		(label first = $' and: [label last = $']) ifTrue: [label := label copyFrom: 2 to: label size - 1].
		objectString := each objectString.
		label = objectString ifTrue: [label := label subStrings first].
		Dictionary new
			at: 'oop'		put: each asOop;
			at: 'stamp'	put: each stampString;
			at: 'pid'		put: each pidString;
			at: 'label'	put: label;
			at: 'type'	put: (#('Fatal' 'Error' 'Warn' 'Info' 'Debug' 'Trace' 'Transcript') at: each priority);
			at: 'tag'		put: (each hasTag ifTrue: [each tag] ifFalse: ['']);
			at: 'object'	put: objectString;
			at: 'hasContinuation'		put: each hasContinuation;
			yourself.
	].
	^Dictionary new
		at: 'list'		put: list;
		yourself.
%
doit
ObjectLog category: 'WebTools'
%
