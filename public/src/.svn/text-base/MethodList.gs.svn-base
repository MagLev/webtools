doit
Tool subclass: 'MethodList'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: WebTools
  options: #()

%

! Remove existing behavior from MethodList
doit
MethodList removeAllMethods.
MethodList class removeAllMethods.
%
! ------------------- Class methods for MethodList
! ------------------- Instance methods for MethodList
category: 'Public'
set compile_env: 0
method: MethodList
json

	| type |
	type := arguments at: 'type'.
	type = 'method' ifTrue: [^self _method].
	type = 'implementors' ifTrue: [^self _implementors].
	type = 'senders' ifTrue: [^self _senders].
	type = 'referencesToGlobal' ifTrue: [^self _referencesToGlobal].
	^Dictionary new
		at: 'a'		put: 'AAA';
		yourself.
%
category: 'Public'
set compile_env: 0
method: MethodList
_implementors

	| name list |
	name := arguments at: 'find' ifAbsent: [^Dictionary new].
	list := ClassOrganizer new implementorsOf: name asSymbol.
	^self _methods: list.
%
category: 'Public'
set compile_env: 0
method: MethodList
_method

	| symbolList name dictionary isMeta class method |
	symbolList := System myUserProfile symbolList.
	name := (arguments at: 'dict') asSymbol.
	dictionary := symbolList 
		detect: [:each | each name = name]
		ifNone: [^Dictionary new].
	name := arguments at: 'klass'.
	(isMeta := name includesString: ' class') ifTrue: [
		name := name copyFrom: 1 to: name size - 6.
	].
	name := name asSymbol.
	class := dictionary 
		at: name 
		ifAbsent: [^Dictionary new].
	isMeta ifTrue: [class := class class].
	method := class compiledMethodAt: (arguments at: 'selector') asSymbol.
	^Dictionary new
		at: 'dictionaryName' put: dictionary name;
		at: 'className' put: class thisClass name;
		at: 'isMeta' put: class isMeta;
		at: 'source' put: method sourceString;
		at: 'stepPoints' put: method _sourceOffsets;
		at: 'sends' put: method _sourceOffsetsOfSends;
		yourself.
%
category: 'Public'
set compile_env: 0
method: MethodList
_methods: aList

	| symbolList list |
	symbolList := System myUserProfile symbolList.
	list := aList collect: [:each | 
		| array dict klass |
		klass := each inClass.
		array := symbolList dictionaryAndSymbolOf: klass thisClass.
		dict := array isNil
			ifTrue: [nil]
			ifFalse: [array first name].
		Dictionary new
			at: 'dict' 		put: dict;
			at: 'klassCat'	put: klass category;
			at: 'klass' 		put: klass name;
			at: 'category' 	put: (klass categoryOfSelector: each selector);
			at: 'selector' 	put: each selector;
			yourself.
	].
	^Dictionary new
		at: 'list' put: list;
		yourself.
%
category: 'Public'
set compile_env: 0
method: MethodList
_referencesToGlobal

	| symbolList name dictionary global list |
	symbolList := System myUserProfile symbolList.
	name := (arguments at: 'dict') asSymbol.
	dictionary := symbolList 
		detect: [:each | each name = name]
		ifNone: [^Dictionary new].
	name := (arguments at: 'find') asSymbol.
	global := dictionary 
		at: name 
		ifAbsent: [^Dictionary new].
	list := ClassOrganizer new referencesToObject: global.
	^self _methods: list.
%
category: 'Public'
set compile_env: 0
method: MethodList
_senders

	| name list |
	name := arguments at: 'find' ifAbsent: [^Dictionary new].
	list := ClassOrganizer new sendersOf: name asSymbol.
	^self _methods: list first.
%
doit
MethodList category: 'WebTools'
%
