doit
Tool subclass: 'Workspace'
	instVarNames: #( cache)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: WebTools
	options: #()

%

! Remove existing behavior from Workspace
doit
Workspace removeAllMethods.
Workspace class removeAllMethods.
%
! ------------------- Class methods for Workspace
category: 'other'
set compile_env: 0
classmethod: Workspace
description

	^'Smalltalk Workspace'.
%
category: 'other'
set compile_env: 0
classmethod: Workspace
displayName

	^'Workspace'.
%
category: 'other'
set compile_env: 0
classmethod: Workspace
sortOrder

	^5.
%
! ------------------- Instance methods for Workspace
category: 'other'
set compile_env: 0
method: Workspace
deleteProcess

	| objectLogEntryClass log entry |
	objectLogEntryClass := System myUserProfile symbolList objectNamed: #'ObjectLogEntry'.
	objectLogEntryClass isNil ifTrue: [^Dictionary new].
	objectLogEntryClass acquireObjectLogLock.
	log := objectLogEntryClass objectLog.
	entry := Object _objectForOop: (arguments at: 'oop') asNumber.
	log remove: entry ifAbsent: [].
	^Dictionary new.
%
category: 'other'
set compile_env: 0
method: Workspace
evaluate

	| semaphoreClass semaphore gsProcess result |
	semaphoreClass := System myUserProfile symbolList objectNamed: #'TransientSemaphore'.
	semaphoreClass isNil ifTrue: [semaphoreClass := Semaphore].
	semaphore := semaphoreClass new.
	result := Dictionary new.
	gsProcess := [
		[
			[
				| value |
				value := (arguments at: 'text') evaluate.
				result 
					at: 'klass' 	put: value class name;
					at: 'string'	put: value printString;
					yourself.
				(value isKindOf: Class) ifTrue: [
					result
						at: 'dict' 	put: (GsSession currentSession symbolList detect: [:each | each includes: value] ifNone: [UserGlobals]) name;
						at: 'name' 	put: value name;
						at: 'cat'		put: value category;
						yourself.
				].
			] on: CompileError do: [:ex | 
				ex printString.		"workaround for bug"
				result
					at: 'errorType' put: 'compileError';
					at: 'errorDetails' put: ex gsArguments first;
					yourself.
				ex return.
			].
		] on: Error , ControlInterrupt do: [:ex | 
			| debuggerLogEntryClass description entry |
			ex printString.		"workaround for bug"
			description := ex description.
			debuggerLogEntryClass := System myUserProfile symbolList objectNamed: #'DebuggerLogEntry'.
			debuggerLogEntryClass isNil ifTrue: [ex return].
			entry := debuggerLogEntryClass error: description object: ex.
			debuggerLogEntryClass createContinuationFor: entry.
			result 
				at: 'errorType'		put: ex class name;
				at: 'description' 	put: description;
				at: 'oop'				put: entry asOop;
				yourself.
			ex return.
		].
		semaphore signal.
	] forkAt: Processor userBackgroundPriority.
	semaphore wait.
	^result.
%
category: 'other'
set compile_env: 0
method: Workspace
initialize

	cache := IntegerKeyValueDictionary new.		"Prevent GC of these objects!"
%
category: 'other'
set compile_env: 0
method: Workspace
json

	^Dictionary new
		yourself.
%
category: 'other'
set compile_env: 0
method: Workspace
saveMethod

	| dict klass category source method warnings |
	dict := (arguments at: 'dict') asSymbol.
	dict := System myUserProfile symbolList detect: [:each | each name == dict].
	klass := (arguments at: 'klass') asSymbol.
	klass := dict at: klass.
	(arguments at: 'isMeta') ifTrue: [klass := klass class].
	category := arguments at: 'category'.
	category := category isNil
		ifTrue: [#'undefined']
		ifFalse: [category asSymbol].
	source := arguments at: 'source'.
	[
		[
			method := klass
				compileMethod: source
				dictionaries: System myUserProfile symbolList
				category: category
				environmentId: 0.
		] on: CompileError do: [:ex | 
			ex printString.		"workaround for bug"
			^Dictionary new
				at: 'compileError' put: ex gsArguments first;
				yourself.
		].
	] on: CompileWarning do: [:ex |
		warnings := ex gsArguments first.
		ex resume.
	].
	^Dictionary new
		at: 'selector' put: method selector;
		at: 'warnings' put: warnings;
		yourself.
%
doit
Workspace category: 'WebTools'
%
