doit
Tool subclass: 'CodeBrowser'
	instVarNames: #( dictionary package classCategory
	                  klass superList superClass methodFilter
	                  selector implementor)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: WebTools
	options: #()

%

! Remove existing behavior from CodeBrowser
doit
CodeBrowser removeAllMethods.
CodeBrowser class removeAllMethods.
%
! ------------------- Class methods for CodeBrowser
category: 'other'
set compile_env: 0
classmethod: CodeBrowser
description

	^'Browse SymbolDictionaries, Classes, and Methods'.
%
category: 'other'
set compile_env: 0
classmethod: CodeBrowser
displayName

	^'Code Browser'.
%
category: 'other'
set compile_env: 0
classmethod: CodeBrowser
sortOrder

	^3.
%
! ------------------- Instance methods for CodeBrowser
category: 'Public'
set compile_env: 0
method: CodeBrowser
json

	^Dictionary new
		at: 'response'			put: self _response;
		at: 'dictList' 			put: self _dictList;
		at: 'packageList'		put: self _packageList;
		at: 'classCatList'		put: self _classCategoryList;
		at: 'classList'			put: self _classList;
		at: 'classDef'			put: self _classDefinition;
		at: 'superList'			put: self _superList;
		at: 'methodFilterList'	put: self _methodFilterList;
		at: 'methodList'		put: self _methodList;
		at: 'implList'			put: self _implementorList;
		at: 'method'			put: self _method;
		yourself.
%
category: 'Public'
set compile_env: 0
method: CodeBrowser
removeClass

	| dict name |
	dict := (arguments at: 'dict') asSymbol.
	name := (arguments at: 'klass') asSymbol.
	dict := System myUserProfile symbolList 
		detect: [:each | each name == dict]
		ifNone: [^Dictionary new 
			at: 'error' put: 'Dictionary not found!';
			yourself].
	dict
		removeKey: name
		ifAbsent: [^Dictionary new 
			at: 'error' put: 'Class not found!';
			yourself].
	^Dictionary new.
%
category: 'Public'
set compile_env: 0
method: CodeBrowser
removeMethod

	dictionary := (arguments at: 'dict') asSymbol.
	dictionary := System myUserProfile symbolList detect: [:each | each name == dictionary].
	klass := (arguments at: 'klass') asSymbol.
	klass := dictionary at: klass.
	(arguments at: 'isMeta') ifTrue: [klass := klass class].
	klass removeSelector: (arguments at: 'name') asSymbol.
	^Dictionary new.
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_classCategoryList

	| selected set |
	classCategory := nil.
	dictionary isNil ifTrue: [^#()].
	selected := arguments at: 'classCat' ifAbsent: [nil].
	set := Set new.
	dictionary do: [:each | 
		each isClass ifTrue: [
			| category |
			category := each category.
			set add: category.
			(category notNil and: [
			category = selected or: [
			selected size <= category size and: [
			selected = (category copyFrom: 1 to: selected size)]]]) ifTrue: [
				classCategory := selected.
			].
		].
	].
	^set asSortedCollection.
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_classDefinition

	^klass notNil 
		ifTrue: [klass thisClass definition] 
		ifFalse: [
^'Object subclass: ''MyNewClass''
	instVarNames: #( instVar1 instVar2 )
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ' , (dictionary isNil ifTrue: ['UserGlobals'] ifFalse: [dictionary name]) , '
	options: #()'].
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_classList

	| selected list |
	klass := nil.
	dictionary isNil ifTrue: [^#()].
	selected := arguments at: 'klass' ifAbsent: [nil].
	selected notNil ifTrue: [selected := selected asSymbol].
	list := dictionary select: [:each | 
		each isClass and: [
		| category |
		each name = selected ifTrue: [klass := each].
		category := each category.
		classCategory isNil or: [category = classCategory or: [
		(category isKindOf: String) and: [(classCategory isKindOf: String) and: [
		classCategory size <= category size and: [
		classCategory = (category copyFrom: 1 to: classCategory size)]]]]]]
	]. 
	(arguments at: 'isMeta' ifAbsent: [false]) ifTrue: [klass := klass class].
	^(list collect: [:each | each name]) asSortedCollection.
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_dictList

	| selected list |
	dictionary := nil.
	(arguments at: 'isDictsTab') ifFalse: [^#()].
	selected := arguments at: 'dict' ifAbsent: [nil].
	list := System myUserProfile symbolList.
	dictionary := list
		detect: [:each | each name asString = selected]
		ifNone: [nil].
	^list collect: [:each | each name].
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_implementorList

	| selected list |
	selected := arguments at: 'implementor' ifAbsent: [nil].
	implementor := nil.
	superList isEmpty ifTrue: [^#()].
	list := superList select: [:each | each selectors includes: selector].
	implementor := list 
		detect: [:each | each name asString = selected]
		ifNone: [superList last].
	^list collect: [:each | each name].
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_instanceVariableList

	| selected list |
	selected := arguments at: 'methodFilter' ifAbsent: [nil].
	selected notNil ifTrue: [selected := selected asSymbol].
	methodFilter := nil.
	superList isEmpty ifTrue: [^#()].
	list := klass allInstVarNames.
	(list includes: selected) ifTrue: [methodFilter := selected].
	^list.
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_method

	| method |
	implementor isNil ifTrue: [^nil].
	selector isNil ifTrue: [^nil].
	method := implementor compiledMethodAt: selector.
	^Dictionary new
		at: 'dictionaryName' put: dictionary name;
		at: 'className' put: klass thisClass name;
		at: 'isMeta' put: klass isMeta;
		at: 'category' put: (klass categoryOfSelector: selector);
		at: 'source' put: method sourceString;
		at: 'stepPoints' put: method _sourceOffsets;
		at: 'sends' put: method _sourceOffsetsOfSends;
		yourself.
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_methodCategoryList

	| selected list |
	selected := arguments at: 'methodFilter' ifAbsent: [nil].
	selected notNil ifTrue: [selected := selected asSymbol].
	methodFilter := nil.
	superList isEmpty ifTrue: [^#()].
	list := Set new.
	(superList indexOf: superClass)
		to: superList size
		do: [:i | list addAll: (superList at: i) categoryNames].
	list := list asSortedCollection.
	(list includes: selected) ifTrue: [methodFilter := selected].
	^list.
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_methodFilterList

	| filterBy |
	filterBy := arguments at: 'methodFilterBy'.
	filterBy = 'category' ifTrue: [^self _methodCategoryList].
	filterBy = 'variable' ifTrue: [^self _instanceVariableList].
	^#().
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_methodList

	| selected set |
	selected := arguments at: 'selector' ifAbsent: [nil].
	selected notNil ifTrue: [selected := selected asSymbol].
	selector := nil.
	superList isEmpty ifTrue: [^#()].
	set := Set new.
	(superList indexOf: superClass)
		to: superList size
		do: [:i | 
			| behavior selectors |
			behavior := superList at: i.
			selectors := methodFilter isNil
				ifTrue: [behavior selectors]
				ifFalse: [self _methodListFor: behavior].
			set addAll: selectors.
		].
	(set includes: selected) ifTrue: [selector := selected].
	^set asSortedCollection.
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_methodListByCategoryFor: aBehavior

	^(aBehavior categoryNames includes: methodFilter)
		ifTrue: [aBehavior selectorsIn: methodFilter]
		ifFalse: [#()].
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_methodListByVariableFor: aBehavior

	| symbol |
	symbol := methodFilter asSymbol.
	^aBehavior selectors asSortedCollection asArray select: [:each | 
		(aBehavior compiledMethodAt: each) instVarsAccessed includes: symbol.
	].
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_methodListFor: aBehavior

	| filterBy |
	filterBy := arguments at: 'methodFilterBy'.
	filterBy = 'category' ifTrue: [^self _methodListByCategoryFor: aBehavior].
	filterBy = 'variable' ifTrue: [^self _methodListByVariableFor: aBehavior].
	^#().
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_packageList

	| mcWorkingCopyClass selected list workingCopy |
	package := nil.
	(arguments at: 'isDictsTab') ifTrue: [^#()].
	mcWorkingCopyClass := GsSession currentSession objectNamed: #'MCWorkingCopy'.
	mcWorkingCopyClass isNil ifTrue: [^#()].
	selected := arguments at: 'mcPackage' ifAbsent: [nil].
	list := mcWorkingCopyClass allManagers.
	list := list collect: [:each | 
		| name |
		name := each packageName.
		name = selected ifTrue: [package := each].
		name.
	].
	workingCopy := mcWorkingCopyClass allManagers 
		detect: [:each | each package name = selected]
		ifNone: [nil].
	dictionary := nil.
	workingCopy notNil ifTrue: [
		dictionary := SymbolDictionary new.
		dictionary at: #'UserGlobals' put: dictionary.
		workingCopy package packageInfo classes do: [:each | dictionary at: each name put: each].
	].
	^list asSortedCollection.
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_response

	^(arguments at: 'selector' ifAbsent: [nil]) notNil ifTrue: [
		nil.
	] ifFalse: [
		self _responseForClass.
	].
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_responseForClass

	| request result symbolList dict response |
	(request := arguments at: 'request' ifAbsent: [^nil]) isNil ifTrue: [^nil].
	result := [
		request evaluate.
	] on: Error do: [:ex | 
		| error |
		error := Array new
			add: ex class name;
			add: ex description;
			yourself.
		(ex isKindOf: CompileError) ifTrue: [
			error add: ex gsArguments.
		].
		^Dictionary new
			at: 'action'		put: 'klass';
			at: 'error'			put: error;
			yourself.
	].
	(result isKindOf: Class) ifTrue: [
		symbolList := GsSession currentSession symbolList.
		dict := symbolList
			detect: [:each | each includes: result]
			ifNone: [^nil].
		response := Dictionary new
			at: 'dict' 			put: dict name asString;
			at: 'classCat'		put: nil;
			at: 'klass'			put: result name asString;
			at: 'isMeta'		put: false;
			at: 'superClass'	put: result name asString;
			at: 'methodCat'		put: nil;
			at: 'selector'		put: nil;
			at: 'implementor'	put: nil;
			yourself.
		response keysAndValuesDo: [:key :value | 
			arguments at: key put: value.
		].
		^response
			at: 'action'		put: 'klass';
			yourself.
	].
	^nil.
%
category: 'Private'
set compile_env: 0
method: CodeBrowser
_superList

	| selected nextClass |
	superList := Array new.
	superClass := nil.
	klass isNil ifTrue: [^#()].
	selected := arguments at: 'superClass' ifAbsent: [klass name].
	nextClass := klass.
	[
		nextClass notNil.
	] whileTrue: [
		superList add: nextClass.
		nextClass name asString = selected ifTrue: [superClass := nextClass].
		nextClass := nextClass superclass.
	].
	superList := superList reverse.
	superClass isNil ifTrue: [superClass := superList last].
	^superList collect: [:each | each name].
%
doit
CodeBrowser category: 'WebTools'
%
