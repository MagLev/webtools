doit
Object subclass: 'Tool'
	instVarNames: #( arguments)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: WebTools
	options: #()

%

! Remove existing behavior from Tool
doit
Tool removeAllMethods.
Tool class removeAllMethods.
%
! ------------------- Class methods for Tool
category: 'other'
set compile_env: 0
classmethod: Tool
arguments: aDictionary

	^self basicNew
		initialize: aDictionary;
		yourself.
%
category: 'other'
set compile_env: 0
classmethod: Tool
description

	^''.
%
category: 'other'
set compile_env: 0
classmethod: Tool
displayName

	^self name.
%
category: 'other'
set compile_env: 0
classmethod: Tool
fileName

	^self name , '.html'.
%
category: 'other'
set compile_env: 0
classmethod: Tool
new

	self error: 'Should use #arguments:'.
%
category: 'other'
set compile_env: 0
classmethod: Tool
sortOrder

	^nil.
%
! ------------------- Instance methods for Tool
category: 'other'
set compile_env: 0
method: Tool
initialize
	"override as needed"
%
category: 'other'
set compile_env: 0
method: Tool
initialize: aDictionary

	arguments := aDictionary.
	self initialize.
%
category: 'other'
set compile_env: 0
method: Tool
json

	self subclassResponsibility.
%
doit
Tool category: 'WebTools'
%
