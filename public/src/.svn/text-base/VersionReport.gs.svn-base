doit
Tool subclass: 'VersionReport'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: WebTools
	options: #()

%

! Remove existing behavior from VersionReport
doit
VersionReport removeAllMethods.
VersionReport class removeAllMethods.
%
! ------------------- Class methods for VersionReport
category: 'other'
set compile_env: 0
classmethod: VersionReport
description

	^'Information about the Stone and Gem processes and their host(s)'.
%
category: 'other'
set compile_env: 0
classmethod: VersionReport
displayName

	^'Version Report'.
%
category: 'other'
set compile_env: 0
classmethod: VersionReport
sortOrder

	^1.
%
! ------------------- Instance methods for VersionReport
category: 'other'
set compile_env: 0
method: VersionReport
json

	^Dictionary new
		at: 'stone' put: System stoneVersionReport;
		at: 'gem' put: System gemVersionReport;
		yourself.
%
doit
VersionReport category: 'WebTools'
%
