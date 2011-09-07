doit
Object subclass: 'Server'
	instVarNames: #( server socket resultCode
	                  httpHeaders stream startTime)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: WebTools
	options: #()

%

! Remove existing behavior from Server
doit
Server removeAllMethods.
Server class removeAllMethods.
%
! ------------------- Class methods for Server
category: 'other'
set compile_env: 0
classmethod: Server
run
"
	Server run.
"
	self startServerAtPort: 8080.
%
category: 'other'
set compile_env: 0
classmethod: Server
runInForeground
"
	Server runInForeground.
"
	self new startForegroundServerAtPort: 8080.
%
category: 'other'
set compile_env: 0
classmethod: Server
startServerAtPort: anInteger
"
	If anInteger is nil then assign a random port.
	Server startServerAtPort: 8080.
"
	| delay |
	delay := Delay forSeconds: 5.
	GsFile stdout nextPutAll: (self new startServerAtPort: anInteger); cr; flush.
	[System commitTransaction] whileTrue: [delay wait].
%
! ------------------- Instance methods for Server
category: 'Request Handler'
set compile_env: 0
method: Server
beNoCache

	httpHeaders
		at: 'Cache-Control'			
		put: 'no-cache'.

%
category: 'Request Handler'
set compile_env: 0
method: Server
contentType: aString

	httpHeaders
		at: 'Content-Type'
		put: aString.

%
category: 'Request Handler'
set compile_env: 0
method: Server
contentTypes

	^KeyValueDictionary new
		at: 'css'		put: 'text/css';
		at: 'gif'		put: 'image/gif';
		at: 'html'	put: 'text/html; charset=UTF-8';
		at: 'ico'		put: 'image/x-icon';
		at: 'js'		put: 'text/javascript';
		at: 'png'		put: 'image/png';
		at: 'json'	put: 'text/json';
		yourself.
%
category: 'Request Handler'
set compile_env: 0
method: Server
doAnswer

	| string |
	string := stream contents.
	stream := (WriteStream on: String new)
		nextPutAll: 'HTTP/1.1 ';
		nextPutAll: resultCode printString; space;
		nextPutAll: self reasonPhrase; lf;
		yourself.
	httpHeaders
		at: 'Content-Length'			
		put: string size printString.
	httpHeaders keys asSortedCollection do: [:each | 
		stream 
			nextPutAll: each;
			nextPutAll: ': ';
			nextPutAll: (httpHeaders at: each);
			lf.
	].
	stream
		lf;
		nextPutAll: string; lf;
		yourself.
	socket writeWillNotBlock ifFalse: [self error: 'socket write will block'].
	string := stream contents.
	1 to: string size by: 10000 do: [:i | 
		| chunk numWritten |
		chunk := string copyFrom: i to: (i + 9999 min: string size).
		numWritten := socket write: chunk.
		numWritten == chunk size ifFalse: [
			socket close. 
			string := 'Tried to write ' , string size printString , ', but wrote ' , numWritten printString.
			GsFile stdout nextPutAll: string.
			self error: string.
			^self.
		].
	].
	socket close.
%
category: 'Request Handler'
set compile_env: 0
method: Server
encode: aString

	| readStream writeStream |
	readStream := ReadStream on: aString.
	writeStream := WriteStream on: String new.
	[
		readStream atEnd not.
	] whileTrue: [
		| next index |
		next := readStream next.
		index := #($" $& $' $< $>) indexOf: next.
		0 < index ifTrue: [
			writeStream nextPutAll: (#('&quot;' '&amp;' '&apos;' '&lt;' '&gt;') at: index).
		] ifFalse: [
			writeStream nextPut: next.
		].
	].
	^writeStream contents.

%
category: 'Request Handler'
set compile_env: 0
method: Server
errorExpectedGetOrPost

	resultCode := 404.
	self htmlWithBody: 'Expected a GET or POST request!'.


%
category: 'Request Handler'
set compile_env: 0
method: Server
errorExpectedHTTP11

	resultCode := 426.
	self htmlWithBody: 'Expected HTTP/1.1 support!'.

%
category: 'Request Handler'
set compile_env: 0
method: Server
errorNotFound: aString

	resultCode := 404.
	self htmlWithBody: aString printString , ' not Found!'.

%
category: 'Request Handler'
set compile_env: 0
method: Server
handleRequest
	"Called on a copy of the instance accepting connections"
	| readStream type string line headers contentLength path args selector class pieces gsFile |
	readStream := ReadStream on: (socket readString: 1000).
	type := readStream upTo: Character space.
	(type = 'GET' or: [type = 'POST']) ifFalse: [^self errorExpectedGetOrPost].
	path := readStream upTo: Character space.
	string := readStream upTo: Character cr.
	readStream peekFor: Character lf.
	string = 'HTTP/1.1' ifFalse: [^self errorExpectedHTTP11].
	headers := Dictionary new.
	[
		line := readStream upTo: Character cr.
		readStream peekFor: Character lf.
		line notEmpty.
	] whileTrue: [
		| index |
		index := line indexOf: $:.
		headers
			at: (line copyFrom: 1 to: index - 1)
			put: (line copyFrom: index + 1 to: line size) trimBlanks.
	].
	(contentLength := headers at: 'Content-Length' ifAbsent: [nil]) notNil ifTrue: [
		contentLength := contentLength asNumber.
		string := readStream upToEnd.
		100 timesRepeat: [
			string size < contentLength ifTrue: [
				string addAll: (socket readString: 10000).
				string size < contentLength ifTrue: [
					(Delay forMilliseconds: 10) wait.
				].
			].
		].
	].
	type = 'GET' ifTrue: [
		pieces := path subStrings: $?.
		path := pieces at: 1.
		string := 1 < pieces size 
			ifTrue: [pieces at: 2]
			ifFalse: [''].
	].
	args := Dictionary new.
	(string subStrings: $&) do: [:each | 
		| index |
		index := each indexOf: $=.
		args
			at: (each copyFrom: 1 to: index - 1)
			put: (self translate: (each copyFrom: index + 1 to: each size)).
	].
	path = '/' ifTrue: [path := '/index.html'].
	pieces := path subStrings: $/.
	selector := (pieces at: 2) asSymbol.
	(self class canUnderstand: selector) ifTrue: [
		^self returnJSON: [self perform: selector].
	].
	class := WebTools at: selector ifAbsent: [nil].
	class notNil ifTrue: [
		^self returnJSON: [
			selector := 3 = pieces size 
				ifTrue: [(pieces at: 3) asSymbol]
				ifFalse: [#'json'].
			(class arguments: args) perform: selector.
		].
	].

	(gsFile := GsFile openReadOnServer: '$GEMSTONE/examples/www' , path) notNil ifTrue: [
		| type |
		type := (path subStrings: $.) last.
		stream nextPutAll: gsFile contents.
		gsFile close.
		resultCode := 200.
		self contentType: (self contentTypes at: type ifAbsent: ['text/html; UTF-8']).
		self doAnswer.
		^self.
	].
	self errorNotFound: path.
%
category: 'Request Handler'
set compile_env: 0
method: Server
handleRequestOn: aSocket
	"Called on a *copy* of the instance accepting connections
	so we can keep local stuff in instance variables and handle
	requests in parallel."

	startTime := Time millisecondClockValue.
	System commitTransaction.
	server := nil.
	socket := aSocket.
	resultCode := nil.
	stream := WriteStream on: String new.
	self 
		initializeHeaders;
		handleRequest;
		yourself.
	System commitTransaction.
%
category: 'Request Handler'
set compile_env: 0
method: Server
htmlForError: ex

	| description |
	((description := ex description) isKindOf: String) ifFalse: [description := description printString].
	stream := WriteStream on: String new.
	stream nextPutAll: '<h3>' , description , '</h3>'.
	((GsProcess stackReportToLevel: 100) subStrings: Character lf) do: [:each | 
		stream nextPutAll: each , '<br />'.
	].
	^stream contents.

%
category: 'Request Handler'
set compile_env: 0
method: Server
htmlWithBody: aString

	stream nextPutAll: 
'<!DOCTYPE html PUBLIC ''-//W3C//DTD XHTML 1.0 Strict//EN'' ''http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd''>
<html xmlns=''http://www.w3.org/1999/xhtml'' xml:lang=''en'' lang=''en''>
<head><title>GemStone/S Web Tools</title></head>
<body>' , aString , '</body></html>'.
	self doAnswer.

%
category: 'Request Handler'
set compile_env: 0
method: Server
initializeHeaders

	| dateTime dateString |
	dateTime := DateTime now.
	dateString := (WriteStream on: String new)
		nextPutAll: (#('Sun' 'Mon' 'Tue' 'Wed' 'Thu' 'Fri' 'Sat') at: dateTime dayOfWeekGmt); space;
		nextPutAll: (dateTime asStringGmtUsingFormat: #(1 2 3 $  2 1 $: true true false false));
		nextPutAll: ' GMT';
		contents.
	httpHeaders := Dictionary new
		at: 'Accept-Ranges'			put: 'bytes';
		at: 'Allow'						put: 'GET';
		at: 'Cache-Control'			put: 'max-age=0'; "86400';"
		at: 'Content-Encoding'		put: 'none';
		at: 'Content-Language'		put: 'en';
		at: 'Content-Type'			put: 'text/html; charset=utf-8';
		at: 'Date' 						put: dateString;
		at: 'Server'						put: 'GemStone/S 64 Bit Server';
		yourself.
%
category: 'Request Handler'
set compile_env: 0
method: Server
parseFileAndArgumentsFrom: aString lines: anArray

	| list file args |
	args := Dictionary new.
	list := aString subStrings: $?.
	file := list at: 1.
	2 <= list size ifTrue: [
		list := (list at: 2) subStrings: $&.
		list := list collect: [:each | each subStrings: $=].
		list do: [:each | 
			args 
				at: each first 
				put: (self translate: each last).
		].
	].
	^file -> args.
%
category: 'Request Handler'
set compile_env: 0
method: Server
parseHeadersFrom: anArray

	| dict |
	dict := Dictionary new.
	anArray do: [:each | 
		| index |
		index := each indexOf: $:.
		dict
			at: (each copyFrom: 1 to: index - 1)
			put: (each copyFrom: index + 2 to: each size).
	].
	^dict.
%
category: 'Request Handler'
set compile_env: 0
method: Server
reasonPhrase

	^(Dictionary new
		at: 200 put: 'OK';
		at: 404 put: 'Not Found';
		at: 405 put: 'Method Not Allowed';
		at: 426 put: 'Upgrade Required';
		yourself)
		at: resultCode
		ifAbsent: ['Unknown Error'].

%
category: 'Request Handler'
set compile_env: 0
method: Server
returnJSON: aBlock

	[
		| data |
		resultCode := 200.
		self contentType: 'text/json; charset=UTF-8'.
		self beNoCache.
		data := [
			aBlock value.
		] on: Error , Admonition do: [:ex | 
			ex return: (Dictionary new 
				at: '_error' put: ex description; 
				at: '_stack' put: (GsProcess stackReportToLevel: 50);
				yourself).
		].
		data at: '_time' put: (Time millisecondClockValue - startTime).
		data printJsonOn: stream.
		self doAnswer.
	] on: Error , Admonition do: [:ex | 
		resultCode := 500.
		self htmlForError: ex.
		self doAnswer.
		ex return.
	].
%
category: 'Web Server'
set compile_env: 0
method: Server
serverURL

	^'http://' , ((System descriptionOfSession: System session) at: 3) , ':' , socket port printString , '/'.

%
category: 'Web Server'
set compile_env: 0
method: Server
setupServerSocketAtPort: anInteger

	socket := GsSocket new.
	(socket makeServerAtPort: anInteger) isNil ifTrue: [
		| string |
		string := socket lastErrorString.
		socket close.
		self error: string.
	].
%
category: 'Web Server'
set compile_env: 0
method: Server
startForegroundServerAtPort: anInteger

	self setupServerSocketAtPort: anInteger.
	GsFile stdout nextPutAll: self serverURL; cr.
	[
		[true] whileTrue: [(Delay forSeconds: 5) wait].
	] forkAt: Processor userBackgroundPriority.
	[
		[
			true.
		] whileTrue: [
			(socket readWillNotBlockWithin: 1000) ifTrue: [
				self copy handleRequestOn: socket accept.
			].
		].
	] ensure: [
		socket close.
	].
%
category: 'Web Server'
set compile_env: 0
method: Server
startServerAtPort: anInteger

	self setupServerSocketAtPort: anInteger.
	server := [
		[
			[
				Processor yield.
				true.
			] whileTrue: [
				(socket readWillNotBlockWithin: 1000) ifTrue: [
					[:aServer :aSocket |
						aServer handleRequestOn: aSocket.
					] forkAt: Processor userBackgroundPriority
						with: (Array 
							with: self copy
							with: socket accept).
				].
			].
		] ensure: [
			socket close.
		].
	] forkAt: Processor userBackgroundPriority.
	^self serverURL.
%
category: 'Web Server'
set compile_env: 0
method: Server
stopServer

	server notNil ifTrue: [
		server terminate.
		server := nil.
	].

%
category: 'Json'
set compile_env: 0
method: Server
tools

	| list |
	list := WebTools select: [:each | each isClass and: [each ~~ Tool and: [each isSubclassOf: Tool]]].
	list := list reject: [:each | each sortOrder isNil].
	list := list asSortedCollection: [:a :b | a sortOrder <= b sortOrder].
	list := list asArray collect: [:each | 
		Dictionary new
			at: 'file' put: each fileName;
			at: 'name' put: each displayName;
			at: 'description' put: each description;
			yourself.
	].
	^Dictionary new
		at: #'tools'	put: list;
		yourself.
%
category: 'Request Handler'
set compile_env: 0
method: Server
translate: aString

	| readStream writeStream string |
	readStream := ReadStream on: aString.
	writeStream := WriteStream on: String new.
	[
		readStream atEnd not.
	] whileTrue: [
		| char |
		char := readStream next.
		char = $+ ifTrue: [
			writeStream space.
		] ifFalse: [
			char = $% ifTrue: [
				| array value |
				array := #($0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $A $B $C $D $E $F).
				value := (array indexOf: readStream next) - 1 * 16 + (array indexOf: readStream next) - 1.
				writeStream nextPut: (Character codePoint: value).
			] ifFalse: [
				writeStream nextPut: char.
			].
		]
	].
	string := writeStream contents.
	string = 'null' ifTrue: [^nil].
	string = 'true' ifTrue: [^true].
	string = 'false' ifTrue: [^false].
	^string
%
doit
Server category: 'WebTools'
%
