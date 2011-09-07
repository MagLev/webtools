doit
Tool subclass: 'SessionList'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: WebTools
  options: #()

%

! Remove existing behavior from SessionList
doit
SessionList removeAllMethods.
SessionList class removeAllMethods.
%
! ------------------- Class methods for SessionList
category: 'other'
set compile_env: 0
classmethod: SessionList
description

	^'Information about current sessions and other processes'.
%
category: 'other'
set compile_env: 0
classmethod: SessionList
displayName

	^'Current Sessions'.
%
category: 'other'
set compile_env: 0
classmethod: SessionList
sortOrder

	^2.
%
! ------------------- Instance methods for SessionList
category: 'Public'
set compile_env: 0
method: SessionList
cacheDescription

	| name cache details |
	name := arguments at: 'name' ifAbsent: [self error: 'Name not provided!'].
	cache := StatStatistic cacheStatDescriptions.
	details := cache 
		at: name 
		ifAbsent: [Array with: 'No description available for ' , name , '!'].
	^Dictionary new
		at: 'description' put: details first;
		yourself.
%
category: 'Public'
set compile_env: 0
method: SessionList
json

	^Dictionary new
		at: 'labels' 		put: self _sessionListLabels;
		at: 'sessions' 	put: self _sessionList;
		at: 'other' 		put: self _nonSessionList;
		yourself.
%
category: 'Public'
set compile_env: 0
method: SessionList
statsForSlot

	| slot descriptions statistics result topFour bytes |
	slot := arguments at: 'slot' ifAbsent: [self error: 'No slot argument!'].
	(statistics := System cacheStatisticsAt: slot asNumber) isNil ifTrue: [self error: 'Process slot not in use (gem has logged out?)!'].
	descriptions := System cacheStatisticsDescriptionForType: (statistics at: 4).
	result := OrderedCollection new.
	bytes := ByteArray new: 8.
	1 to: descriptions size do: [:i | 
		| name metadata |
		name := descriptions at: i.
		metadata := StatStatistic cacheStatDescriptions at: name ifAbsent: [#('' '' '' '' '')].
		result add: (Dictionary new
			at: 'name'	put: name;
			at: 'type'	put: (metadata at: 2);
			at: 'level'	put: (metadata at: 3);
			at: 'units'	put: (metadata at: 4);
			at: 'isOs'	put: (metadata at: 5);
			at: 'value'	put: (statistics at: i);
			yourself).
	].
	topFour := result copyFrom: 1 to: 4.
	result := result copyFrom: 5 to: descriptions size.
	result := result asSortedCollection: [:a :b | (a at: 'name') <= (b at: 'name')].
	result := topFour , result asArray.
	^Dictionary new
		at: 'stats' put: result;
		yourself.
%
category: 'Private'
set compile_env: 0
method: SessionList
_nonSessionList

	| slots |
	[
		| sessions slot |
		sessions := System currentSessions.
		slots := OrderedCollection new.
		slot := 0.
		[true] whileTrue: [
			| array |
			array := System cacheStatistics: slot.
			(sessions includes: (array at: 3)) ifFalse: [
				slots add: (Array with: slot with: array first).
			].
			slot := slot + 1.
		].
	] on: Error do: [:ex | 
		^slots.
	].
%
category: 'Private'
set compile_env: 0
method: SessionList
_sessionList

	| sessions now |
	sessions := System currentSessions collect: [:each | (System descriptionOfSession: each) , (Array with: (System cacheSlotForSessionId: each))].
	sessions := sessions reject: [:each | each isNil].
	now := System timeGmt.	"seconds since January 1, 1970"
	sessions := sessions collect: [:list | 
		| x |
		list
			at: 1 	put: (list at: 1) userId;		"UserProfile"
			at: 4 	put: ((x := list at: 4) = 0 ifTrue: [''] ifFalse: [x]);	"Primitive"
			at: 5 	put: (now - (list at: 5)) printString , 's';		"View age"
			at: 7 	put: (#('none' 'out' 'in') at: (list at: 7) + 2);
			at: 14 	put: (now - (list at: 14)) printString , 's';		"Quiet"
			at: 15 	put: (now - (list at: 15)) printString , 's';		"Age"
			yourself.
	].
	0 timesRepeat: [sessions := sessions , sessions].
	^sessions.
%
category: 'Private'
set compile_env: 0
method: SessionList
_sessionListLabels
		"See System class>>#'descriptionOfSession:'"

	^(Array new: 17)
		at: 1 	put: 'User' -> 'The UserProfile of the session, or nil if the UserProfile is recently created and not visible from this session''s transactional view, or the session is no longer active.';
		at: 2 	put: 'PID' -> 'The process ID of the Gem process of the session.';
		at: 3 	put: 'Host' -> 'The hostname of the machine running the Gem process (a String, limited to 127 bytes).';
		at: 4 	put: 'Prim' -> 'Primitive number in which the Gem is executing (if it is executing in a long primitive such as MFC).';
		at: 5 	put: 'View Age' -> 'Time since the session''s most recent beginTransaction, commitTransaction, or abortTransaction.';
		at: 6 	put: 'State' -> 'The session state (an enum from SesTaskStatusEType in session.ht).';
		at: 7 	put: 'Trans' -> 'One of the following: ''none'' if the session is in transactionless mode, ''out'' if it is not in a transaction, and ''in'' if it is in a transaction.';
		at: 8 	put: 'Oldest CR' -> 'A Boolean whose value is true if the session is currently referencing the oldest commit record, and false if it is not.';
		at: 9 	put: 'Serial' -> 'The session''s serial number. A serial number will not be reused until the stone is restarted.';
		at: 10 	put: 'Session' -> ('The session''s sessionId. The configured maximum is ' , System maxSessionId printString , ' for this stone.');
		at: 11 	put: 'GCI IP' -> 'A String containing the ip address of host running the GCI process. If the GCI application is linked (using libgcilnk*.so or gcilnk*.dll) this ip address is the address of the machine running the gem process.';
		at: 12 	put: 'Priority' -> 'The priority of the session where 0 is lowest, 2 is normal, and 4 is highest. Session priority is used by the stone to order requests for service by sessions.';
		at: 13 	put: 'Host ID' -> 'Unique host ID of the host where the session is running.';
		at: 14 	put: 'Quiet' -> 'Time since the session''s most recent request to stone.';
		at: 15 	put: 'Age' -> 'Time since the session logged in.';
		at: 16 	put: 'CRB' -> 'Commit Record Backlog: number of commits which have occurred since the session obtained its view.';
		at: 17	put: 'Slot' -> 'The session''s cache process slot number if it is connected to the same shared page cache as Server. A return of nil indicates the session could not be located (so the gem is likely on another host).';
		yourself.
%
doit
SessionList category: 'WebTools'
%
