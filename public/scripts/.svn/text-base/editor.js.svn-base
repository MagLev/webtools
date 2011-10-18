GemStone.saveScript('scripts/editor.js', function(options) {
	var	editor			
	,	isMethod		= false
	,	dictionaryName
	,	className
	,	isMeta			= false
	,	category		
	,	source			= ''
	,	errorInfo		= {}
	,	stepPoints
	,	unmarkFunctions = []
	,	mousePosition	= { x: 0, y: 0 }
	,	onSaveFunction
	;
	GemStone.loadCSS('CodeMirror/css/codemirror.css');
	GemStone.loadCSS('CodeMirror/css/default.css');
	GemStone.loadCSS('css/editor.css');
	GemStone.runJsOnce('CodeMirror/js/codemirror.js', function() {
		GemStone.runJsOnce('CodeMirror/js/ruby.js', function() {});
		GemStone.runJsOnce('CodeMirror/js/smalltalk.js', function() {
			setupEditor(); 
		}); 
	});
	return {
		isOkayToChange: isOkayToChange
	,	isModified: 	isModified
	,	setCode:		setCode
	,	setMethod:		setMethod
	};

	function setupEditor() {
		var id = GemStone.nextId();
		options.editArea.attr('id', id);
		editor = CodeMirror(document.getElementById(id), {
			mode: 'text/x-stsrc'
		,	enterMode: 'keep'
		,	lineNumbers: true
		,	matchBrackets: true
		,	onChange: onEditorChange
		,	onHighlightComplete: onHighlightComplete
		});
		options.editArea.focus(setFocusToEnd);
		$(editor.getWrapperElement()).mousemove( function(e) { 
			mousePosition.x = e.pageX; 
			mousePosition.y = e.pageY; 
		});
		options.editArea.keydown(keydown);
		$(editor.getWrapperElement()).focus();
	}

	// keypress does not <Ctrl>+<S> (and other browser overrides) so we use keydown event
	function keydown(event) {
		if (!event.ctrlKey) { return; }
//		console.log(event);
		if (event.keyCode === 65) { // <Ctrl>+<A>
			var lastLine = editor.lineCount() - 1
			,	size = editor.getLine(lastLine).length
			,	end = { line: lastLine, ch: size }
			,	start = { line: 0, ch: 0 }
			;
			editor.setSelection(start, end);
			event.preventDefault();
			return;
		}
		if (event.keyCode === 68) { // <Ctrl>+<D>
			evaluateText();
			event.preventDefault();
			return;
		}
		if (event.keyCode === 83) { // <Ctrl>+<S>
			saveText();
			event.preventDefault();
			return;
		}
		return; 
	}
	
	function evaluateText() {
		var pos = editor.getCursor(true)
		,	start = { line: pos.line, ch: pos.ch }
		;
		pos = editor.getCursor(false);
		var	end = { line: pos.line, ch: pos.ch }
		, 	myRequest;
		if (!editor.somethingSelected()) {
			start.ch = 0;
			end.ch = editor.getLine(start.line).length;
			editor.setSelection(start, end);
		}
		myRequest = { text: editor.getRange(start, end) };
		GemStone.ajax('POST','Workspace/evaluate', myRequest, onEvaluate);
		return;
		
		function onEvaluate(data) {
			if (!data.errorType) {
				start = editor.getCursor(false);
				editor.setCursor(start);
				editor.replaceRange(' ' + data.string, start);
				var end = editor.getCursor(false);
				editor.setSelection(start, end);
				return;
			}
			if (data.errorType === 'compileError') {
				compileError(data.errorDetails);
				return;
			}
			if (confirm(data.description + '\nDebug process?')) {
				GemStone.ajax(
					'GET'
				,	'Debugger.html' 
				,	null
				,	function(html) {
						var string = html.replace('OOP', data.oop);
						$('body').append(string);
					} 
				);
				return;
			} else {
				GemStone.ajax('POST', 'Workspace/deleteProcess', data);
			}
			console.log(data);
		}
	}

	function saveText() {
		if (isMethod) {
			saveMethod();
		} else {
			saveCode();
		}
	}
	
	function saveCode() {
		var myRequest = {
			text: editor.getValue()
		};
		GemStone.ajax('POST','Workspace/evaluate', myRequest, onSaveCode);
		return;
		
		function onSaveCode(data) {
			if (!data.errorType) {
				source = editor.getValue();
				if (onSaveFunction) { onSaveFunction(data); }
				return;
			}
			if (data.errorType === 'compileError') {
				compileError(data.errorDetails);
				return;
			}	
		}
	}
	
	function compileError(data) {
		var position =  positionOfOffset(data[0][1]);
		editor.replaceRange('\n', { line: position.line + 1, ch: 0 });
		errorInfo.position = position;
		errorInfo.element = document.createElement('div');
		$(errorInfo.element).addClass('compileError');
		$(errorInfo.element).text('^-' + data[0][2]);
		editor.addWidget(position, errorInfo.element, true);
	}
 
	function saveMethod() {
		var myRequest = {
			dict:		dictionaryName
		,	klass:		className
		,	isMeta:		isMeta
		,	category:	category
		,	source: 	editor.getValue()
		};
		GemStone.ajax('POST','Workspace/saveMethod', myRequest, onSaveMethod);
		return;
		
		function onSaveMethod(data) {
			if (data.compileError) { compileError(data.compileError); return; }
			source = editor.getValue();
			if (data.warnings) { alert(data.warnings); }
			if (onSaveFunction) { onSaveFunction(data.selector); }
		}
	}
	
	
	
	function setFocusToEnd() {
		var line = editor.lineCount()
		,	string = editor.getLine(line - 1)
		,	ch = string.length - 1
		;
		editor.focus(); 
		editor.setCursor(line, ch);
	}

	function onEditorChange() {
		stepPoints = new Array;	// [zero-based offset, one-based step-point, selector]
		doUnmark();
		if (errorInfo.position) { 
			var line = errorInfo.position.line + 1;
			errorInfo.position = null;
			$(errorInfo.element).remove();
			editor.removeLine(line);
		}
	}

	function doUnmark() {
		$.each(unmarkFunctions, function(i, aFunction) { aFunction(); });
		unmarkFunctions = [];
	}
	
	function isOkayToChange() {
		if (!isModified()) { return true; }
		if (confirm('Okay to lose edits?')) {
			source = editor.getValue();
			return true;
		}
		return false;
	}
	
	function isModified() { 
		return (editor ? editor.getValue() !== source : false);
	};
	
	function setMethod(data, aFunction) {
		if (!editor) { return; }
		isMethod = true;
		onSaveFunction = aFunction;
		doUnmark();
		editor.setValue('');
		stepPoints = new Array();	// [zero-based offset, one-based step-point, selector]
		if (!data) { return; }
		dictionaryName = data.dictionaryName;
		className = data.className;
		isMeta = data.isMeta;
		category = data.category;
		source = data.source ? data.source : '';
		setCodeMode(source);
		editor.setValue(source); 
		calculateStepPoints(data);
	}
	
	function setCode(aString, aFunction) {
		if (!editor) { return; }
		isMethod = false;
		onSaveFunction = aFunction;
		doUnmark();
		editor.setValue('');
		stepPoints = [];
		if (!aString) { return; }
		dictionaryName = null;
		className = null;
		isMeta = null;
		category = null;
		source = aString;
		setCodeMode(source);
		editor.setValue(source); 
	}
	
	function setCodeMode(source) {
		ruby_regex = /^\s*(def|do|begin|<a define)/
		smalltalk_regex = /^[_a-zA-Z0-9]+:/
		if (source.match(ruby_regex)) {
			editor.setOption('mode', 'text/x-ruby');
		} else if (source.match(smalltalk_regex)) {
			editor.setOption('mode', 'text/x-stsrc');
		} else {
			editor.setOption('mode', 'text/x-stsrc');
		}
	}
	
	function calculateStepPoints(data) {
		if (!data.stepPoints) { return; }
		for (var i = 0; i < data.stepPoints.length; i = i + 1) {
			var offset = data.stepPoints[i] - 1
			,	from = positionOfOffset(offset + 2)
			,	token = editor.getTokenAt(from)
			,	to = { line: from.line, ch: token.end }
			,	lineInfo
			,	cssClass
			;
			cssClass = 'gsStepPoint';
			if (i + 1 === data.nowAt) { cssClass = cssClass + ' gsCurrentStepPoint'; }
			from.ch = from.ch - 1;
			unmarkFunctions[unmarkFunctions.length] = editor.markText(from, to, cssClass);
			stepPoints[stepPoints.length] = [offset, i + 1];
		}
		for (var i = 0; i < data.sends.length; i = i + 2) {
			var offset = data.sends[i] - 1
			,	selector = data.sends[i + 1]
			,	stepPoint = stepPointDataFor(offset)
			;
			stepPoint[2] = selector;
		}
		GemStone.menu({
			selector: $(editor.getWrapperElement())
		,	menu: editorMenu
		});
	}

	function positionOfOffset(anInteger) {
		var lines = editor.getValue().split('\n')
		,	start = 0
		,	end = 0
		, 	position = { 
				line: lines.length - 1, 
				ch: lines[lines.length - 1].length - 1 
			}
		;
		$.each(lines, function(i, line) {
			start = end;
			end = start + line.length + 1;
			if (start <= anInteger && anInteger <= end) {
				position = { line: i, ch: anInteger - start - 1 };
			}
		});
		return position;
	}
	
	function offsetOfPosition(position) {
		var offset = null
		,	lines = editor.getValue().split('\n')
		,	start = 0
		,	end = 0
		;
		$.each(lines, function(i, line) {
			start = end;
			end = start + line.length + 1;
			if (i === position.line) {
				offset = start + position.ch;
			}
		});
		return offset;
	}
	
	function stepPointDataFor(offset) {
		var value;
		$.each(stepPoints, function(i, stepPoint) { 
			// if mouse is to left of step point, allow it
			if (stepPoint[0] === offset + 1) { value = stepPoint; }
			if (stepPoint[0] === offset) { value = stepPoint; }
		});
		return value;
	}

	function editorMenu(element) {
		var coordsChar = editor.coordsChar(mousePosition)
		,	token = editor.getTokenAt(coordsChar)
		,	offset = offsetOfPosition({line: coordsChar.line, ch: token.start})
		,	stepPointData = stepPointDataFor(offset)
		,	stepPoint = stepPointData ? stepPointData[1] : null
		,	mySelector = stepPointData ? stepPointData[2] : null
		,	items = []
		;
		if (stepPoint) {
			items[0] = {
				title: 'Step Point ' + stepPoint
			,	customClass: 'disabled'
			,	action: function() {}
			};
			if (mySelector) {
				items[1] = {
					title: 'Implementors of ' + mySelector
				,	action: function() { GemStone.browseImplementorsOf(mySelector); }
				};
				items[2] = {
					title: 'Senders of ' + mySelector
				,	action: function() { GemStone.browseSendersOf(mySelector); }
				};
			}
		}
		items[items.length] = {
			title: 'Implementors of ...'
		,	action: function() { GemStone.browseImplementorsOf(); }
		};
		items[items.length] = {
			title: 'Senders of ...'
		,	action: function() { GemStone.browseSendersOf(); }
		};
		return items; 
	}
	
	function onHighlightComplete(anEditor) {
//		console.log('onHighlightComplete');
	}
	
/*
,	'Undo': {
		click: function(element) { console.log('Undo'); }
	}
,	'Redo': {
		click: function(element) { console.log('Redo'); }
	}
,	'------------': {
		click: function(element) {  }
	,	klass: 'separator'
	}
,	'Cut': {
		click: function(element) { console.log('Cut'); }
	}
,	'Copy': {
		click: function(element) { console.log('Copy'); }
	}
,	'Paste': {
		click: function(element) { console.log('Paste'); }
	}
,	'Delete': {
		click: function(element) { console.log('Delete'); }
	}
,	'-----------': {
		click: function(element) {  }
	,	klass: 'separator'
	}
,	'Select All': {
		click: function(element) { console.log('Select All'); }
	}
,	'----------': {
		click: function(element) {  }
	,	klass: 'separator'
	}
,	'Senders': {
		click: function(element) { browseSenders(selector); }
	,	klass: 'gsSenders'
	}
,	'Implementors': {
		click: function(element) { browseImplementors(selector); }
	,	klass: 'gsImplementors'
	}
}
*/
});
