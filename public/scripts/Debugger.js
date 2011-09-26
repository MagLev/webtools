GemStone.saveScript('scripts/Debugger.js', function(oop) {
	var $tabPanel
	,	editor
	,	errorElement
	;
	GemStone.loadCSS('css/Debugger.css');
	GemStone.addTab({
		id:		'newTab'
	,	label:	'Debugger'
	,	title:	'Debugger'
	,	onAdd:	onAdd
	});
	return;
	
	function onAdd(tabPanel) {
		$tabPanel = tabPanel;
		setupEditor();
		$('button.restart', $tabPanel).click(function() { restart(); });
		$('button.into', $tabPanel).click(function() { step(-1); });
		$('button.over', $tabPanel).click(function() { step(0 ); });
		$('button.out', $tabPanel).click( function() { step(1 ); });
		$('button.go', $tabPanel).click(  function() { go(  ); });
		GemStone.ajax('GET','Debugger', { oop: oop }, gotData);
	}
	
	function setupEditor() {
		GemStone.runJs(
			'scripts/editor.js'
		,	{ 
				editArea: $('.editArea', $tabPanel)
			}
		,	function(object) { editor = object; }
		);
	}

	function gotData(data) {
//		console.log(data);
		var items = new Array();
		$('.label', $tabPanel).text(data.label ? data.label : '');
		if (!data.stack) { return; }
		$.each(data.stack, function(index, value){
			items.push('<div class="clickable frame frame');
			items.push(index + 1);
			items.push('" title="');
			items.push(value);
			items.push('">');
			items.push(value);
			items.push('</div>');
		});
		$('.stack', $tabPanel)
			.empty()
			.append(items.join(''));
		$('.stack', $tabPanel).scrollTop(0);
		$('.stack .clickable', $tabPanel).click(clickedOnFrame);
		$('.variableValue', $tabPanel).text('');
		$('.stack .clickable', $tabPanel).first().click();
	}
	
	function clickedOnFrame() {
		$('.stack .frame', $tabPanel).removeClass('selected');
		$(this).addClass('selected');
		var myRequest = {
				oop: oop
			,	frame: selectedFrame()
			};
		GemStone.ajax('GET', 'Debugger/frame', myRequest, gotFrame);
		$('.variableValue', $tabPanel).text('');
	}
	
	function selectedFrame() {
		return parseInt($('.stack .selected', $tabPanel).attr('class').match(/frame([0-9]+)/)[1]);
	}

	function gotFrame(data) {
//		console.log(data);
		$('button', $tabPanel).attr('disabled','');
		if (editor) {
			if (data.method) {
				editor.setMethod(data.method);
			} else {
				editor.setCode();
			}
		}
		var items = new Array();
		$.each(data.variables, function(index, each) {
			items.push('<tr class="clickable">');
			items.push('<td class="name">' + each.name + '</td>');
			items.push('<td class="value" title="');
			items.push(each.string.replace(/["]/g, '\\\"') + '">');
			items.push(each.string.substring(0, 30) + '</td>');
			items.push('<td class="oop">' + each.oop + '</td>');
			items.push('</tr>');
		});
		$('.variables tbody', $tabPanel)
			.empty()
			.append(items.join(''));
		$('.variables', $tabPanel).scrollTop(0);
		$('.variables .clickable', $tabPanel).click(clickedOnVariable);
	}

	function clickedOnVariable() {
		$('.variables .clickable', $tabPanel).removeClass('selected');
		$(this).addClass('selected');
		$('.variableValue', $tabPanel).text($('td.value', this).attr('title'));		
	}
	
	function step(anInteger) {
		var myRequest = {
				oop: oop
			,	level: typeof anInteger === 'number' ? selectedFrame() + anInteger : null
			};
		GemStone.ajax('POST', 'Debugger/step', myRequest, didStep);
	}
	
	function go() {
		GemStone.ajax('POST', 'Debugger/go', { oop: oop }, didGo);
	}

	function restart() {
		 var myRequest = {
				oop: oop
			,	frame: selectedFrame()
			};
		 GemStone.ajax('POST', 'Debugger/restart', myRequest, didStep);
	}

	function didStep(data) {
		console.log(data);
		gotData(data); // will return early if no stack change
		gotFrame(data);
	}

	function didGo(data) {
		 console.log(data);
	}
	
});
