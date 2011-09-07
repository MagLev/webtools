GemStone.saveScript('scripts/ObjectLog.js', function() {
	var $tabPanel
	;
	GemStone.loadCSS('css/ObjectLog.css');
	GemStone.addTab({
		id:		'newTab'
	,	label:	'Object Log'
	,	title:	'Object Log'
	,	onAdd:	onAdd
	});
	return;
	
	function onAdd(tabPanel) {
		$tabPanel = tabPanel;
		GemStone.ajax('GET','ObjectLog', {}, gotList);
		GemStone.scroll($('.objectLog', $tabPanel));
	}
	
	function gotList(data) {
		var items = new Array();
		if (!data.list) { return; }
		$.each(data.list, function(index, value){
			items.push('<tr class="clickable" title="' + value.oop + '">');
			items.push('<td class="stamp" title="' + value.stamp + '">' + value.stamp + '</td>');
			items.push('<td class="pid" title="' + value.pid + '">' + value.pid + '</td>');
			items.push('<td class="label" title="' + value.label + '">' + value.label + '</td>');
			items.push('<td class="type" title="' + value.type + '">' + value.type + '</td>');
			items.push('<td class="tag" title="' + value.tag + '">' + value.tag + '</td>');
			items.push('<td class="object" title="' + value.object + '">' + value.object.substring(0,50) + '</td>');
			items.push('<td class="stack" title="' + (value.hasContinuation ? 'Click to debug continuation' : '') + '">' + (value.hasContinuation ? 'Yes' : '') + '</td>');
			items.push('</tr>');
		});
		$('.objectLog .tableBody tbody', $tabPanel)
			.empty()
			.append(items.join(''));
		$('.objectLog .tableBody', $tabPanel).scrollTop(0);
		$('.objectLog .tableBody tbody tr', $tabPanel).click(clickedOnEntry);
		$(window).resize();		//	force resize to update column widths
	}

	function clickedOnEntry(event) {
		if (0 < $('td.stack', this).text().length) {
			if (confirm($('td.object', this).attr('title') + '\n\nDebug continuation?')) {
				var oop = $(this).attr('title');
				GemStone.ajax(
					'GET'
				,	'Debugger.html' 
				,	null
				,	function(html) {
						var string = html.replace('OOP', oop);
						$('body').append(string);
					} 
				);
			}
		} else {
			alert($('td.object', this).attr('title'));
		}
	}
});
