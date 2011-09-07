GemStone.saveScript('scripts/Statistics.js', function() {
	var	$tabPanel
	;
	GemStone.loadCSS('css/Statistics.css');
	GemStone.runJsOnce('flot/jquery.flot.js', function() {
		GemStone.runJsOnce('flot/jquery.flot.stack.js', function() {
			GemStone.addTab({
				id:		'newTab'
			,	label:	'Statistics'
			,	title:	'Work with Statmonitor files'
			,	onAdd:	onAdd
			}); 
		});
	});
	return;

	function onAdd(tabPanel) {
		$tabPanel = tabPanel;
		$('.readFile').click(readFile);
		$('.refresh').click(refresh);
		refresh();
	}
	
	function refresh() {
		GemStone.ajax('GET', 'Statistics', null, gotFiles);
	}

	function readFile() {
		$('#readStatmonDialog').dialog({
			buttons: { Read: doRead }
		,	modal: true
		,	open: function() { $('#statmonPath').focus() }
		});
	}

	function doRead() {
		var data = { path: $('#statmonPath').val() };
		$(this).dialog('close'); 
		GemStone.ajax('GET', 'Statistics/readFile', data, gotFiles); 
	}

		
	function gotFiles(data) {
		GemStone.scroll($('.statmonFiles', $tabPanel));
		var items = [];
		$('.statmonFiles tbody', $tabPanel).empty();
		$('.statmonProcesses tbody', $tabPanel).empty();
		$('.statmonProcesses', $tabPanel).addClass('hidden');
		$('.statmonStats tbody', $tabPanel).empty();
		$('.statmonStats', $tabPanel).addClass('hidden');
		$.each(data['files'], function() {
			items.push('<tr class="clickable">');
			items.push('<td title="' + this['time'] + '">' + this['time'] + '</td>');
			items.push('<td title="' + this['platform'] + '">' + this['platform'] + '</td>');
			items.push('<td title="' + this['gemStoneVersion'] + '">' + this['gemStoneVersion'] + '</td>');
			items.push('<td title="' + this['machine'] + '">' + this['machine'] + '</td>');
			items.push('<td class="path" title="' + this['path'] + '">' + this['path'] + '</td>');
			items.push('</tr>');
		});
		$('.statmonFiles tbody', $tabPanel).append(items.join(''));
		GemStone.menu({
			selector: $('.statmonFiles tbody tr', $tabPanel)
		,	menu: statmonMenu
		});
		$('.statmonFiles tbody tr', $tabPanel).click(showFile);
		$(window).resize();		//	force resize to update column widths
		return;
		
		function statmonMenu(element) { 
			return [
				{	title: 'Remove'
				,	action: function() { remove($('td.path', element).text()); }
				}
			,	{	title: 'SPC Page Types'
				,	action: function() { spcPageTypes($('td.path', element).text()); }
				}
			]; 
		}

		function remove(path) {
			$('div#removeStatmonDialog span.path').text(path);
			$('div#removeStatmonDialog').dialog({
				buttons: { 
					Remove: function() { doRemove(path, this); }
				,	Cancel: function() { $(this).dialog('close'); }
				}
			,	modal: true
			});
		}

		function spcPageTypes(path) {
			GemStone.ajax('GET', 'Statistics/spcPageTypes', { path: path }, gotStastic);
		}
		
		function doRemove(path, dialog) {
			var data = { path: path };
			GemStone.ajax('GET', 'Statistics/removeFile', data, gotFiles); 
			$(dialog).dialog('close'); 
		}

		function showFile() { 
			$('.statmonFiles tbody tr', $tabPanel).removeClass('selected');
			$(this).addClass('selected');
			var data = { 
				path: $('.statmonFiles tbody tr.selected td.path', $tabPanel).text()
	//		,	filter: $('#statmonFilter option', $tabPanel)
			};
			GemStone.ajax('GET', 'Statistics/processes', data, gotProcesses);
		}
	}

	function gotProcesses(data) {
//		console.log(data);
		GemStone.scroll($('.statmonProcesses', $tabPanel));
		$('.statmonProcesses', $tabPanel).removeClass('hidden');
		$('.statmonProcesses tbody', $tabPanel).empty();
		$('.statmonStats tbody', $tabPanel).empty();
		$('.statmonStats', $tabPanel).addClass('hidden');
	/*
		$('#statmonFilter', $tabPanel).empty();
		var items = ['<option></option>'];
		$.each(data['statNames'], function() {
			items.push('<option>' + this + '</option>');
		});
		$('#statmonFilter', $tabPanel).append(items.join(''));
	*/
		var items = [];
		$.each(data['processes'], function() {
			var time = this['start']
			,	prim = []
			,	crb = 0 < this['crb'] ? this['crb'] : ''
			;
			$.each(this['prim'],function() { prim.push(this); });
			items.push('<tr class="clickable" id="oop' + this['oop'] + '">');
			items.push('<td title="' + time + '">' + time.substring(11, 16) + '</td>');
			items.push('<td class="number">' + this['count'] + '</td>');
			items.push('<td class="number">' + this['pid'] + '</td>');
			items.push('<td class="number">' + this['session'] + '</td>');
			items.push('<td>' + this['type'] + '</td>');
			items.push('<td>' + prim.join(',') + '</td>');
			items.push('<td class="number">' + crb + '</td>');
			items.push('<td>' + this['name'] + '</td>');
			items.push('</tr>');
		});
		$('.statmonProcesses tbody', $tabPanel).append(items.join(''));
		$('.statmonProcesses tbody tr', $tabPanel).click(showProcess);
		$(window).resize();		//	force resize to update column widths
		return;
		
		function showProcess() { 
			$('.statmonProcesses tbody tr', $tabPanel).removeClass('selected');
			$(this).addClass('selected');
			var data = { 
				path: $('.statmonFiles tbody tr.selected .path', $tabPanel).text() 
			,	processOop: this.id.substring(3)	
			};
			GemStone.ajax('GET', 'Statistics/process', data, gotProcess);
		}
	}

	function gotProcess(data) {
		GemStone.scroll($('.statmonStats', $tabPanel));
		var items = [];
		$('.statmonStats tbody', $tabPanel).empty();
		$('.statmonStats', $tabPanel).removeClass('hidden');
		$.each(data['statistics'], function() {
			var type = this['type']
			,	min = this['min']
			,	max = this['max']
			,	mean = this['mean']
			,	rate = this['rate']
			,	units = this['units']
			,	average
			,	title;
			if (units == 'none') units = '';
			if (type == 'counter') {
				min = max = '';
				average = rate;
				title = rate;
				if (0 < average && average < 1) {
					var factor = Math.pow(10, Math.ceil(0 - Math.log(rate) / Math.LN10));
					average = Math.round(average * factor) / factor;
				} else {
					average = Math.round(average);
				}
				if (average != 0 && units == 'milliseconds') {
					average = average / 10;		// convert to percent
					var factor = Math.pow(10, Math.ceil(0 - Math.log(average) / Math.LN10));
					average = (Math.round(average * factor) / factor) + '%';
					title = title + ' ms/sec';
				} else {
					average = average + '/sec';
					if (average < 1) {
						title = Math.round(1 / average) + ' seconds between each';
					} else {
						title = title + ' ' + units + '/sec';
					}
				}
			} else {
				average = Math.round(mean);
				title = average + ' ' + units;
			}
			items.push('<tr class="clickable" id="oop' + this['oop'] + '">');
			items.push('<td title="' + this['descr']);
			items.push(' (' + type + ':' + units + ')"');
			items.push(' class="name">' + this['name'] + '</td>');
			items.push('<td class="number">' + min + '</td>');
			items.push('<td class="number">' + max + '</td>');
			items.push('<td class="number" title="' + title + '">' + average + '</td>');
			items.push('</tr>');
		});
		$('.statmonStats tbody', $tabPanel).append(items.join(''));
		$('.statmonStats tbody tr', $tabPanel).click(showStatistic);
		$('.statmonStats .tableBody', $tabPanel).scrollTop(0);
		$(window).resize();		//	force resize to update column widths
		return;
		
		function showStatistic() {
			var data = { 
				path: $('.statmonFiles tbody tr.selected .path', $tabPanel).text() 
			,	processOop: $('.statmonProcesses tbody tr.selected', $tabPanel).attr('id').substring(3)
			,	statName: $('.name', this).text()	
			};
			GemStone.ajax('GET', 'Statistics/statistic', data, gotStastic);
		}
	}

	function gotStastic(data) {
		console.log(data);
		var name = data['statName']
		,	id = name + data['processOop']
		,	flotData = data['flotData']
		,	options = data['options']
		;
		$('#tabs').append('<div id="' + id + '"'
			+ ' class="statPanel maximize">' 
			+ $('#statTemplate').html() 
			+ '</div>');
		var $flotDiv = $('#' + id + ' .flot');
		$flotDiv.bind("plothover", function (event, pos, item) {
			if (item) {
				var point = item['datapoint']
				,	date = (new Date(point[0] + 28800000)).toString().substr(0,24)
				,	value = point[1]
				;
				$flotDiv.attr('title', value + ' at ' + date);
			}
		});

		resize();
		GemStone.addTab({
			id:		id
		,	label:	name
		,	title:	'Graph of statistics'
		,	onAdd:	function(aPanel) { $(aPanel).resize(resize); }
		});
		return;
		
		function resize() {
			$flotDiv
				.css('height', $flotDiv.parent().height() - 50)
				.css('width', $flotDiv.parent().width() - 50);
			$.plot($flotDiv, flotData, options);
		}
	}
});