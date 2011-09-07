GemStone.saveScript('scripts/SessionList.js', function() {
	var	$tabPanel
	;
	GemStone.loadCSS('css/SessionList.css');
	GemStone.addTab({
		id:		'newTab'
	,	label:	'Sessions'
	,	title:	'Information on Gems and other OS processes'
	,	onAdd:	onAdd
	});
	return;

	function onAdd(tabPanel) {
		$tabPanel = tabPanel;
		GemStone.scroll($('.gemList', $tabPanel));
		GemStone.ajax('GET', 'SessionList', null, gotData);
	}
	
	function gotData(json) {
		var names = $.map(json.labels, function(each) { return each.key; })
		,	slotIndex = names.indexOf('Slot')
		,	hostIndex = names.indexOf('Host ID')
		;
		buildHeader();
		buildData();
		$(window).resize();		//	force resize to update column widths
		buildStoneList();
		$('.slot', $tabPanel).click(showStats);

		function buildHeader() {
			var items = [];
			$.each(json.labels, function() {
				if ('Host ID' !== this.key) {
					items.push('<th title="' + this.value + '">' + this.key + '</th>');
				}
			});
			var html = items.join('');
			$('thead tr', $tabPanel).empty().append(html);
		}
		
		function buildData() {
			var items = [];
			$.each(json.sessions, function(index, sessionInfo) {
				var slot = sessionInfo[slotIndex]
				,	classes = 'slot clickable';
				items.push('<tr');
				if (slot) {
					items.push(' title="Click to see cache statistics for slot ');
					items.push(slot + '"');
					classes = classes + ' slot' + slot;
				}
				items.push(' class="' + classes + '">');
				$.each(sessionInfo, function(index, field) {
					if (index !== hostIndex) {
						var x = (field + '').match(/(\d+)(s)/);
						items.push('<td>');
						if (x) {
							var 
								sec = x[1]
							,	min = Math.floor(sec / 60)
							,	hrs = Math.floor(min / 60)
							,	dys = Math.floor(hrs / 24)
							;
							if (1 < dys) {
								items.push(dys + 'd');
							} else if (1 < hrs) {
								items.push(hrs + 'h');
							} else if (1 < min) {
								items.push(min + 'm');
							}else {
								items.push(sec + 's');
							}
						} else {
							items.push(field);
						}
						items.push('</td>');
					}
				});
				items.push('</tr>');
			});
			$('.gemList .tableBody tbody')
				.empty()
				.append(items.join(''));
		}

		function buildStoneList() {
			var items = []
			;
			$.each(json.other, function(index, name) {
			var	slotId = 'slot' + name[0];
				items.push('<li');
				items.push(' class="' + (0 == index ? 'first' : 'rest') + '"');
				items.push(' title="Click to see cache statistics for slot ');
				items.push(name[0] + '"');
				items.push('><span class="slot clickable ' + slotId + '"');
				items.push('>' + name[1] + '</span></li>');
			});
			$('.stoneProcesses')
				.empty()
				.append(items.join(''));
		}

		function showStats() {
			var slot = $(this).attr('class').match(/slot(\d+)/)[1]
			;
			$('#tabs').append('<div id="newTab" class="statsPanel maximize">' 
				+ $('.statsTemplate').html() 
				+ '</div>');
			GemStone.addTab({
				id:		'newTab'
			,	label:	'Slot ' + slot + ' Stats'
			,	title:	'View process statistics from the shared page cache'
			,	onAdd:	function($tabPanel) { getStats($tabPanel, slot); }
			});
			return;
		}
		
		function getStats($tabPanel, slot) {
			GemStone.scroll($('.stats', $tabPanel));
			GemStone.ajax(
				'GET'
			,	'SessionList/statsForSlot'
			,	{'slot' : slot}
			,	function(data){ gotStats($tabPanel, slot, data); }
			);
			return;
			
			function gotStats($tabPanel, slot, data) {
				var items = [];
				$.each(data.stats, function() {
					items.push('<tr>');
					items.push('<td class="statName">' + this.name + '&nbsp');
					items.push('<a class="descr" href="#">?</a></td>');
					items.push('<td>' + this.type 	+ '</td>');
					items.push('<td>' + this.level 	+ '</td>');
					items.push('<td>' + this.units 	+ '</td>');
					items.push('<td>' + this.isOs 	+ '</td>');
					items.push('<td>' + this.value 	+ '</td>');
					items.push('</tr>');
				});
				$('.tableBody tbody', $tabPanel).empty().append(items.join(''));
				$('.tableBody tbody a.descr', $tabPanel).click(function(event){
					getDescription(event, $(this))
				});
				$(window).resize();		//	force resize to update column widths
				return;

				function getDescription(event, element) {
					var name = $(element).parent().text();
					name = name.substring(0, name.length - 2);
					GemStone.ajax('GET', 'SessionList/cacheDescription', 
						{'name' : name}, gotDescription);
					event.preventDefault();
					return; 
					
					function gotDescription(data) {
						alert(data.description);
						$(element).parent().attr('title', data.description);
					}
				}
			}
		}
	}
});
