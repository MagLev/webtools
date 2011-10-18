GemStone.saveScript('scripts/MethodList.js', function(search) {
	var $tabPanel
	,	editor
	,	errorElement
	;
	GemStone.loadCSS('css/MethodList.css');
	GemStone.addTab({
		id:		'newTab'
	,	label:	search.label
	,	title:	'Browse ' + search.label
	,	onAdd:	onAdd
	});
	return;
	
	function onAdd(tabPanel) {
		$tabPanel = tabPanel;
//		$tabPanel.keydown(keydown);
		setupEditor();
		GemStone.ajax('GET','MethodList', search, gotList);
		GemStone.scroll($('.methodList', $tabPanel));
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

	function gotList(data) {
//		console.log(data);
		var items = [];
		if (!data.list) { return; }
		$.each(data.list, function(index, value){
			items.push('<tr class="clickable method">');
			items.push('<td class="dict" title="' 		+ GemStone.encodeHTML(value.dict) 		+ '">' + value.dict + '</td>');
			items.push('<td class="klassCat" title="' 	+ GemStone.encodeHTML(value.klassCat) 	+ '">' + value.klassCat + '</td>');
			items.push('<td class="klass" title="' 		+ GemStone.encodeHTML(value.klass) 		+ '">' + value.klass + '</td>');
			items.push('<td class="category" title="' 	+ GemStone.encodeHTML(value.category) 	+ '">' + value.category + '</td>');
			items.push('<td class="selector" title="' 	+ GemStone.encodeHTML(value.selector) 	+ '">' + value.selector + '</td></tr>');
		});
		$('.methodList .tableBody tbody', $tabPanel)
			.empty()
			.append(items.join(''));
		$('.methodList .tableBody', $tabPanel).scrollTop(0);
		$('.tableBody tbody .method', $tabPanel).click(clickedOnMethod);
		$(window).resize();		//	force resize to update column widths
	}

	function clickedOnMethod(event) {
		var myRequest = {
				type: 		'method'
			,	dict: 		encodeURI($('.dict', $(this)).text())
			,	klass: 		encodeURI($('.klass', $(this)).text())
			,	selector: 	encodeURI($('.selector', $(this)).text())
			};
		$('.methodList .method', $tabPanel).removeClass('selected');
		$(this).addClass('selected');
//		console.log(myRequest);
		GemStone.ajax('GET','MethodList', myRequest, gotMethod);
	}
	
	function gotMethod(data) {
		if (editor) {
			editor.setMethod(data);
		}
	}
	
});
