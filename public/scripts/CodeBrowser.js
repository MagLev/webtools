GemStone.saveScript('scripts/CodeBrowser.js', function() {
	var	$tabPanel
	,	editor
	,	classCategories = ['']
	,	isDictsTab		= true
	,	dictionary
	,	mcPackage
	,	classCategory
	,	klass
	,	isMetaClass 	= false
	,	superClass
	,	methodFilterBy	= 'category'
	,	methodFilter
	,	selector
	,	implementor
	,	isCtrlKeyDown	= false
	;
	GemStone.loadCSS('css/CodeBrowser.css');
	GemStone.runJsOnce('jsTree/jquery.jstree.js', function() {
		$.jstree._themes = 'jsTree/themes/';
		GemStone.addTab({
			id:		'newTab'
		,	label:	'Code Browser'
		,	title:	'Browse Smalltalk classes'
		,	onAdd:	onAdd
		}); 
	});
	return;

	function onAdd(tabPanel) {
		$tabPanel = tabPanel;
		$('.divList').keydown(keydown);
		doLayout(); 
		update();
	}
	
	// keypress does not capture arrow keys (and other browser overrides) so we use keydown event
	function keydown(event) {
		if (event.ctrlKey || event.shiftKey) { return; }
		if (event.which === 38 || event.which === 40) { upDownKey(event); } 
		return; 
	}
	
	function upDownKey(event) {
		var element = $('.divList:focus')[0];
		if (element) { 
			element = $('.selected', element);
			element = event.which === 38 ? element.prev() : element.next();
			if (element) { 
				element.click();
				event.preventDefault(); 
			}
		}
		return;
	}
	
	function doLayout() {
		createDictionaryPackageTabs();
		createClassesTabs();
		createCategoriesVariablesTabs();
		createInstanceClassTabs();
		fixupTabsCornerStyling();
		setupEditor();
		return;

		function prepareTabs($div, onSelect) {
			var $divList = $('div', $div);
			$.each($('ul li', $div), function(index) {
				var id = GemStone.nextId();
				$('a', this).attr('href', '#' + id);
				$($divList[index]).attr('id', id);
			});
			$div.tabs({ select: onSelect });
		}
		
		function createDictionaryPackageTabs() {
			prepareTabs(
				$('.dictPckgTabs', $tabPanel), 
				function(event, ui) {
					if (setIsDictsTab(0 === ui.index)) { update(); }
				}
			);
		}

		function createClassesTabs() {
			prepareTabs(
				$('.classesTab', $tabPanel), 
				function(event, ui) {  }
			);
		}

		function createCategoriesVariablesTabs() {
			prepareTabs(
				$('.catsVariablesTabs', $tabPanel), 
				function(event, ui) {
					var filterBy = ['category', 'variable'][ui.index];
					if (setMethodFilterBy(filterBy)) { update(); }
				}
			);
		}

		function createInstanceClassTabs() {
			prepareTabs(
				$('.instanceClassTabs', $tabPanel), 
				function(event, ui) {
					if (setIsMetaClass(1 === ui.index)) { update(); }
				}
			);
		}

		function fixupTabsCornerStyling() {
			$('.ui-tabs-nav, .ui-tabs-nav > *, .tabs-empty', $tabPanel)
				.removeClass('ui-corner-all ui-corner-bottom')
				.addClass('ui-corner-top');
			$('.tabs-bottom, .tabs-bottom .ui-tabs-nav, .tabs-bottom .ui-tabs-nav > *', $tabPanel)
				.removeClass('ui-corner-all ui-corner-top')
				.addClass('ui-corner-bottom');
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
	}

	function update() {
		var myRequest = {
				isDictsTab	: isDictsTab
			,	dict		: dictionary
			,	mcPackage	: mcPackage
			,	classCat	: classCategory
			,	klass		: klass
			,	isMeta		: isMetaClass
			,	superClass	: superClass
			,	methodFilterBy	: methodFilterBy
			,	methodFilter	: methodFilter
			,	selector	: selector
			,	implementor	: implementor
			};
//		console.trace();
//		console.log(myRequest);
		GemStone.ajax('GET','CodeBrowser', myRequest, gotData);
	}

	function gotData(data) {
//		console.log(data);
		setMethodFilterListPosition();
		fillDictList();
		fillPackageList();
		fillClassCatTree();
		fillClassList();
		fillSuperList();
		fillMethodFilterList();
		fillMethodList();
		fillImplementorList();
		fillEditArea();
		return;
		
		function setMethodFilterListPosition() {
			$('.methodFilterList', $tabPanel)
				.css('top', $('.catsVariablesTabs', $tabPanel).outerHeight() - 1 + 'px')
				.css('bottom', $('.instanceClassTabs', $tabPanel).height() + 1 + 'px')
				;
		}
		
		function fillDictList() {
			var items = []
			,	foundSelection = false;
			$.each(data.dictList, function(index, value){
				items.push('<div title="' + value + '"');
				items.push(' class="clickable');
				if (value === dictionary) { items.push(' selected'); foundSelection = true; }
				items.push('">' + value + '</div>');
			});
			$('.dictList', $tabPanel).empty().append(items.join(''));
			$('.dictList .clickable', $tabPanel).click(function(event){
				if (setDictionary($(this).text())) { update(); }
			});
			GemStone.menu({
				selector: $('.dictList .clickable', $tabPanel)
			,	menu: dictionaryListMenu
			});
			if (!foundSelection) { dictionary = null; }
		}

		function dictionaryListMenu(element) { 
			return [
				{	title: 'Menu 1'
				,	action: function() { console.log(['menu1', element]); }
				}
			,	{	title: 'Menu 2'
				,	action: function() { console.log(['menu2', element]); }
				}
			]; 
		}
		
		function fillPackageList() {
			var items = []
			,	foundSelection = false;
			$.each(data.packageList, function(index, value){
				items.push('<div title="' + value + '"');
				items.push(' class="clickable');
				if (value === mcPackage) { items.push(' selected'); foundSelection = true; }
				items.push('">' + value + '</div>');
			});
			$('.pckgList', $tabPanel).empty().append(items.join(''));
			$('.pckgList .clickable', $tabPanel).click(function(event){
				if (setPackage($(this).text())) { update(); }
			});

			if (!foundSelection) { mcPackage = null; }
		}
		
		function fillClassCatTree() {
			if (!dictionary) { return }
			var openNodes = $('.classCats', $tabPanel)
					.find('li')
					.map(function(index, element) { return element.id; })
					.filter(function() { 
						return $('.classCats', $tabPanel).jstree('is_open', '#' + this); 
					})
			,	root = { 
					data: 'Categories'
				,	attr: { id: idOfClassCat(null), title: '' }
				,	children: [ ] 
				}
			;
			$.each(data.classCatList, function(i, each) { 
				addCategoryToNode(each, root, null); 
			});
			if (!idOfClassCat(classCategory)) { classCategory = null; }
			if ( root.children.length ) { root.state = 'open'; }
			$('.classCats', $tabPanel)
				.jstree({
					plugins: [ 'themes', 'json_data', 'ui' ]
				,	themes: { icons: false }
				,	json_data: { data: [ root ] }
				,	ui: { 
						select_limit: 1
					,	initially_select: [ idOfClassCat(classCategory) ] 
					}
				})
				.bind('select_node.jstree', function(event, data) {
					var path = $('.classCats', $tabPanel).jstree('get_selected').attr('title');
					if (setClassCat(path)) { update(); }
				});
			return;

			function addCategoryToNode(category, node, parentPath) {
				var index = category.indexOf('-')
				,	first = category.substring(0, 0 <= index ? index : category.length)
				,	rest = 0 <= index ? category.substring(index + 1) : null
				,	children = node.children
				,	myPath = (parentPath ? parentPath + '-' : '') + first
				,	child
				;
				$.each(children, function(index, each) {
					if (first === each.data) { child = each; }
				});
				if ( !child ) {
					var id = idOfClassCat(myPath)
					,	isOpen = false
					;
					$.each(openNodes, function(index, value) { 
						if (value === id) { isOpen = true; }
					});
					child = { 
						data: first 
					,	attr: { id: id, title: myPath }
					,	state: (isOpen ? 'open' : null)
					,	children: [ ]
					};
					children.push(child);
				}
				if (rest) {
					addCategoryToNode(rest, child, myPath);
				}
			}
		}

		function fillClassList() {
			var items = []
			,	foundSelection = false
			;
			$.each(data.classList, function(index, value){
				items.push('<div title="' + value + '"');
				items.push(' class="clickable');
				if (klass === value) { 
					foundSelection = true;
					items.push(' selected'); 
				}
				items.push('">' + value + '</div>');
			});
			if (!foundSelection) { klass = null; }
			$('.classList', $tabPanel).empty().append(items.join(''));
			$('.classList .clickable', $tabPanel).click(function(){
				if (setKlass($(this).text())) { update(); }
			});

			GemStone.menu({
				selector: $('.classList .clickable', $tabPanel)
			,	menu: classListMenu
			});
			return;
		}

		function classListMenu(element) { 
			var klass = $(element).text();
			return [
				{	title: 'Browse References'
				,	action: function() { 
						GemStone.browseReferencesTo(dictionary, klass); 
					}
				}
			,	{	title: 'File Out Class'
				,	action: function() { 
						alert('Coming soon!');
					}
				}
			,	{	title: 'Subclass Creation Template'
				,	action: function() { 
						alert('Coming soon!');
					}
				}
			,	{	title: 'Add Missing Accessors'
				,	action: function() { 
						alert('Coming soon!');
					}
				}
			,	{	title: 'Remove'
				,	action: function() { 
						if (confirm('Do you want to remove ' + klass + ' from ' + dictionary + '?')) {
							removeClass(dictionary, klass);
						}
					}
				}
			,	{	title: 'Remove Prior Versions'
				,	action: function() { 
						alert('Coming soon!');
					}
				}
			]; 
		}
		
		function removeClass(aDictName, aKlassName) {
			var myRequest = { dict: aDictName , klass: aKlassName };
			GemStone.ajax('POST','CodeBrowser/removeClass', myRequest, onRemoveClass);
			return;
			
			function onRemoveClass(data) {
				if (data.error) { 
					alert(data.error);
				} else {
					setKlass();
					update();
				}
			}
		}
		
		function fillSuperList() {
			var foundSelection = false;
			$('.superList', $tabPanel).empty();
			var items = [];
			if (!superClass) { 
				superClass = isMetaClass ? klass + ' class' : klass; 
			}
			$.each(data.superList, function(index, value){
				items.push('<option value="' + value + '"');
				items.push(' title="' + value + '"');
				if (superClass === value) { 
					foundSelection = true; 
					items.push(' selected'); 
				}
				items.push('>' + value + '</div>');
			});
			if ( !foundSelection ) { superClass = null; }
			$('.superList', $tabPanel).append(items.join(''));
			$('.superList').change(function(){
				if(setSuperClass($(this).val())) { update(); }
			});
		}

		function fillMethodFilterList() {
			var foundSelection = false;
			$('.methodFilterList', $tabPanel).empty();
			$.each(data.methodFilterList, function(index,value){
				if (methodFilter === value) { foundSelection = true; }
			});
			if ( !foundSelection ) { methodFilter = null; }
			if ( 0 === data.methodFilterList.length ) { return; }
			var items = [];
			items.push('<div title="" class="clickable');
			if (!foundSelection) { items.push(' selected'); }
			items.push('">* * ALL * *</div>');
			$.each(data.methodFilterList, function(index,value){
				items.push('<div title="' + value + '"');
				items.push(' class="clickable');
				if (value === methodFilter) { items.push(' selected'); }
				items.push('">' + value + '</div>');
			});
			$('.methodFilterList', $tabPanel).append(items.join(''));
			$('.methodFilterList .clickable', $tabPanel).click(function(){
				if (setMethodFilter($(this).text())) { update(); }
			});
		}

		function fillMethodList() {
			var foundSelection = false;
			$('.methodList', $tabPanel).empty();
			var items = [];
			$.each(data.methodList, function(index, value){
				items.push('<div');
				items.push(' title="' + value + '"');
				items.push(' class="clickable');
				if (selector === value) { 
					foundSelection = true;
					items.push(' selected'); 
				}
				items.push('">' + value + '</div>');
			});
			if (!foundSelection) { selector = null; }
			$('.methodList', $tabPanel).append(items.join(''));
			$('.methodList .clickable', $tabPanel).click(function(){
				if (setSelector($(this).text())) { update(); }
			});
			GemStone.menu({
				selector: $('.methodList .clickable', $tabPanel)
			,	menu: methodListMenu
			});
			return;
		}

		function methodListMenu(element) {
			var name = $(element).text();
			return [
				{	title: 'Remove Method'
				,	action: function() { 
						if (confirm('Do you want to remove #\'' + name + 
								'\' from ' + klass + 
								(isMetaClass ? ' class' : '') + 
								' in ' + dictionary + '?')) {
							removeMethod(dictionary, klass, isMetaClass, name);
						}
					}
				}
			,	{	title: 'Browse Implementors'
				,	action: function() { 
						GemStone.browseImplementorsOf(name); 
					}
				}
			,	{	title: 'Browse Implementors of ...'
				,	action: function() { 
						GemStone.browseImplementorsOf(); 
					}
				}
			,	{	title: 'Browse Senders'
				,	action: function() { 
						GemStone.browseSendersOf(name);
					}
				}
			,	{	title: 'Browse Senders of ...'
				,	action: function() { 
						GemStone.browseSendersOf();
					}
				}
			];

			function removeMethod(dictName, klassName, isMetaBool, methodName) {
				var myRequest = { 
					dict: 	dictName 
				,	klass: 	klassName
				,	isMeta:	isMetaBool
				,	name:	methodName
				};
				GemStone.ajax('POST','CodeBrowser/removeMethod', myRequest, onRemoveMethod);
				return;
				
				function onRemoveMethod(data) {
					if (data.error) { 
						alert(data.error);
					} else {
						setSelector();
						update();
					}
				}
				console.log([dictName, klassName, isMetaBool, methodName]);
			}

		}
		
		function fillImplementorList() {
			var foundSelection = false;
			$.each(data.implList, function(index,value){
				if (implementor === value) { foundSelection = true; }
			});
			if ( !foundSelection ) { implementor = data.implList[data.implList.length - 1]; }
			$('.implList', $tabPanel).empty();
			var items = [];
			$.each(data.implList, function(index, value){
				items.push('<option value="' + value + '"');
				items.push(' title="' + value + '"');
				if (implementor === value) { items.push(' selected'); }
				items.push('>' + value + '</div>');
			});
			$('.implList', $tabPanel).append(items.join(''));
			$('.implList').change(function(){
				if(setImplementor($(this).val())) { update(); }
			});
		}

		function fillEditArea() {
			if (!editor) { return; }
			if (data.method) {
				editor.setMethod(data.method, onSaveMethod);
			} else {
				editor.setCode(data.classDef, onSaveClass);
			}
		}
		
	}
	
	
	function onSaveClass(data) {
		setIsDictsTab(true);
		setDictionary(data.dict);
		if (!classCategory && classCategory !== data.cat) { setClassCat(''); }
		setKlass(data.name);
		update();
	}
	
	function onSaveMethod(data) {
		if (setSelector(data)) { update(); }
	}
	
	function isOkayToChange() {
		return editor ? editor.isOkayToChange() : true;
	}

	function idOfClassCat(aString) {
		if (!aString) { return 'classCat-0'; }
		var index = classCategories.indexOf(aString);
		if (-1 === index) {
			classCategories.push(aString);
			index = classCategories.length - 1;
		}
		return 'classCat-' + index;
	}

	function setIsDictsTab(value) {
		if (isDictsTab === value) { return false; }
		isDictsTab = value;
		setDictionary(null);
		setPackage(null);
		return true;
	}
	
	function setDictionary(value) {
		if (dictionary === value) { return false; }
		if (!isOkayToChange()) { return false; }
		dictionary = value; 
		setPackage(null);
		setClassCat(null);
		return true;
	}
	
	function setPackage(value) {
		if (mcPackage === value) { return false; }
		if (!isOkayToChange()) { return false; }
		mcPackage = value;
		setDictionary(null);
		setClassCat(null);
		return true;
	}

	function setClassCat(value) {
		if (value === '') { return setClassCat(null); }
		if (classCategory === value) { return false; }
		if (!isOkayToChange()) { return false; }
		classCategory = value; 
		setKlass(null);
		return true;
	}

	function setKlass(value) { 
		if (klass === value) { return false; }
		if (!isOkayToChange()) { return false; }
		klass = value;
		setSuperClass(null);
		setMethodFilter(null);
		setSelector(null);
		return true;
	}
	
	function setIsMetaClass(value) {
		if (isMetaClass === value) { return false; }
		if (!isOkayToChange()) { return false; }
		isMetaClass = value;
		setSuperClass(null);
		setMethodFilter(null);
		return true;
	}
	
	function setSuperClass(value) {
		if (superClass === value) { return false; }
		if (!isOkayToChange()) { return false; }
		superClass = value;
		return true;
	}

	function setMethodFilterBy(value) {
		if (methodFilterBy === value) { return false; }
		if (!isOkayToChange()) { return false; }
		methodFilterBy = value;
		setMethodFilter(null);
		return true;
	}
	
	function setMethodFilter(value) {
		if (methodFilter === value) { return false; }
		if (!isOkayToChange()) { return false; }
		methodFilter = value; 
		setSelector(null);
		return true;
	}

	function setSelector(value) {
		if (selector === value) { return false; }
		if (!isOkayToChange()) { return false; }
		selector = value;
		return true;
	}
	
	function setImplementor(value) {
		if (implementor === value) { return false; }
		if (!isOkayToChange()) { return false; }
		implementor = value;
		return true;
	}
	
});
