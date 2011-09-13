/*
        webtools.js
*/

GemStone = function() { //      hide everything inside an anonymous function to isolate namespace
        var requestCount = 0    //      keep a cumulative count of round-trips to server
        ,       tools = {}
        ,       $tabs
        ,       $statusBar
        ,       nextId = 0
        ,       scripts = {}
        ;

        $(document).ready(onDocumentReady);

        return {
                addTab:         addTab          //      add a new tab panel
        ,       ajax:           ajax            //      wrapper for server call
        ,       browseImplementorsOf:   browseImplementorsOf
        ,       browseReferencesTo:     browseReferencesTo
        ,       browseSendersOf:                browseSendersOf
        ,       loadCSS:        loadCSS         //      add CSS link to head if not already present
        ,       menu:           menu            //      popup a dynamic context menu
        ,       nextId:         getNextId       //      unique HTML element identifier
        ,       runJs:          runJs           //      run a script
        ,       runJsOnce:      runJsOnce       //      run a script only if not yet run
        ,       saveScript:     saveScript      //      cache function to reuse without reloading
        ,       scroll:         scroll          //      add scrolling to a table
        ,       encodeHTML:     encodeHTML      //      escape for HTML
        ,       decodeHTML:     decodeHTML      //      unescape HTML
        }

        function onDocumentReady() {
                $tabs = $('#tabs');
                $statusBar = $('#statusBar');
                createMainTabs();
                ajax('GET', 'tools', null, gotTools);
                $(window).resize(resize);       //      register to get resize events
                return;

                function createMainTabs() {
                        $tabs.tabs({ show: showTab });
                        $('.ui-tabs-nav', $tabs)
                                .removeClass('ui-corner-all')
                                .addClass('ui-corner-top');
                        return;

                        function showTab(event, ui) {
                                var $tab = $('#' + ui.panel.id)
                                ,       tool = $tab.data('tool')
                                ;
                                $(window).resize();
                                if ( tool && tool.onShow ) {
                                        tool.onShow($tab[0]);
                                }
                        }
                }

                function gotTools(json) {
                        var items = [];
                        $.each(json.tools, function(){
                                items.push('<tr class="clickable" title="' + this.file + '">');
                                items.push('<td>' + this.name + '</td>');
                                items.push('<td>' + this.description + '</td>');
                                items.push('</tr>');
                        });
                        $('#homePanel tbody').empty().append(items.join(''));
                        $('#homePanel tbody tr').click(onClick);
                        return;

                        function onClick(event) {
                                event.preventDefault();
                                ajax(
                                        'GET'
                                ,       $(this).attr('title')
                                ,       null
                                ,       function(html) { $('body').append(html); }
                                );
                        }
                }
        }

        function addTab(tool) {
                var selector = '#' + getNextId();
                $('#' + tool.id).attr('id', selector.substring(1));
                moveOnlyOnce($(selector));
                $tabs.tabs('add', selector, tool.label);
                var $listItem = $('> ul > li:last', $tabs);
                $listItem.attr('title', tool.title);
                addCloseButton($listItem);
                $(selector).data('tool', tool);
                $tabs.tabs('select', selector);
                if (tool.onAdd) { tool.onAdd($(selector)); }
                return;

                function addCloseButton($listItem) {
                        $listItem.append('<a href="#" class="closeTab">X</a>');
                        $('a.closeTab', $listItem).click(removeTab);
                        return;

                        function removeTab(event) {
                                var list = $listItem.parent().children('li')
                                ,       index = list.index($listItem)
                                ;
                                $(selector).remove();
                                $tabs.tabs('remove', index);
                                event.preventDefault();
                        }
                };

                // a panel (such as CodeBrowser) could appear multiple times
                // but there are some parts of it that we want to exist only once (e.g. css)
                function moveOnlyOnce($tabPanel) {
                        var     $div = $('.once', $tabPanel)
                        ,       newClass = $div.attr('id')
                        ;
                        $div.addClass(newClass);
                        if (1 === $('.' + newClass).length) {
                                $div.attr('id', null);
                                $('style', $div).appendTo('head');
                                $('style', $div).remove();
                                if ($div.html().replace(/(^\s+)(\s+$)/, "").length) {
                                        $('body').append($div);
                                }
                        } else {
                                $div.remove();
                        }
                }
        }

        function browseMethods(data) {
                ajax(
                        'GET'
                ,       'MethodList.html'
                ,       null
                ,       function(html) {
                                var string = html.replace('SEARCH_OBJECT', JSON.stringify(data));
                                $('body').append(string);
                        }
                );
        }

        function browseImplementorsOf(anObject) {
                var name = anObject ? anObject : prompt('Browse implementors of?');
                if (!name) { return; }
                browseMethods({
                        type: 'implementors'
                ,       find: name
                ,       label: 'Implementors of ' + name
                });
        }

        function browseReferencesTo(dict, klass) {
                browseMethods({
                        type: 'referencesToGlobal'
                ,       dict: dict
                ,       find: klass
                ,       label: 'References to ' + klass
                });
        }

        function browseSendersOf(anObject) {
                var name = anObject ? anObject : prompt('Browse senders of?');
                if (!name) { return; }
                browseMethods({
                        type: 'senders'
                ,       find: name
                ,       label: 'Senders of ' + name
                });
        }

        function ajax(type, url, args, callback) {
                var startTime = new Date().getTime();
                $statusBar.text('Sent request #' + (++requestCount) + ' for ' + url);
                $.ajax({
                        type: type
                ,       url: url
                ,       data: args
                ,       success: success
                });
/*
                console.log([type, url, args]);
                if (type === 'GET' && args) {
                        var delimiter = '?'
                        ,       items = new Array();
                        items.push(url);
                        $.each(args, function(key, value) {
                                items.push(delimiter);
                                items.push(key);
                                items.push('=');
                                items.push(value);
                                delimiter = '&';
                        });
                        history.pushState(null, '', items.join(''));
                }
*/
                return;

                function success(json) {
                        var serverTime = json._time;
                        var networkTime = new Date().getTime() - startTime - serverTime;
                        startTime = new Date().getTime();
                        var error = json._error;
                        if (error) {
                                alert(error + ' (see console log for stack)');
                                console.log(error + '\r\n' + json._stack);
                        } else {
                                if (callback) { (callback)(json); }
                        };
                        var elapsed = new Date().getTime() - startTime;
                        $statusBar.text(
                                'Request for ' + url + ' (roundtrip #' + requestCount + ') took ' +
                                serverTime + ' ms on server, ' + networkTime + ' ms on the network, and '
                                + elapsed + ' ms on the client.'
                        );
                };
        }

        function getNextId() {
                nextId = nextId + 1;
                return 'id' + nextId;
        }

        function loadCSS(href) {
                if ($('head link[href="' + href + '"]')[0]) { return; }
                $('head').append('<link rel="stylesheet" type="text/css" href="' + href + '"/>');
        }

        function menu(options) {
                options.selector.jjmenu(
                        'rightClick'
                ,       [ {
                                getByFunction: function() {
                                        var items = options.menu(triggerElement);
                                        $.each(items, function(index, each) {
                                                each.action = {
                                                        type: 'xfn'
                                                ,       callback: each.action
                                                };
                                        });
                                        return items;
                                }
                        } ]
                ,       null    //      userData
                ,       { xposition: 'mouse', yposition: 'mouse' }
                );
        }

        function saveScript(src, aFunction) {
                scripts[src] = aFunction;
        }

        //      runJs(sourcePath)
        //      runJs(sourcePath, arguments)
        //      runJs(sourcePath, successFunction)
        //      runJs(sourcePath, arguments, successFunction)
        function runJs(src, arg1, arg2) {
                var args
                ,       done
                ,       result
                ;
                if (typeof arg1 === 'function') {
                        done = arg1;
                } else {
                        args = arg1;
                        done = arg2;
                }
                if (typeof scripts[src] === 'function') {
                        result = scripts[src](args);
                        if (done) { done(result); }
                } else {
                        scripts[src] = null;
                        runJsOnce(src, almostDone);
                }
                return;

                function almostDone() {
                        if (typeof scripts[src] === 'function') {
                                result = scripts[src](args);
                        }
                        if (done) { done(result); }
                }
        }

        function runJsOnce(src, done) {
                if (scripts[src]) {
                        if (done) { done(null); }
                } else {
                        scripts[src] = true;
                        $.getScript(src, function() { done(null); });
                }
        }

        function scroll($div) {
                if ( 1 === $('table', $div).length ) {
                        var table = $('table', $div).html();
                        $div.empty().append(
                                '<div class="tableHeader"><table>' + table + '</table></div>' +
                                '<div class="tableBody"><table>' + table + '</table></div>'
                                );
                        $('.tableHeader tbody', $div).remove();
                        $('.tableBody caption', $div).remove();
                        $div.addClass('scrollingTable');
                }
        }

        function encodeHTML(string) {
                return $('<div/>').text(string).html();
        }

        function decodeHTML(string) {
                return $('<div/>').html(string).text();
        }

        function resize() {
                var $tables = $('.ui-tabs-panel .scrollingTable')
                                                .not('.ui-tabs-hide .scrollingTable');
                $.each($tables, function() {
                        setColumnWidths($('.tableHeader table', this), $('.tableBody table', this));
                });
                return;

                function setColumnWidths($header, $body) {
                        if (0 === $('thead tr th.filler', $header).length) {
                                $('thead tr', $header).append('<th class="filler"></th>');
                        }
                        var emptyBody = 0 === $('tbody tr', $body).length
                        ,       columnCount = $('thead tr th', $body).length - 1
                        ;
                        $('thead', $body).removeClass('hide');
                        clearWidths($header);
                        $('thead tr th.filler', $header).css('width', '18px');
                        clearWidths($body);
                        if (emptyBody) {
                                $('tbody', $body).append('<tr />');
                                for (var i = 0; i < columnCount; ++i) {
                                        $('tbody tr', $body).append('<td>&nbsp</td>');
                                }
                        }
                        var widths = $.map($('tbody tr:first td', $body), calculateWidth);
                        setWidths($body);
                        widths[0] = widths[0] + 1;
                        widths.push(18);
                        setWidths($header);
                        if (emptyBody) {
                                $('tbody', $body).empty();
                        }
                        $('thead', $body).addClass('hide');
                        $body.parent().css('top', $header.outerHeight() - 3 + 'px');
                        $body.
                        return;

                        function clearWidths($element) {
                                $('thead tr:first th', $element).each(function() {
                                        $(this).removeAttr('style');
                                });
                                $element.removeAttr('style');
                        }
                        function setWidths($element) {
                                var total = 0;
                                $('thead tr:first th', $element).each(function(index) {
                                        $(this).css({ width: widths[index] + 'px' });
                                        total = total + widths[index];
                                });
                                $element.css({'table-layout': 'fixed', 'width': total});
                        }
                        function calculateWidth(td) {
                                if ($.browser.msie)     { return $(td).outerWidth() - 1;        }
                                if ($.browser.mozilla)  { return $(td).width();                         }
                                return $(td).outerWidth();
                        }
                }
        }

}();
