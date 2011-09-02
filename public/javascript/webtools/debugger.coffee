window.escapeHTML = (str) ->
  str = "" unless str?
  str += str unless typeof str == "string"
  $("<i></i>").text(str).html()

class Frame
  constructor: (@server, @pid, @frame_idx, @container) ->

  update_detail_view: (objectInfo) ->
    @inspector.hide()
    $('#objInfoClass').text(objectInfo['(__class__)'])
    $('#objInfoValue').text(objectInfo['(__self__)'])
    this.renderTableData '#objInstVars', objectInfo, (idx, data) ->
      $("<tr><td>#{idx}</td><td>#{data}</td></tr>")
    @inspector.show()

  renderTableData: (tableId, object, formatFn) ->
    ui = $(tableId + ' tbody')
    ui.empty()
    $.each object, (idx, data) ->
      ui.append(formatFn(idx, data))

  create_detail_view: ->
    @inspector = $('#objectInspector')
    @inspector.remove()
    @container.append(@inspector)
    @inspector.removeClass('hidden')

  create_source_code_holder: () ->
    @source = $(document.createElement("script"))
    @source.attr
      type: "syntaxhighlighter",
      class: "brush: ruby"
    @container.prepend(@source)
    @source

  create_inspector: (object, index, path) ->
    index = 0 unless index?
    path = "#{@server}/process/#{@pid}/frames/#{@frame_idx}" unless path?
    options = $([])
    $.each object, (key, value) ->
      if "#{key}" == "object"
        options.push("#{value}")
      else unless "#{key}" == "__proto__"
        options.push("#{key}")
    if not @inspector_div?
      @inspector_div = $(document.createElement("div"))
      @inspector_div.addClass("inspectors")
      @container.prepend(@inspector_div)
    if @inspectors[index]?
      inspector = @inspectors[index]
    else
      @inspectors[index] = (inspector = $(document.createElement("select")))
      @inspector_div.append(inspector)
      inspector.attr
        multiple: true
        class: 'inspector'
    inspector.html("")
    $(options).each (idx, o) =>
      inspector.append("<option value=#{idx}>#{escapeHTML(o)}</option>")
    this.create_evaluator(path)
    inspector.bind "change", =>
      value = options[inspector.val()]
      url = "#{path}/objects/#{value}"
      for i in @inspectors.slice(index + 1)
        i.html("")
      $.get url, (object) =>
        this.create_inspector(object, index + 1, url)
        this.update_detail_view(object)
        SyntaxHighlighter.highlight()
      , 'json'

  create_evaluator: (path) ->
    unless @evaluator?
      @evaluator = $(document.createElement("input"))
      @evaluator.attr
        type: "text"
        name: "#{@frame_idx}_evaluator"
        id: "#{@frame_idx}_evaluator"
        class: "ui-widget-content ui-corner-all"
      @container.prepend(@evaluator)
    @evaluator.unbind("keypress")
    @evaluator.bind "keypress", (e) =>
      code = e.keyCode if e.keyCode?
      code = e.which unless code
      if code == 13 # RETURN
        $.post path,
          data:
            "do-it": @evaluator.val()
        , (object) =>
          @evaluator.val("#{@evaluator.val()} => #{object['(__self__)']}")
          @evaluator.select()
        , 'json'

  render: ->
    @container.html("")
    @inspectors = []
    this.create_source_code_holder()
    $.get "#{@server}/process/#{@pid}/frames/#{@frame_idx}", (frame) =>
      @source.html("<![CDATA[\n#{frame.debug_info.source}\n]]>")
      SyntaxHighlighter.highlight()
      this.create_inspector(frame.debug_info.context)
      this.create_detail_view()
    , 'json'

class Process
  constructor: (@server, @pid, @tab) ->
    @info_div = $("##{@tab} .info-bar")
    @stack_div = $("##{@tab} .frame-list")

  render: ->
    this.render_info()
    this.render_stack()

  render_info: ->
    request = $.get "#{@server}/process/#{@pid}", (data) =>
      @info_div.text("#{data.label} (#{data.timestamp})")
    , 'json'

  render_stack: ->
    @stack_div.html("")
    request = $.get "#{@server}/process/#{@pid}/frames", (framelist) =>
      $(framelist).each (idx, f) =>
        header = $(document.createElement("h3"))
        link = $(document.createElement("a"))
        link.attr
          href: '#'
          data_idx: idx
        link.text("#{f.class}##{f.method_name}")
        link.append("<small>#{escapeHTML(f.source_location)}</small>")
        header.html(link)
        div = document.createElement("div")
        $(div).text("Waiting for data...")
        @stack_div.append(header)
        @stack_div.append(div)
        if idx == 0
          @selected_frame = new Frame(@server, @pid, 0, $(div))
          @selected_frame.render()
      @stack_div.accordion
        clearStyle: true
        collapsible: true
        changestart: (event, ui) =>
          frame_idx = ui.newHeader.children("a").attr("data_idx")
          if frame_idx?
            @selected_frame = new Frame(@server, @pid, frame_idx, ui.newContent)
            @selected_frame.render()
    , 'json'

class Debugger
  constructor: (@server) ->
    @tab_content_template = $("#tab_content_template")

  toString: ->
    "Debugger on #{server}"

  server_alive: () ->
    request = $.ajax
      url: "#{@server}/process"
      async: false
    request.status == 200

  clone_template: () ->
    @content = @tab_content_template.clone()
    @content.show()
    @content.removeAttr("id")

  fill_process_selector: () ->
    process_box = @content.children("select[name='process-select-box']")
    $.getJSON "#{@server}/process", (errors) ->
      $(errors).each (idx, e) ->
        process_box.append("<option value='#{e.process_id}'>#{e.process_id}: #{escapeHTML(e.label)}</option>")
    process_box.bind "change", =>
      @process = new Process(@server, process_box.val(), @tab)
      @process.render()

  content_for: (ui_panel) ->
    if not @content?
      @tab = $(ui_panel).attr("id")
      return "<p>The URL " + url + " could not be reached</p>" unless this.server_alive()
      this.clone_template()
      this.fill_process_selector()
      ui_panel.append(@content)
    @content

DebuggerApp =
  setup: ->
    debuggers = []
    tab_server_input = $("#tab_server")
    tab_counter = 2

    # actual addTab function: adds new tab using the title input
    # from the form above
    add_tab = ->
      tab_server = tab_server_input.val()
      tabs.tabs("add", "#tabs-#{tab_counter}", tab_server)
      tab_counter++

    # tabs init with a custom tab template and an "add" callback
    # filling in the content
    tabs = $("#tabs").tabs
      tabTemplate: '<li><a href=\'#{href}\'>#{label}</a>' +
        "<span class='ui-icon ui-icon-close'>Remove Tab</span></li>"
      add: (event, ui) ->
        new_debugger = new Debugger(tab_server_input.val())
        debuggers.push(new_debugger)
        new_debugger.content_for($(ui.panel))

    # modal dialog init: custom buttons and a "close" callback
    # reseting the form inside
    dialog = $("#dialog").dialog
      autoOpen: false
      modal: true
      buttons:
        Add: ->
          add_tab()
          dialog.dialog("close")
        Cancel: ->
          dialog.dialog("close")
      open: ->
        tab_server_input.focus()
      close: ->
        form[0].reset()

    # addTab form: calls addTab function on submit and closes the
    # dialog
    form = $("form", this.dialog).submit ->
      add_tab()
      dialog.dialog("close")
      false

    # addTab button: just opens the dialog
    $("#add_tab").button().click ->
      dialog.dialog("open");

    # close icon: removing the tab on click
    $("#tabs span.ui-icon-close").live "click", ->
      index = $("li", tabs).index($(this).parent())
      tabs.tabs("remove", index);

$(document).ready ->
  DebuggerApp.setup()
