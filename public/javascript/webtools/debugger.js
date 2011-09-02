var Debugger, DebuggerApp, Frame, Process;
var __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };
window.escapeHTML = function(str) {
  if (str == null) {
    str = "";
  }
  if (typeof str !== "string") {
    str += str;
  }
  return $("<i></i>").text(str).html();
};
Frame = (function() {
  function Frame(server, pid, frame_idx, container) {
    this.server = server;
    this.pid = pid;
    this.frame_idx = frame_idx;
    this.container = container;
  }
  Frame.prototype.create_source_code_holder = function() {
    this.source = $(document.createElement("script"));
    this.source.attr({
      type: "syntaxhighlighter",
      "class": "brush: ruby"
    });
    this.container.prepend(this.source);
    return this.source;
  };
  Frame.prototype.create_inspector = function(object, index, path) {
    var inspector, options;
    if (index == null) {
      index = 0;
    }
    if (path == null) {
      path = "" + this.server + "/process/" + this.pid + "/frames/" + this.frame_idx;
    }
    options = $([]);
    $.each(object, function(key, value) {
      if (("" + key) === "object") {
        return options.push("" + value);
      } else if (("" + key) !== "__proto__") {
        return options.push("" + key);
      }
    });
    if (!(this.inspector_div != null)) {
      this.inspector_div = $(document.createElement("div"));
      this.inspector_div.addClass("inspectors");
      this.container.prepend(this.inspector_div);
    }
    if (this.inspectors[index] != null) {
      inspector = this.inspectors[index];
    } else {
      this.inspectors[index] = (inspector = $(document.createElement("select")));
      this.inspector_div.append(inspector);
      inspector.attr({
        multiple: true,
        "class": 'inspector'
      });
    }
    inspector.html("");
    $(options).each(__bind(function(idx, o) {
      return inspector.append("<option value=" + idx + ">" + (escapeHTML(o)) + "</option>");
    }, this));
    this.create_evaluator(path);
    return inspector.bind("change", __bind(function() {
      var i, url, value, _i, _len, _ref;
      value = options[inspector.val()];
      url = "" + path + "/objects/" + value;
      _ref = this.inspectors.slice(index + 1);
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        i = _ref[_i];
        i.html("");
      }
      return $.get(url, __bind(function(object) {
        this.create_inspector(object, index + 1, url);
        return SyntaxHighlighter.highlight();
      }, this), 'json');
    }, this));
  };
  Frame.prototype.create_evaluator = function(path) {
    if (this.evaluator == null) {
      this.evaluator = $(document.createElement("input"));
      this.evaluator.attr({
        type: "text",
        name: "" + this.frame_idx + "_evaluator",
        id: "" + this.frame_idx + "_evaluator",
        "class": "ui-widget-content ui-corner-all"
      });
      this.container.prepend(this.evaluator);
    }
    this.evaluator.unbind("keypress");
    return this.evaluator.bind("keypress", __bind(function(e) {
      var code;
      if (e.keyCode != null) {
        code = e.keyCode;
      }
      if (!code) {
        code = e.which;
      }
      if (code === 13) {
        return $.post(path, {
          data: {
            "do-it": this.evaluator.val()
          }
        }, __bind(function(object) {
          return this.evaluator.val("" + (this.evaluator.val()) + " => " + object['(__self__)']);
        }, this), 'json');
      }
    }, this));
  };
  Frame.prototype.render = function() {
    this.container.html("");
    this.inspectors = [];
    this.create_source_code_holder();
    return $.get("" + this.server + "/process/" + this.pid + "/frames/" + this.frame_idx, __bind(function(frame) {
      this.source.html("<![CDATA[\n" + frame.debug_info.source + "\n]]>");
      SyntaxHighlighter.highlight();
      return this.create_inspector(frame.debug_info.context);
    }, this), 'json');
  };
  return Frame;
})();
Process = (function() {
  function Process(server, pid, tab) {
    this.server = server;
    this.pid = pid;
    this.tab = tab;
    this.info_div = $("#" + this.tab + " .info-bar");
    this.stack_div = $("#" + this.tab + " .frame-list");
  }
  Process.prototype.render = function() {
    this.render_info();
    return this.render_stack();
  };
  Process.prototype.render_info = function() {
    var request;
    return request = $.get("" + this.server + "/process/" + this.pid, __bind(function(data) {
      return this.info_div.text("" + data.label + " (" + data.timestamp + ")");
    }, this), 'json');
  };
  Process.prototype.render_stack = function() {
    var request;
    this.stack_div.html("");
    return request = $.get("" + this.server + "/process/" + this.pid + "/frames", __bind(function(framelist) {
      $(framelist).each(__bind(function(idx, f) {
        var div, header, link;
        header = $(document.createElement("h3"));
        link = $(document.createElement("a"));
        link.attr({
          href: '#',
          data_idx: idx
        });
        link.text("" + f["class"] + "#" + f.method_name);
        link.append("<small>" + (escapeHTML(f.source_location)) + "</small>");
        header.html(link);
        div = document.createElement("div");
        $(div).text("Waiting for data...");
        this.stack_div.append(header);
        this.stack_div.append(div);
        if (idx === 0) {
          this.selected_frame = new Frame(this.server, this.pid, 0, $(div));
          return this.selected_frame.render();
        }
      }, this));
      return this.stack_div.accordion({
        clearStyle: true,
        collapsible: true,
        changestart: __bind(function(event, ui) {
          var frame_idx;
          frame_idx = ui.newHeader.children("a").attr("data_idx");
          if (frame_idx != null) {
            this.selected_frame = new Frame(this.server, this.pid, frame_idx, ui.newContent);
            return this.selected_frame.render();
          }
        }, this)
      });
    }, this), 'json');
  };
  return Process;
})();
Debugger = (function() {
  function Debugger(server, process_id) {
    this.server = server;
    this.process_id = process_id;
    this.tab_content_template = $("#tab_content_template");
  }
  Debugger.prototype.toString = function() {
    return "Debugger on " + server;
  };
  Debugger.prototype.server_alive = function() {
    var request;
    request = $.ajax({
      url: "" + this.server + "/process",
      async: false
    });
    return request.status === 200;
  };
  Debugger.prototype.clone_template = function() {
    this.content = this.tab_content_template.clone();
    this.content.show();
    return this.content.removeAttr("id");
  };
  Debugger.prototype.fill_process_selector = function() {
    var process_box;
    process_box = this.content.children("select[name='process-select-box']");
    $.getJSON("" + this.server + "/process", function(errors) {
      return $(errors).each(function(idx, e) {
        return process_box.append("<option value='" + e.process_id + "'>" + (escapeHTML(e.label)) + "</option>");
      });
    });
    if ((this.process_id != null) && this.process_id !== "") {
      $.getJSON("" + this.server + "/process/" + this.process_id, function(p) {
        return process_box.append("<option value='" + p.process_id + "'>" + (escapeHTML(p.label)) + "</option>");
      });
    }
    return process_box.bind("change", __bind(function() {
      this.process = new Process(this.server, process_box.val(), this.tab);
      return this.process.render();
    }, this));
  };
  Debugger.prototype.content_for = function(ui_panel) {
    if (!(this.content != null)) {
      this.tab = $(ui_panel).attr("id");
      if (!this.server_alive()) {
        return "<p>The URL " + url + " could not be reached</p>";
      }
      this.clone_template();
      this.fill_process_selector();
      ui_panel.append(this.content);
    }
    return this.content;
  };
  return Debugger;
})();
DebuggerApp = {
  setup: function() {
    var add_tab, debuggers, dialog, form, tab_counter, tab_process_input, tab_server_input, tabs;
    debuggers = [];
    tab_server_input = $("#tab_server");
    tab_process_input = $("#tab_process");
    tab_counter = 2;
    add_tab = function() {
      var tab_server;
      tab_server = tab_server_input.val();
      tabs.tabs("add", "#tabs-" + tab_counter, tab_server);
      return tab_counter++;
    };
    tabs = $("#tabs").tabs({
      tabTemplate: '<li><a href=\'#{href}\'>#{label}</a>' + "<span class='ui-icon ui-icon-close'>Remove Tab</span></li>",
      add: function(event, ui) {
        var new_debugger;
        new_debugger = new Debugger(tab_server_input.val(), tab_process_input.val());
        debuggers.push(new_debugger);
        return new_debugger.content_for($(ui.panel));
      }
    });
    dialog = $("#dialog").dialog({
      autoOpen: false,
      modal: true,
      buttons: {
        Add: function() {
          add_tab();
          return dialog.dialog("close");
        },
        Cancel: function() {
          return dialog.dialog("close");
        }
      },
      open: function() {
        return tab_server_input.focus();
      },
      close: function() {
        return form[0].reset();
      }
    });
    form = $("form", this.dialog).submit(function() {
      add_tab();
      dialog.dialog("close");
      return false;
    });
    $("#add_tab").button().click(function() {
      return dialog.dialog("open");
    });
    return $("#tabs span.ui-icon-close").live("click", function() {
      var index;
      index = $("li", tabs).index($(this).parent());
      return tabs.tabs("remove", index);
    });
  }
};
$(document).ready(function() {
  return DebuggerApp.setup();
});