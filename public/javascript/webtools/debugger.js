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
  Frame.prototype.update_detail_view = function(objectInfo) {
    this.inspector.hide();
    this.inspector.find('.objInfoClass').text(objectInfo['class']);
    this.inspector.find('.objInfoValue').text(objectInfo['inspect']);
    this.renderTableData(this.inspector.children('.objInstVars'), objectInfo.instance_variables, function(idx, data) {
      return $("<tr><td>" + idx + "</td><td>" + data + "</td></tr>");
    });
    return this.inspector.show();
  };
  Frame.prototype.renderTableData = function(table, object, formatFn) {
    var ui;
    ui = table.children('tbody');
    ui.empty();
    return $.each(object, function(key, value) {
      if (key.indexOf("@") === 0) {
        return ui.append(formatFn(key, value));
      }
    });
  };
  Frame.prototype.create_detail_view = function() {
    this.inspector = $('#objectInspector').clone();
    this.inspector.removeAttr('id');
    this.inspector.removeClass('hidden');
    return this.container.append(this.inspector);
  };
  Frame.prototype.create_source_code_holder = function() {
    this.container.prepend(window.editorDiv);
    window.editor.save_url = "" + this.server + "/process/" + this.pid + "/frames/" + this.frame_idx;
    return window.editorDiv.show();
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
    if (object.instance_variables != null) {
      $.each(object.instance_variables, function(key, value) {
        return options.push("" + key);
      });
    } else {
      $.each(object, function(key, value) {
        return options.push("" + key);
      });
    }
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
        this.create_inspector(object.instance_variables, index + 1, url);
        return this.update_detail_view(object);
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
        "class": "ui-widget-content ui-corner-all",
        style: 'width: 100%;'
      });
      this.container.prepend(this.evaluator);
    }
    this.evaluator.bind("focus", __bind(function() {}, this));
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
        return $.ajax({
          url: path,
          data: {
            "do-it": this.evaluator.val()
          },
          success: __bind(function(data) {
            var result;
            result = data["do-it-result"];
            this.evaluator.val("" + (this.evaluator.val()) + " => " + result['inspect']);
            this.update_detail_view(result);
            return this.evaluator.select();
          }, this),
          dataType: 'json',
          type: 'PUT'
        });
      }
    }, this));
  };
  Frame.prototype.render = function() {
    this.container.html("");
    this.inspectors = [];
    this.create_source_code_holder();
    return $.get("" + this.server + "/process/" + this.pid + "/frames/" + this.frame_idx, __bind(function(frame) {
      editor.getSession().setValue(frame.debug_info.source);
      this.create_inspector(frame.debug_info.context);
      return this.create_detail_view();
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
        var div, header, link, restartLink;
        header = $(document.createElement("h3"));
        link = $(document.createElement("a"));
        link.attr({
          href: '#',
          data_idx: idx
        });
        link.text("" + f["class"] + "#" + f.method_name);
        link.append("<small>" + (escapeHTML(f.source_location)) + "</small>");
        header.append(link);
        restartLink = $('<a href="#">Restart frame</a>');
        restartLink.bind("click", __bind(function(e) {
          $.ajax({
            url: "" + this.server + "/process/" + this.pid + "/frames/" + idx,
            type: 'DELETE',
            success: __bind(function() {
              return this.render();
            }, this)
          });
          return e.preventDefault();
        }, this));
        header.append(restartLink);
        div = document.createElement("div");
        $(div).text("Waiting for data...");
        this.stack_div.append(header);
        this.stack_div.append(div);
        if (idx === 0) {
          this.selected_frame = new Frame(this.server, this.pid, 0, $(div));
          return this.selected_frame.render();
        }
      }, this));
      this.stack_div.accordion("destroy");
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
  function Debugger(server) {
    this.server = server;
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
    var refreshButton;
    this.process_box = this.content.children("select[name='process-select-box']");
    $.getJSON("" + this.server + "/process", __bind(function(errors) {
      return $(errors).each(__bind(function(idx, e) {
        return this.process_box.append("<option value='" + e.process_id + "'>" + e.process_id + ": " + (escapeHTML(e.label)) + "</option>");
      }, this));
    }, this));
    this.process_box.bind("change", __bind(function() {
      this.process = new Process(this.server, this.process_box.val(), this.tab);
      return this.process.render();
    }, this));
    refreshButton = this.content.children(".reload-button");
    return refreshButton.bind("click", __bind(function(e) {
      e.preventDefault();
      this.process = new Process(this.server, this.process_box.val(), this.tab);
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
    var add_tab, debuggers, dialog, form, tab_counter, tab_server_input, tabs;
    debuggers = [];
    tab_server_input = $("#tab_server");
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
        new_debugger = new Debugger(tab_server_input.val());
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
  DebuggerApp.setup();
  window.RubyMode = require("ace/mode/ruby").Mode;
  window.canon = require('pilot/canon');
  window.editorDiv = $("#editor");
  window.editor = ace.edit('editor');
  window.editor.getSession().setUseSoftTabs(true);
  window.editor.getSession().setMode(new RubyMode());
  return canon.addCommand({
    name: "save",
    bindKey: {
      win: "Ctrl-S",
      mac: "Command-S",
      sender: "editor"
    },
    exec: function() {
      return $.ajax({
        url: window.editor.save_url,
        type: 'PUT',
        data: {
          debug_info: {
            source: window.editor.getSession().getValue()
          }
        },
        success: function() {
          alert('Save successful. The stack has been reset to the new method.');
          return $("#tabs").select(".reload-button").filter(':visible').click();
        }
      });
    }
  });
});