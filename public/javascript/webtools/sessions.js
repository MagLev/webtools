$(document).ready(function() {
  return $.getJSON("" + WebTools.location + "/sessions", function(data) {
    var error, field, row, session, tbody, thead, _i, _j, _len, _len2, _ref;
    error = data['_error'];
    if (error != null) {
      console.log(data);
      return alert(error + ' (see console log for stack)');
    } else {
      thead = $('#sessionHeaders').children("tr");
      $(data.headers).each(function(idx, element) {
        return thead.append("<th title='" + element[1] + "'>" + element[0] + "</th>");
      });
      tbody = $('#sessionTable');
      _ref = data.report;
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        session = _ref[_i];
        row = $('<tr>');
        for (_j = 0, _len2 = session.length; _j < _len2; _j++) {
          field = session[_j];
          row.append("<td>" + field + "</td>");
        }
        tbody.append(row);
      }
      return $('.timestamp').html("Report generated " + data.timestamp);
    }
  });
});