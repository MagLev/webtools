$(document).ready(function() {
  return $.getJSON("" + WebTools.location + "/version", function(data) {
    var error, tbody, thead;
    error = data['_error'];
    if (error != null) {
      console.log(data);
      return alert(error + ' (see console log for stack)');
    } else {
      thead = $('#versionHeaders');
      $(data.headers).each(function(idx, element) {
        return thead.append("<th title='" + element[1] + "'>" + element[0] + "</th>");
      });
      tbody = $('#versionTable');
      $.each(data.report, function(k, v) {
        return tbody.append("<tr><th>" + k + "</th><td>" + v[0] + "</td><td>" + v[1] + "</td></tr>");
      });
      return $('#versionTimestamp').html("Report generated " + data.timestamp);
    }
  });
});