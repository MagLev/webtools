$(document).ready ->
  $.getJSON "#{WebTools.location}/version", (data) ->
    error = data['_error']
    if error?
      console.log(data)
      alert(error + ' (see console log for stack)')
    else
      thead = $('#versionHeaders')
      $(data.headers).each (idx, element) ->
        thead.append("<th title='#{element[1]}'>#{element[0]}</th>")
      tbody = $('#versionTable')
      $.each data.report, (k, v) ->
        tbody.append("<tr><th>#{k}</th><td>#{v[0]}</td><td>#{v[1]}</td></tr>")
      $('#versionTimestamp').html("Report generated #{data.timestamp}")
