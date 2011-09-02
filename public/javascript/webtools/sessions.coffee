$(document).ready ->
  $.getJSON "#{WebTools.location}/sessions", (data) ->
    error = data['_error']
    if error?
      console.log(data)
      alert(error + ' (see console log for stack)')
    else
      thead = $('#sessionHeaders').children("tr")
      $(data.headers).each (idx, element) ->
        thead.append("<th title='#{element[1]}'>#{element[0]}</th>")
      tbody = $('#sessionTable')
      for session in data.report
        row = $('<tr>')
        for field in session
          row.append("<td>#{field}</td>")
        tbody.append(row)
      $('.timestamp').html("Report generated #{data.timestamp}")
