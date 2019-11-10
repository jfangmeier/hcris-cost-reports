library(DT)
datatable(
  cbind(' ' = '&oplus;', costreport_df %>% filter(Year == 2016)), escape = -2,
  options = list(
    columnDefs = list(
      list(visible = FALSE, targets = c(0, 2, 3)),
      list(orderable = FALSE, className = 'details-control', targets = 1)
    )
  ),
  callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    return '<div style=\"background-color:#eee; padding: .5em;\"> Model: ' +
            d[0] + ', mpg: ' + d[2] + ', cyl: ' + d[3] + '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&CircleMinus;');
    }
  });"
  ))






output$pivot <- renderDocument({
  costreport_df_pivot <- costreport_df_react() %>% 
    select(-Long, -Lat)
  
  costreport_df_pivot %>% 
    tbl_df() %>%
    rpivotTable(rows = "Year", cols="Teaching Status", vals = "Freq", aggregatorName = "Sum", rendererName = "Table", width="100%", height="400px")
})