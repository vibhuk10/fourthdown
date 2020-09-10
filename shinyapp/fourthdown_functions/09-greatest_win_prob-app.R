greatest_win_prob <- function(table_input) {
  num <- which.max(table_input$win_prob)
  name <- test[num,6]
  name
}

