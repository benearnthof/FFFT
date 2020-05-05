allinputs <- reactive({
  x <- reactiveValuesToList(input)
  data.frame(names = names(x), values = unlist(x, use.names = FALSE))
})

output$show_inputs <- renderTable({
  allinputs()
})

matchedinputs <- reactive({
  x <- reactiveValuesToList(input)
  table <- data.frame()
  for (j in seq_len(input$number_weibulls)) {
    for (i in 1:3) {
      table[j, i] <- paste0("forecastformal", i, j)
    }
  }
  i <- match(names(x), unlist(table, use.names = FALSE), nomatch = 0L)
  ret <- x[i > 0]
  data.frame(names = names(ret), values = unlist(ret, use.names = FALSE))
})

output$show_matched <- renderTable({
  matchedinputs()
})
