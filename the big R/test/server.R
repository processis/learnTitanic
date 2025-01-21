server <- function(input, output) {
  
  d1 <- reactive({
    inFile1 <- input$dat
    if (is.null(inFile1)) return(NULL)
    fread(inFile1$datapath)
  })
  
  
  output$head <- renderTable({
    dat= d1()
    head(dat)
  })
  
  output$summary <- renderPrint({
    dat= d1()
    summary(dat)
  })
  
  output$col <- renderPrint({
    dat= d1()
    lm(dat)
  })
  
  output$col1 <- renderPlot({
    dat= d1()
    plot(month,height)
  })
  
}