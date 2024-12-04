server <- function(input, output, session) {
  output$files <- renderTable(input$upload)
}

shinyApp(ui, server)