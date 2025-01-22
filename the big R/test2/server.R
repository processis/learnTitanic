server <- function(input, output, session) {
  onSessionEnded(stopApp)
  data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote)
    return(df)
  })
  
  output$contents <- renderTable({
    if (input$disp == "head") {
      return(head(data()))
    }
    else {
      return(data())
    }
  })
  output$summar <- renderPrint({
    req(input$file1)
    summary(data())
  }
  )
  
  output$variable_x <- renderUI({
    selectInput("variableNames_x", label = "Variable_X", choices = names(data()))  
  })
  output$variable_y <- renderUI({
    selectInput("variableNames_y", label = "Variable_Y", choices = names(data()) ) 
  })
  dat <- reactive({
    test <- data.frame(data()[[input$variableNames_x]], data()[[input$variableNames_y]])
    colnames(test) <- c("X", "Y")
    return(test)
  })
  
  output$plot <- renderPlot({
    if (is.null(data)) { return(NULL)
    } else {
      ggplot(dat(),aes(x = X,y = Y)) + geom_point(colour = 'red') +
        labs(y = input$variableNames_y,
             x = input$variableNames_x,
             title = "ggplot")
    }
  })
}