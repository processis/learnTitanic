server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  #req(input$file1)
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    

    
    
    
    
    
    
   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    #xia la kuang xuan xiang
    #yu UI dui ying
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    
    updateSelectInput(session, inputId = 'xcol2', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol2', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    
    updateSelectInput(session, inputId = 'xcol3', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol3', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    updateSelectInput(session, inputId = 'ycol4', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    
    
    
    
    return(df)
  })
  
  output$contents <- renderTable({
    data()
    
     })
  
  output$summar <- renderPrint({
    req(input$file1)
    summary(data())
  })
  
  
  
  
  
  regFormula <- reactive({
    as.formula(paste('mpg ~', input$x))
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$MyPlot <- renderPlot({
    # for a histogram: remove the second variable (it has to be numeric as well):
    # x    <- data()[, c(input$xcol, input$ycol)]
    # bins <- nrow(data())
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # Correct way:
    # x    <- data()[, input$xcol]
    # bins <- nrow(data())
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    
    # I Since you have two inputs I decided to make a scatterplot
    x <- data()[, c(input$xcol, input$ycol)]
    plot(x)
    
  })
  
  
  
  
  output$Myhistogram <- renderPlot({
    # for a histogram: remove the second variable (it has to be numeric as well):
    #x    <- data()[, c(input$xcol, input$ycol)]
    #bins <- nrow(data())
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # Correct way:
    x    <- data()[, input$xcol]
    #y    <- data()[, input$ycol]
    bins <- nrow(data())
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #hist(y, breaks = bins, col = 'darkgray', border = 'white')
    
    
    # I Since you have two inputs I decided to make a scatterplot
    #x <- data()[, c(input$xcol, input$ycol)]
    #plot(x)
    
  })
  
  
  
  
  
  output$Mycol <- renderPlot({
    # for a histogram: remove the second variable (it has to be numeric as well):
    #x    <- data()[, c(input$xcol, input$ycol)]
    #bins <- nrow(data())
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # Correct way:
    #x    <- data()[, input$xcol]
    #bins <- nrow(data())
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    
    # I Since you have two inputs I decided to make a scatterplot
    x <- data()[, c(input$xcol, input$ycol)]
    plot(x)
    
  })
  
  
  
  output$Mycol <-renderTable(
    {
      #x <- data()[, c(input$xcol, input$ycol)]
      x    <- data()[, input$xcol]
      y<-data()[, input$ycol]
      cor(x,y)
      
      #model<-lm(y~x)
      #summary(model)
    }
  )
  
  
  
  
  output$Mylm<-renderPrint(
    {
      x    <- data()[, input$xcol]
      y<-data()[, input$ycol]
      
      
      model<-lm(y~x)
      summary(model())
    }
  )
  
 
  
  
  
  
  
   
  
  
  
  
  
  
  
  
  
  
  
  
})
