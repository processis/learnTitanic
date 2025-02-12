library(nnet)

ui = navbarPage(tabPanel("Regression Analysis",
                         dataTableOutput('mytable'),
                         sidebarLayout(
                           sidebarPanel(width=3, fileInput("file1", "Please choose a CSV file",
                                                           multiple = T,
                                                           accept = c("text/csv",
                                                                      "text/comma-separated-values,text/plain",
                                                                      ".csv")),
                                        tags$hr(),
                                        checkboxInput("header", "Header", TRUE),
                                        radioButtons("sep", "Separator",
                                                     choices = c(Comma = ",",
                                                                 Semicolon = ";",
                                                                 Tab = "\t"),
                                                     selected = ","),
                                        radioButtons("quote", "Quote",
                                                     choices = c(None = "",
                                                                 "Double Quote" = '"',
                                                                 "Single Quote" = "'"),
                                                     selected = '"'),
                                        tags$hr(),
                                        radioButtons("disp", "Display",
                                                     choices = c(Head = "head",
                                                                 All = "all"),
                                                     selected = "head")
                                        
                           ),
                           
                           mainPanel(
                             tableOutput("contents"),
                             actionButton("choice", "Define Regression Variables"),
                             selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                             uiOutput("dependent1"),
                             #tableOutput("Table_selected.col"),
                             verbatimTextOutput("regTab")
                           )
                         ),
                         
))

server = function(input, output, session) {
  
  mydf <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df = read.csv(input$file1$datapath,
                  header = input$header,
                  sep = input$sep,
                  quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$contents = renderTable({
    req(mydf())
    mydf()
  })
  
  # Code for allowing the user to select the variables/columns of interest
  info <- eventReactive(input$choice, {
    req(mydf())
    f <- mydf()
    f
  })
  
  observeEvent(input$choice, {  ## to update only when you click on the actionButton
    req(info())
    updateSelectInput(session,"independent", "Please Select independent Variable(s):", choices = names(info()) )
  })
  
  
  # output$Table_selected.col <- renderTable({
  #   input$choice
  #   req(info(),input$columns)
  #   f = info()
  #   f = subset(f, select = input$columns) #subsetting takes place here
  #   head(f)
  # })
  
  output$dependent1 = renderUI({
    req(mydf(),input$independent)
    radioButtons("dependent1", "Select a dependent Variable:",choices=names(mydf())[!names(mydf()) %in% input$independent])
  })
  
  ###  need to build your formuila correctly; It will work with multiple independent variables
  ###  model <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = RegData)})
  
  runRegression <- reactive({
    req(mydf(),input$independent,input$dependent1)
    lm(reformulate(input$independent, input$dependent1), data=mydf())
    # multinom(reformulate(input$independent, input$dependent1), data=mydf())  ### mulitnomial from nnet package
  })
  
  output$regTab = renderPrint({
    req(runRegression())
    if(!is.null(input$independent)){
      summary(runRegression())
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  })
  
}

shinyApp(ui, server)