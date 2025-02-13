library(shiny)
library(datasets)

ui <- shinyUI(fluidPage(
  titlePanel("Column Plot"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               
               
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
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
                 
                 
                 radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                              inline = TRUE),
                 downloadButton('downloadReport')
                 
                 
                 
                 
                 
               ),
               mainPanel(
                 verbatimTextOutput("summar"),
                 tableOutput('contents')
                 
                 
               )
             )
    ),
    
    
    
    
    tabPanel("First Type",
             pageWithSidebar(
               headerPanel('My First Plot'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")
                 
               ),
               mainPanel(
                 plotOutput('MyPlot')
               )
             )
    ),
    
    
    
    tabPanel("second Type",
             pageWithSidebar(
               headerPanel('My histogram'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol2', 'X Variable', ""),
                 #selectInput('ycol2', 'Y Variable', "", selected = "")
                 
                 #numericInput("n","NUmber of observations:",value=100),
                 #submitButton("Update Plot")
                 
                 
                 #fileInput("file", "上传CSV文件", accept = c(".csv")),
                 uiOutput("column_selector")
                 
                 
                 
                 
                 
               ),
               mainPanel(
                 plotOutput('Myhistogram')
               )
             )
    ),
    
    
    
    tabPanel("third Type",
             pageWithSidebar(
               headerPanel('My col'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol3', 'X Variable', ""),
                 selectInput('ycol3', 'Y Variable', "", selected = "")
                 
               ),
               mainPanel(
                 tableOutput('Mycol')
               )
             )
    ),
    
    
    
    
    
    
    
    
    
    tabPanel("fifth Type",
             pageWithSidebar(
               headerPanel('My lm1'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol5', 'X Variable', ""),
                 selectInput('ycol5', 'Y Variable', "", selected = "")
                 
               ),
               mainPanel(
                 actionButton("choice", "Define Regression Variables"),
                 selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 uiOutput("dependent1"),
                 #tableOutput("Table_selected.col"),
                 verbatimTextOutput("regTab")
               )
             )
    ),
    
    
    
    
    
    
    
    tabPanel("sixth Type",
             pageWithSidebar(
               headerPanel('My dd'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol6', 'X Variable', ""),
                 selectInput('ycol6', 'Y Variable', "", selected = ""),
                 selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
                 
               ),
               mainPanel(
                 actionButton("choice", "incorporate external information"),
                 
                 selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
                 
                 tableOutput("table_display")
               )
             )
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  )
)
)