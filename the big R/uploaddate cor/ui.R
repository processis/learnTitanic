# Define UI for application that draws a histogram
ui <- fillPage(
  theme = "yeti",
  tags$title("online volcano"),

  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # uploading file
      div(
        fileInput("csvFile", "Choose defferential analysis result File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        )
      ),
      
     
      # select gene 
      
     
         # select fold change and pvalue 
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("col output",
                 plotOutput("colImage") 
        ),
        tabPanel("data table",
                 dataTableOutput("inputdata"))
      )
      
    )
  )
)
