# Define server logic required to draw a histogram
server <- function(input, output) {
  
  inputdf <- reactive({
    inFile <- input$csvFile
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath)
    pos <- which(colnames(df) %in% c(month,heigth))
    colnames(df)[pos] <- c("geneID","log2FoldChange","pvalue")
    df
  })
  
  
  output$inputdata <- renderDataTable({
    
    inputdf()
    
  })
  
  
  output$scatter <- renderPlot({
    p <- gplot(df, aes(x=months, y=height,color="#5e616d"))+      
      geom_point()                                        #绘画散点图                        
      
    })
  
 p# 返回生成的图形
}




