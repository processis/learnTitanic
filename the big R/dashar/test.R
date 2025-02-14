# 加载必要的库
library(shiny)
library(dplyr)
library(ggplot2)
library(rmarkdown)

# 定义UI
ui <- fluidPage(
  titlePanel("数据分析与报告生成"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上传数据表 (CSV文件)", accept = c(".csv")),
      actionButton("analyze", "分析数据"),
      downloadButton("downloadReport", "下载分析报告")
    ),
    
    mainPanel(
      tableOutput("dataPreview"),
      plotOutput("dataPlot")
    )
  )
)

# 定义服务器逻辑
server <- function(input, output) {
  
  # 读取上传的数据
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # 预览数据
  output$dataPreview <- renderTable({
    head(data())
  })
  
  # 数据分析并生成图表
  output$dataPlot <- renderPlot({
    req(input$analyze)
    ggplot(data(), aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point() +
      ggtitle("Sepal Length vs Sepal Width")
  })
  
  # 生成并下载分析报告
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("analysis-report-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # 创建一个临时的R Markdown文件
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # 设置参数传递给R Markdown
      params <- list(data = data())
      
      # 渲染报告
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

# 运行Shiny应用
shinyApp(ui = ui, server = server)