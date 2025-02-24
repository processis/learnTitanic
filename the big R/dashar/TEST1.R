# 定义UI
ui <- fluidPage(
  titlePanel("上传数据并绘制直方图"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上传CSV文件", accept = c(".csv")),
      selectInput("column", "选择列", choices = NULL),
      sliderInput("binwidth", "直方图宽度", min = 0.1, max = 10, value = 1)
    ),
    
    mainPanel(
      plotOutput("histogram")
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  
  # 读取上传的文件
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # 更新列选择器
  observeEvent(data(), {
    updateSelectInput(session, "column", choices = names(data()))
  })
  
  # 绘制直方图
  output$histogram <- renderPlot({
    req(input$column)
    ggplot(data(), aes_string(x = input$column)) +
      geom_histogram(binwidth = input$binwidth, fill = "blue", color = "black") +
      labs(title = paste("Histogram of", input$column),
           x = input$column,
           y = "Count")
  })
}

# 运行Shiny应用程序
shinyApp(ui = ui, server = server)