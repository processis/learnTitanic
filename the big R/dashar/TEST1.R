# 定义UI
ui <- fluidPage(
  titlePanel("相关性分析与热力图"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上传CSV文件", accept = c(".csv")),
      uiOutput("variable_select"),  # 动态生成变量选择器
      actionButton("analyze", "进行相关性分析")
    ),
    
    mainPanel(
      h4("相关性矩阵"),
      tableOutput("correlation_matrix"),  # 显示相关性矩阵
      h4("热力图"),
      plotOutput("heatmap")  # 显示热力图
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  
  # 读取上传的数据
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # 动态生成变量选择器
  output$variable_select <- renderUI({
    req(data())
    numeric_vars <- names(data())[sapply(data(), is.numeric)]  # 仅选择数值型变量
    checkboxGroupInput("selected_vars", "选择变量进行相关性分析", choices = numeric_vars)
  })
  
  # 计算相关性矩阵
  correlation_matrix <- reactive({
    req(input$selected_vars)
    selected_data <- data()[, input$selected_vars, drop = FALSE]
    cor(selected_data, use = "complete.obs")  # 计算相关性矩阵
  })
  
  # 显示相关性矩阵
  output$correlation_matrix <- renderTable({
    req(correlation_matrix())
    correlation_matrix()
  }, rownames = TRUE)
  
  # 绘制热力图
  output$heatmap <- renderPlot({
    req(correlation_matrix())
    melted_cormat <- melt(correlation_matrix())  # 将相关性矩阵转换为长格式
    ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "", fill = "Correlation")
  })
}

# 运行Shiny应用程序
shinyApp(ui = ui, server = server)